/* Homework Assignment 5
   Programming Languages
   CS471, Spring 20
   Binghamton University */

/* Instructions */

/* This section deals with general submission instructions. 
   First, grab this assignment's  source file. BEFORE MOVING ON RENAME 
   hw5S20 to hw5.pl. You will be able to code in and run the file 
   in the prolog interpreter directly. I recommend reading this assignment 
   directly from the source file.

We will be using swipl for our prolog environment: 
To load/reload this file, cd to its directory and run swipl. 
Then, in the prompt, type [hw5].

     cd PATH_TO_FILE
     swipl
     [hw5].

From then on you may execute queries (goals) in the prompt. As usual, you should provide your answers in the designated spot. Once you have added some code to the file, rerun [hw5]. in the swipl prompt to reload.

In addition, there are unit tests for each problem. These are there to help you better understand what the question asks for, as well as check your code. They are included in our "database" as queries and are initially commented out -- % is a Prolog line comment.

This time all test cases should give warnings if the test cases fail, that is, we only have test cases like
%:- member_times(4,[3,3,2,3],0).
for success cases and
% :- result([+(3,7), mod(104,7)],[10,13]) -> fail ; true.
for failure cases.

After you have finished a problem and are ready to test, remove the initial % for each test for the associated problem and reload the assignment file ([hw5].). Any test that gives a warning indicates a problem. If you pass the tests there is a good chance that your code is correct, but not guaranteed; the tests are meant as guided feedback and are not a check for 100% correctness. */

/* Submission */

/* Please store hw5.pl within a tarball named username_hw5.tar.gz  */

/* Homework 5 */

/* Due: Next Tues, 11:59 PM */

/* Purpose: To get comfortable with unification and pattern matching. */



/* Problem 1:
In the last homework we can encode a binary search tree in Prolog using complex terms: i.e, the following BST

        5
       / \
      3   7
     / \
    1   4

can be encoded as node(5,node(3,node(1,leaf,leaf),node(4,leaf,leaf)),node(7,leaf,leaf)).

Write a predicate insert(X,Y,Z) that succeeds if Z is the tree Y with X inserted (insert X into Y). You may assume you have a binary search tree. */

/* Problem 1 Answer: */

insert(X,leaf,node(X, leaf, leaf)).
insert(X, node(V, Yl, R), node(V, Zl, R)):-
	X=<V,
	insert(X, Yl, Zl).
insert(X, node(V, L, Yr), node(V, L, Zr)):-
	X>V,
	insert(X, Yr, Zr).
% completely misinterpreted this at first to think that Z was the BST Y with the BST X inserted into it, got really confused

/* Problem 1 Test: */

:- insert(3,node(5,leaf,leaf),X), X = node(5,node(3,leaf,leaf),leaf).
:- insert(7,node(5,leaf,leaf),X), X = node(5,leaf,node(7,leaf,leaf)).
:- insert(1,node(5,node(3,leaf,leaf),node(7,leaf,leaf)),X), X = node(5,node(3,node(1,leaf,leaf),leaf),node(7,leaf,leaf)).
:- insert(1,node(5,node(3,node(2,leaf,leaf),leaf),node(7,leaf,leaf)),X), X = node(5,node(3,node(2,node(1,leaf,leaf),leaf),leaf),node(7,leaf,leaf)).

:- (insert(3,node(5,node(3,node(2,leaf,leaf),leaf),node(7,leaf,leaf)),X), X = node(5,node(3,node(2,node(3,leaf,leaf),leaf)),node(7,leaf,leaf))) -> fail ; true.


/* Problem 2:
Using the same encoding for a binary search tree, write a predicate to_list(X,Y) that succeeds if Y is an in-order list of the elements of all the nodes of tree X (Y should show an in order traversal of X). If you are rusty and do not remember what an in order traversal is, give https://en.wikipedia.org/wiki/Tree_traversal#In-order a quick glance.

For example...
to_list(node(5,node(3,node(1,leaf,leaf),node(4,leaf,leaf)),node(7,leaf,leaf)),X) will succeed with X = [1,3,4,5,7]. */

/* Problem 2 Answer:  */

append([],Y,Y).
append([H|Xt], Y, [H|Zt]):-
	append(Xt, Y, Zt).

to_list(leaf, []).
to_list(node(Xv, Xl, Xr),Y):-
	to_list(Xl, Yl),
	to_list(Xr, Yr),
	append(Yl, [Xv|Yr], Y).

/* Problem 2 Tests:  */
:- to_list(node(3,leaf,leaf),L), L = [3].
:- to_list(node(5,node(3,leaf,leaf),leaf),L), L = [3,5].
:- to_list(node(5,node(3,node(1,leaf,leaf),node(4,leaf,leaf)),node(7,leaf,leaf)),L), L = [1,3,4,5,7].

:- (to_list(node(3,leaf,leaf),L), L = [5]) -> fail ; true.


/* Problem 3:
Write a predicate right_rotate(X,Y) that succeeds if Y is the tree X rotated right at its root. Read https://en.wikipedia.org/wiki/Tree_rotation to refresh yourself on what it means for a tree to be rotated. This problem may seem hard at first, but once you "see" the answer it really demonstrates the elegance of unification/pattern matching. You do not need to handle error cases.

For example, the following shows a right rotation at the root.

        5                        3
       / \                      / \
      3   7         -->        2   5
     / \                          / \
    2   4                        4   7

*/

/* Problem 3 Answer: */

right_rotate(leaf, leaf).
%right_rotate(node(V, leaf, R), node(V, leaf, R)).
right_rotate(node(Xv, node(Yv, Yl, Xlr), Xr), node(Yv, Yl, node(Xv, Xlr, Xr))).
	

/* Problem 3 Test: */
:- right_rotate(node(5,node(3,node(2,leaf,leaf),node(4,leaf,leaf)),node(7,leaf,leaf)),X), X = node(3, node(2, leaf, leaf), node(5, node(4, leaf, leaf), node(7, leaf, leaf))). %SUCCEED
:- right_rotate(node(5,node(3,leaf,node(4,leaf,leaf)),node(7,leaf,leaf)),X), X = node(3, leaf, node(5, node(4, leaf, leaf), node(7, leaf, leaf))). %SUCCEED
:- right_rotate(node(3,node(2,node(1,leaf,leaf),leaf),leaf),X), right_rotate(X,Y), Y = node(1,leaf,node(2,leaf,node(3,leaf,leaf))). %SUCCEED

:- right_rotate(node(5,leaf,node(7,leaf,leaf)),_) -> fail ; true. %FAIL


/* Problem 4:

 In the previous assignment, you wrote Prolog rules for symbolic differentiation.
 Below is my solution for this problem.
 Keep in mind that terms such as U+V are still trees with the functor
 at the root, and that evaluation of such terms requires additional processing
 which you will complete.

 Define the predicate, evaluate/3, that uses the result from symbolic
 differentiation and a list of items with the structure var:value (e.g. [a:5,x:6])
 and computes the resulting value, e.g.

    ?- d(3*(x +2*x*x),x,Result), VarValue = [x:2,y:5], evaluate(Result,Value,VarValue).
    Result = 3* (1+ (2*x*1+x*2))+ (x+2*x*x)*0,
    VarValue = [x:2, y:5],
    Value = 27


    ?- d((3*x) ^ 4,x,Result), VarValue = [x:2,y:5] , evaluate(Result,Value,VarValue).
    Result = 4* (3*x)^3*3,
    VarValue = [x:2, y:5],
    Value = 2592.


 */

/* Problem 4 Answer:  */

d(X,X,1):-atom(X).
d(C,X,0):-atom(X), number(C).
d(C*X,X,C):-atom(X), number(C).
d(-U, X, -DU) :- atom(X), d(U, X, DU).
d( U + V, X, RU + RV ):-atom(X), d(U,X,RU), d(V,X,RV).
d( U - V, X, RU - RV ):-atom(X), d(U,X,RU), d(V,X,RV).
d(U * V, X, U * DV + V * DU):- atom(X), d(U,X,DU), d(V,X,DV).
d(U ^ N, X, N*U ^ N1*DU) :- atom(X), integer(N), N1 is N-1, d(U, X, DU).

evaluate(N,N,_):-
	number(N).
evaluate(X,N,[X:N|_]):-
	atom(X),
	number(N).
evaluate(A, S, [X:N|T]):-
	atom(A),
	atom(X),
	number(N),
	evaluate(A, S, T).
evaluate(A^B,S,L):-
	evaluate(A, Sa, L),
	evaluate(B, Sb, L),
	S is Sa^Sb.
evaluate(A*B,S,L):-
	evaluate(A, Sa, L),
	evaluate(B, Sb, L),
	S is Sa*Sb.
evaluate(A+B,S,L):-
	evaluate(A, Sa, L),
	evaluate(B, Sb, L),
	S is Sa+Sb.
evaluate(A-B,S,L):-
	evaluate(A, Sa, L),
	evaluate(B, Sb, L),
	S is Sa-Sb.

/* Problem 4 Tests:  */
:- evaluate(x*y, 6, [x:2, y:3]).
:- evaluate(x^3, 8, [x:2]).
:- evaluate(2*8, 16, []).
:- evaluate(2*y, 16, [y:8]).

:- evaluate(x*y, 8, [x:2, y:3]) -> fail ; true.
:- evaluate(2*8, 0, []) -> fail ; true.

:- d(3*(x +2*x*x),x,Result), VarValue = [x:2,y:5], evaluate(Result,Value,VarValue),
    Result = 3* (1+ (2*x*1+x*2))+ (x+2*x*x)*0,
    VarValue = [x:2, y:5],
    Value = 27.

:- d((3*x) ^ 4,x,Result), VarValue = [x:2,y:5] , evaluate(Result,Value,VarValue),
    Result = 4* (3*x)^3*3,
    VarValue = [x:2, y:5],
    Value = 2592.


/* Problem 5:
We will encode a mini-AST in Prolog using complex data structures. A "node" will consist of either a nb(Functor,LeftExpr,RightExpr), nu(Functor,Expr) or nn(Number).

nb(Functor,LeftExpr,RightExpr) -- "node binary", Functor is guaranteed to be a binary arithmetic predicate that can be evaluated with `is`. LeftExpr and RightExpr are recursively defined "nodes".

nu(Functor,Expr) -- "node unary", Functor is guaranteed to be a unary arithmetic predicate that can be evaluated with `is`. Expr is a recursively defined "node".

nn(Number) -- "node number", Number is guaranteed to be just that.

Hence, the following AST
      +
     / \
    *   random
   / \       \
  2  3        5
would be encoded as nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))).

Write a predicate run(X,Y) that succeeds if Y is the result obtained from "running" (evaluating) X. You will need to use the =.. predicate. It may be helped to view some of the binary and unary arithmetic predicates -- http://www.swi-prolog.org/man/Ardith.html. If written correctly, your solution should not need to individually handle each possible arithmetic predicate. */

/* Problem 5 Answer: */

run(nn(N), N):-number(N).
run(nu(F, X), Y):-
	functor(_,F,1),
	run(X, Yx),
	Yp =.. [F,Yx],
	Y is Yp.
run(nb(F,Xl,Xr), Y):-
	functor(_,F,2),
	run(Xl, Yl),
	run(Xr, Yr),
	Yp =.. [F,Yl,Yr],
	Y is Yp.

/* Problem 5 Tests: */
:- run(nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))),_).
:- run(nb(+,nb(*,nn(2),nn(3)),nn(3)),E), E=9.
:- run(nb(+,nb(*,nn(2),nn(3)),nb(-,nn(6),nn(3))),E), E=9.
:- run(nn(2),E), E=2.
:- run(nu(abs,nn(-2)),E), E=2.

:- (run(nb(+,nb(*,nn(2),nn(3)),nb(-,nn(6),nn(3))),E), E=8) -> fail ; true.


/* Problem 6:
Using the AST described in problem 5, write a predicate binaryAP/2.  binaryAP(AST, BPlst) succeeds if all the binary arithmetic predicates that occur in AST are collected into BPlst.  Use an in order traversal of AST.  */

/* Problem 6 Answer: */

binaryAP(nn(_), []).
binaryAP(nu(F,X), B):-
	functor(_,F,1),
	binaryAP(X,B).
binaryAP(nb(F,Xl,Xr),BPlst):-
	functor(_,F,2),
	binaryAP(Xl, Bl),
	binaryAP(Xr, Br),
	append(Bl,[F|Br],BPlst).

/* Problem 6 Tests: */
:- T = nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))), binaryAP(T,L), L = [*, +].
:- T = nb(+, nb(*, nn(2), nn(3)), nb(-,nn(3), nn(5))),  binaryAP(T,L), L = [*, +, -].
:- T = nb(+, nb(*, nn(2),  nb(-,nn(3), nb(//, nn(2), nn(5)))),nn(9)) ,  binaryAP(T,L), L = [*, -, //, +].

:- (T = nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))), binaryAP(T,L), L = [+,*]) -> fail ; true.



/* Problem 7:
   Write a predicate unNest/2.  unNest(+Nested, -LstAtoms), where Nested is an arbitrarily nested
   list of atoms and LstAtoms is a list of atoms (i.e. no items in LstAtoms is a list), 
   succeeds if all the atoms from Nested are in LstAtoms and in the same order.
   For example, unNest([a,[b,[c]],d],U) succeeds if U = [a, b, c, d] .

   Think What NOT how. This can be done with only 3 clauses */

/* Problem 7 Answer: */

unNest([],[]).
unNest(N,[N]):-
	atom(N).
unNest([Nh|Nt],L):-
	unNest(Nh,Lh),
	unNest(Nt,Lt),
	append(Lh,Lt,L).

/* Problem 7 Tests: */

:- unNest([[r,s,[x,y]],[a],[],[s,t]],One), One = [r, s, x, y, a, s, t].
:- unNest([[[r]],[]],[r]).
:- unNest([[r,s,[x,y]],[a],[],[s,t]],One), One = [r, s, x, y, a, t] -> fail ; true. 


/* Problem 8:

In class we discussed difference lists and how to append two of them in "constant" time.

Write a predicate, append3DL(A,B,C,D) that succeeds if D is the difference lists A, B, and C appended.
*/

/* Problem 8 Answer: */

append3DL(X-B, Y-C, Z-D, S-D):-
	append(_,B,X),
	append(B,C,Y),
	append(C,D,Z),
	append(X,Z,S).

/* Problem 8 Tests: */
:- append3DL([1,2|A]-A,[3,4|B]-B,[5,6|[]]-[],L), L = [1,2,3,4,5,6]-[].
:- append3DL([a,b|A]-A,[b,1,2|B]-B,[3|C]-C,L), L = [a, b, b, 1, 2, 3|C]-C.

%:- (append3DL([1,2|A]-A,[3,4|B]-B,[5,6|[]]-[],L), L = [1,2,3,4,5]-[]) -> fail ; true.



/* Problem 9:
    In assignment 2 problem 13, you were given the following definition of Ackermann's function.
	ack( m,n ) =	n + 1                          if m = 0
	ack( m,n ) =	ack(m - 1, 1)                  if n = 0 and m > 0 
	ack( m,n ) =	ack( m-1, ack( m, n-1 ) )      if n >0 and m > 0

   Convert this definition to a Prolog predicate ack/ 3, ack(M,N,R) where R should unify with the
   result of ack(M,N) as indicated by the above definition.
   In addition provide 3 test cases.  In at least one test case, m should have a value greater than 0.
*/

/* 
Problem 9 Answer 
*/

ack(0,N,R):-R is N+1.
ack(M,0,R):-
	M>0,
	Mn is M-1,
	ack(Mn,1,R).
ack(M,N,R):-
	M>0,
	N>0,
	Mn is m-1,
	Nn is n-1,
	ack(M,Nn,Rn),
	ack(Mn,Rn,R).

/* Problem 9 Test */

% You're own your own for this one :)

:- ack(0,0,R), R=1.
:- ack(1,0,R), R=2.
:- ack(1,1,R), R=4.

/* Problem 10:

Write a predicate change/2 that given the change amount, computes the way in which exact change can be given.
Your solution should make use of the following facts that describe US coins. */

coin(dollar, 100).
coin(half, 50).
coin(quarter, 25).
coin(dime,10).
coin(nickel,5).
coin(penny,1).

/* The predicate change(S,CL) succeeds if given a positive integer S, CL is a list of tuples that contains the name of the coin and the number of coins needed to return the correct change.

The Coin Changing problem is an interesting problem usually studied in Algorithms.
http://condor.depaul.edu/~rjohnson/algorithm/coins.pdf is a nice discussion.
Think about (no need to turn in)
   1) How could we generalize this problem to handle coins from other currencies?
   2) What are the different techniques to find the change with the fewest number of coins ?
   3) What happens if the order of the "coin" facts change?  
   */

/* Problem 10 Answer: */

/* Problem 10 Tests: */
%:- change(168,C), C = [ (dollar, 1), (half, 1), (dime, 1), (nickel, 1), (penny, 3)] .
%:- change(75,C),  C = [ (half, 1), (quarter, 1)] .

%:- (change(75,C), C = [(half, 2)]) -> fail ; true.

/* Problem 11
   Define a predicate filter(+Goal, +Lst, ?Result), such that Result contains all the elements of Lst 
   for which the Goal succeeds. In other words, Goal is a boolean function that can operate on
   elements of Lst. Remove the elements for which Goal returns false.
   We have defined a predicate even/1 for use in tests.
 */
 
even(X) :- 0 is X mod 2.

/* Problem 11 Answer: */

/* Problem 11 Tests: */

%:- filter(even, [1, 2, 3, 4], X), X=[2,4].
%:- (filter(even, [1, 2, 3, 4], [2,4,6]))-> fail ; true.
%:- (filter(even, [1, 2, 3, 4], X), X=[1,3])-> fail ; true.
