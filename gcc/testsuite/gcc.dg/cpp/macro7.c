/* { dg-do preprocess } */

/* Test we don't complain about directives in macro expansions when
   looking for the '(' of a function-like macro.

   Submitter: Neil Booth. 3 Mar 2000.  */

#define f(x) x
f
#define g
