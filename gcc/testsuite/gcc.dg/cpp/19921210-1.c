/* Test for proper disabling of macros within their own expansions.  */
/* { dg-do compile } */

/* The following is a trick to evaluate a complex boolean expression
   at compile time, inspired by autoconf 2.13's sizeof-detection.  */

enum { a = 4, f = 3 };

#define a1(y) (y+2)
#define a2(y) a1(y)+1
#define f a+f

char array[(a2(f)) == 10 ? 1 : -1];
