/* Test that alpha-base-1.c compiles with optimization.  */
/* { dg-do link { target alpha*-*-* } } */
/* { dg-options "-mcpu=ev4 -O2" } */

#include "base-1.c"
