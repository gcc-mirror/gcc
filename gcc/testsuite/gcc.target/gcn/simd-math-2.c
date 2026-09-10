/* Check that the SIMD versions of math routines give the same (or
   sufficiently close) results as their scalar equivalents.  */

/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fno-math-errno" } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_double } */
/* { dg-set-target-env-var "GCN_STACK_SIZE" "3000000" } */

#include "simd-math-1.c"
