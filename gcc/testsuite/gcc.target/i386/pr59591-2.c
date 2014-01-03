/* PR tree-optimization/59591 */
/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -mavx2 -fno-vect-cost-model" } */
/* { dg-require-effective-target avx2 } */

#define CHECK_H "avx2-check.h"
#define TEST avx2_test

#include "../../gcc.dg/vect/pr59591-2.c"

#include CHECK_H

static void
TEST (void)
{
  bar ();
}
