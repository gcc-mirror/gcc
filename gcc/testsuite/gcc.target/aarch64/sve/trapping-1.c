/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim-details" } */

/* PR tree-optimization/117234  */
/* VEC_DUPLICATE_EXPR should not be declared as
   a trapping.  */
#include <arm_sve.h>

svfloat32_t f(float a, int t, _Bool *b)
{
  svfloat32_t tt = svdup_f32(0.0);
  for (int i =0 ;i < t; i++)
  {
    if (b[i])
      tt = svadd_f32_z(svptrue_b32(),tt,svdup_f32(a));
  }
  return tt;
}

/* There should be 1 `invariant up to level`, one for the VEC_DUPLICATE_EXPR. */
/* { dg-final { scan-tree-dump-times "invariant up to level" 1 "lim2" } } */
