/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -ftrapv -fdump-tree-optimized" } */

#include <arm_neon.h>

int
f (int32x4_t a, int32x4_t b)
{
  return vaddvq_s32 (a) + vaddvq_s32 (b);
}

/* Trapping scalar addition must remain outside the reductions.  */
/* { dg-final { scan-tree-dump-times "REDUC_PLUS" 2 "optimized" } } */
