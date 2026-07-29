/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* Two reductions feeding the matching scalar operation are one reduction of
   the elementwise operation, which trades a lane-crossing reduction for an
   elementwise one.  */
#include <arm_neon.h>
int f (int32x4_t a, int32x4_t b) { return vaddvq_s32 (a) + vaddvq_s32 (b); }
int g (int32x4_t a, int32x4_t b) { return vaddvq_s32 (a) - vaddvq_s32 (b); }
int h (int32x4_t a, int32x4_t b) { int x = vmaxvq_s32 (a), y = vmaxvq_s32 (b); return x > y ? x : y; }
/* { dg-final { scan-tree-dump-times "REDUC_PLUS" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "REDUC_MAX" 1 "optimized" } } */
