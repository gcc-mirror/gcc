/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-tree-optimized" } */

/* The signed vcnt and vclz intrinsics count an unsigned vector.  */

#include <arm_neon.h>

int8x8_t f1 (int8x8_t a) { return vcnt_s8 (a); }
int8x16_t f2 (int8x16_t a) { return vcntq_s8 (a); }
int8x8_t f3 (int8x8_t a) { return vclz_s8 (a); }
int32x4_t f4 (int32x4_t a) { return vclzq_s32 (a); }

/* { dg-final { scan-tree-dump-times {\.POPCOUNT \(_} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.CLZ \(_} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not {\.(POPCOUNT|CLZ) \(a_} "optimized" } } */
