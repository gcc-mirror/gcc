/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -DTYPE=int64 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_AND" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_XOR" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_IOR" "optimized" } } */
/* { dg-final { scan-assembler-times "vpxorq"  1 } } */
/* { dg-final { scan-assembler-times "vporq"  1 } } */
/* { dg-final { scan-assembler-times "vpandq"  1 } } */

#include "cond_op_anylogic_d-1.c"
