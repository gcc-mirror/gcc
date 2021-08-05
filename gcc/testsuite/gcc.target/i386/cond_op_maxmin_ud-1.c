/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -DTYPE=uint32 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_MAX" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MIN" "optimized" } } */
/* { dg-final { scan-assembler-times "vpmaxud"  1 } } */
/* { dg-final { scan-assembler-times "vpminud"  1 } } */

#include "cond_op_maxmin_d-1.c"
