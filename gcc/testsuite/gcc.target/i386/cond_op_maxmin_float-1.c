/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -DTYPE=float -fdump-tree-optimized -DFN_MAX=fmaxf -DFN_MIN=fminf" } */
/* { dg-final { scan-tree-dump ".COND_MAX" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MIN" "optimized" } } */
/* { dg-final { scan-assembler-times "vmaxps"  1 } } */
/* { dg-final { scan-assembler-times "vminps"  1 } } */

#include "cond_op_maxmin_double-1.c"
