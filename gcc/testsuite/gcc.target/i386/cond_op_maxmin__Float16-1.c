/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids -DTYPE=_Float16 -fdump-tree-optimized -DFN_MAX=__builtin_fmaxf16 -DFN_MIN=__builtin_fminf16" } */
/* { dg-final { scan-tree-dump ".COND_MAX" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MIN" "optimized" } } */
/* { dg-final { scan-assembler-times "vmaxph"  1 } } */
/* { dg-final { scan-assembler-times "vminph"  1 } } */

#include "cond_op_maxmin_double-1.c"
