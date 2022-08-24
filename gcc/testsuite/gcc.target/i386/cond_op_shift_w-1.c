/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized -DTYPE=int16" } */
/* { dg-final { scan-tree-dump-times "\.COND_" 4 "optimized" } } */
/* { dg-final { scan-assembler-times "vpsraw"  1 } } */
/* { dg-final { scan-assembler-times "vpsllw"  1 } } */

#include "cond_op_shift_d-1.c"
