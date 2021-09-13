/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -fdump-tree-optimized -DTYPE=uint64" } */
/* { dg-final { scan-tree-dump-times ".COND_SHR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times ".COND_SHL" 2 "optimized" } } */
/* { dg-final { scan-assembler-times "vpsrlq"  1 } } */
/* { dg-final { scan-assembler-times "vpsllq"  1 } } */
/* { dg-final { scan-assembler-times "vpsrlq"  1 } } */
/* { dg-final { scan-assembler-times "vpsllq"  1 } } */

#include "cond_op_shift_d-1.c"
