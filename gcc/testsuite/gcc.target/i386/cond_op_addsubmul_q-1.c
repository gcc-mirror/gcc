/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -DTYPE=long -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump ".COND_ADD" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_SUB" "optimized" } } */
/* { dg-final { scan-tree-dump ".COND_MUL" "optimized" } } */
#define AVX512DQ
#include "cond_op_addsubmul_d-1.c"
