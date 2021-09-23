/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -mavx512bw -DTYPE=short" } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */

#define AVX512BW
#include "cond_op_addsubmul_d-2.c"
