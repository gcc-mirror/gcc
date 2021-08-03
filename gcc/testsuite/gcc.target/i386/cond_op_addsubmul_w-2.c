/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -mavx512bw -DTYPE=short" } */

#define AVX512BW
#include "cond_op_addsubmul_d-2.c"
