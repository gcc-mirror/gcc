/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mprefer-vector-width=256 -DTYPE=_Float16" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512fp16 } */

#define AVX512FP16
#include "cond_op_addsubmuldiv_double-2.c"
