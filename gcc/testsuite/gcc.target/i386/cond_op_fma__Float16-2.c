/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mprefer-vector-width=256 -DTYPE=_Float16 -D__BUILTIN_FMA=__builtin_fmaf16 -DNUM=100" } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-require-effective-target avx512vl } */

#define AVX512FP16
#include "cond_op_fma_double-2.c"
