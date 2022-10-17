/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mprefer-vector-width=256 -DTYPE=_Float16 -DFN_MAX=__builtin_fmaxf16 -DFN_MIN=__builtin_fminf16 -ffast-math" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512fp16 } */

#define AVX512FP16
#include "cond_op_maxmin_double-2.c"
