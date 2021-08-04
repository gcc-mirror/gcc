/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -DTYPE=float -D__BUILTIN_FMA=__builtin_fmaf" } */
/* { dg-require-effective-target avx512vl } */

#include "cond_op_fma_double-2.c"
