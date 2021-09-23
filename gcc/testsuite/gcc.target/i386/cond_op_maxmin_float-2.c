/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -DTYPE=float -DFN_MAX=fmaxf -DFN_MIN=fminf" } */
/* { dg-require-effective-target avx512vl } */

#include "cond_op_maxmin_double-2.c"
