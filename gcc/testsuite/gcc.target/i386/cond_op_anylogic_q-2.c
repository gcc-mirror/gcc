/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -DTYPE=int64" } */
/* { dg-require-effective-target avx512vl } */

#include "cond_op_anylogic_d-2.c"
