/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mavx512bw -mprefer-vector-width=256 -DTYPE=uint16" } */
/* { dg-require-effective-target avx512vl } */
/* { dg-require-effective-target avx512bw } */

#include "cond_op_maxmin_d-2.c"
