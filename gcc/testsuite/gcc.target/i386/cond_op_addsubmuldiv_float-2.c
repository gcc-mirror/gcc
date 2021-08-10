/* { dg-do run } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -DTYPE=float" } */
/* { dg-require-effective-target avx512vl } */

#include "cond_op_addsubmuldiv_double-2.c"
