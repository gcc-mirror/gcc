/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -mprefer-vector-width=256 -mavx512dq -DTYPE=long" } */

#include "cond_op_addsubmul_d-2.c"
