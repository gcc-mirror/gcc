/* { dg-do run { target { riscv_zvfh && riscv_zfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model -fno-signaling-nans" } */

#define FN(X) __builtin_fmin##X
#include "cond_fmax_zvfh_run-2.c"
