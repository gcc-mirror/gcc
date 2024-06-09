/* { dg-do run { target { riscv_zvfh && riscv_zfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-signaling-nans" } */

#define FN(X) __builtin_fmin##X
#include "fmax_zvfh_run-1.c"
