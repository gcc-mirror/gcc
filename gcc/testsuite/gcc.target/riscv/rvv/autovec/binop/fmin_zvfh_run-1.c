/* { dg-do run { target { riscv_zvfh } } } */
/* { dg-additional-options "--param=riscv-autovec-preference=scalable -fno-signaling-nans" } */

#define FN(X) __builtin_fmin##X
#include "fmax_zvfh_run-1.c"
