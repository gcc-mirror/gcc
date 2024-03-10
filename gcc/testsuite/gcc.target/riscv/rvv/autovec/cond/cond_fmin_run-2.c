/* { dg-do run { target { riscv_vector } } } */
/* { dg-additional-options "--param=riscv-autovec-preference=scalable -fno-vect-cost-model -ffast-math" } */

#define FN(X) __builtin_fmin##X
#include "cond_fmax_run-2.c"
