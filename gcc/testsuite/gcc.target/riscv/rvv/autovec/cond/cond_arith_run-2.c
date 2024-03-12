/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "--param=riscv-autovec-preference=scalable -fno-vect-cost-model" } */

#include "cond_arith_run-1.c"
