/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "--param=riscv-autovec-preference=scalable -fno-trapping-math" } */
/* { dg-require-effective-target fenv_exceptions } */

#define TEST_EXCEPTIONS 0
#include "vcond_run-2.c"
