/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-trapping-math" } */
/* { dg-require-effective-target fenv_exceptions } */

#define TEST_EXCEPTIONS 0
#include "vcond_run-2.c"
