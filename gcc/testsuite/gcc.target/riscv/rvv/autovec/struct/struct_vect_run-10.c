/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-std=gnu99 -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#define TYPE _Float16
#define ITYPE int16_t

/* Use a lower iteration count so we do not run into precision problems.  */
#define N 46
#include "struct_vect_run-6.c"
