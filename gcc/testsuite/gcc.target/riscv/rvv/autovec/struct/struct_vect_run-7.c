/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#define TYPE uint16_t
#define ITYPE int16_t
#include "struct_vect_run-6.c"
