/* { dg-do run { target { riscv_v && riscv_zvfh_hw } } } */
/* { dg-additional-options "-std=c99 --param=riscv-autovec-preference=scalable -fno-vect-cost-model" } */

#define TYPE _Float16
#define ITYPE int16_t
#include "struct_vect_run-6.c"
