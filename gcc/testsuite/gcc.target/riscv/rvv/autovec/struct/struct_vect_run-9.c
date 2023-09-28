/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=riscv-autovec-preference=scalable -fno-vect-cost-model" } */

#define TYPE uint64_t
#define ITYPE int64_t
#include "struct_vect_run-6.c"
