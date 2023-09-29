/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 --param=riscv-autovec-preference=fixed-vlmax -funroll-all-loops -fno-schedule-insns -fno-schedule-insns2" } */

#define TYPE uint64_t
#include "struct_vect_run-1.c"
