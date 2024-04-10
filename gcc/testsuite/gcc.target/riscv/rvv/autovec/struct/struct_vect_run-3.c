/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -mrvv-vector-bits=zvl -funroll-all-loops -fno-schedule-insns -fno-schedule-insns2" } */

#define TYPE uint32_t
#include "struct_vect_run-1.c"
