/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -funroll-all-loops -fno-schedule-insns -fno-schedule-insns2 -mno-autovec-segment" } */

#include "struct_vect-3.c"

/* { dg-final { scan-assembler-not {v[ls]seg[2-8]e[123468]+\.v} } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
