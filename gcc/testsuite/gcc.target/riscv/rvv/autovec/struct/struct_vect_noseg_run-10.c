/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-std=c99 -mrvv-vector-bits=scalable -fno-vect-cost-model -mno-autovec-segment" } */

#include "struct_vect_run-10.c"
