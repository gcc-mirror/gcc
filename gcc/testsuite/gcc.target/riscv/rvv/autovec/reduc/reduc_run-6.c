/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model" } */

#define N 0x1100

#include "reduc_run-5.c"
