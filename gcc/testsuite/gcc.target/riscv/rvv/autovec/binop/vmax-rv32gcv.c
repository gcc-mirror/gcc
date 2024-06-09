/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -ffast-math" } */

#include "vmax-template.h"

/* { dg-final { scan-assembler-times {\tvmax\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvmaxu\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvfmax\.vv} 6 } } */
