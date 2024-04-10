/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -ffast-math" } */

#include "vmul-template.h"

/* { dg-final { scan-assembler-times {\tvmul\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvfmul\.vv} 6 } } */
