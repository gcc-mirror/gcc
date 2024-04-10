/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -ffast-math" } */

#include "vmin-template.h"

/* { dg-final { scan-assembler-times {\tvmin\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvminu\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvfmin\.vv} 6 } } */
