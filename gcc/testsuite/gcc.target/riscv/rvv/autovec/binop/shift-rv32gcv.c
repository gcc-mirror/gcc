/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl" } */

#include "shift-template.h"

/* { dg-final { scan-assembler-times {\tvsll\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvsrl\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvsra\.vv} 4 } } */
