/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl" } */

#include "shift-scalar-template.h"

/* { dg-final { scan-assembler-times {\tvsll\.vi} 31 } } */
/* { dg-final { scan-assembler-times {\tvsll\.vx} 1 } } */
