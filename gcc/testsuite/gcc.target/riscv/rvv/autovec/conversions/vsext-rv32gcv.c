/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl" } */

#include "vsext-template.h"

/* { dg-final { scan-assembler-times {\tvsext\.vf2} 3 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf4} 2 } } */
/* { dg-final { scan-assembler-times {\tvsext\.vf8} 1 } } */
