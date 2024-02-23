/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl" } */

#include "vfwcvt-template.h"

/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.f\.v} 4 } } */
