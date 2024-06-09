/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl" } */

#include "vncvt-template.h"

/* { dg-final { scan-assembler-times {\tvncvt.x.x.w} 10 } } */
