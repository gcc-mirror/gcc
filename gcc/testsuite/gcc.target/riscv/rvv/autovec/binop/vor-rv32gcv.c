/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vor-template.h"

/* { dg-final { scan-assembler-times {\tvor\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvor\.vi} 8 } } */
