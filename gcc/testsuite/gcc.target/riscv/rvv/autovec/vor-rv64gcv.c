/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vor-template.h"

/* { dg-final { scan-assembler-times {\tvor\.vv} 12 } } */
/* { dg-final { scan-assembler-times {\tvor\.vi} 6 } } */
