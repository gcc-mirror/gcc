/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vmul-template.h"

/* { dg-final { scan-assembler-times {\tvmul\.vv} 12 } } */
