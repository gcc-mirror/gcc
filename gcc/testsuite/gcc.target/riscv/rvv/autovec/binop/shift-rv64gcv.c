/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "shift-template.h"

/* { dg-final { scan-assembler-times {\tvsll\.vv} 6 } } */
/* { dg-final { scan-assembler-times {\tvsrl\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvsra\.vv} 3 } } */
