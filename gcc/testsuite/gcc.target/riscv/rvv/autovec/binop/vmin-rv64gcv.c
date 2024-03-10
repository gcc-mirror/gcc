/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vmin-template.h"

/* { dg-final { scan-assembler-times {\tvmin\.vv} 6 } } */
/* { dg-final { scan-assembler-times {\tvminu\.vv} 6 } } */
