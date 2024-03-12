/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax -ffast-math" } */

#include "vmax-template.h"

/* { dg-final { scan-assembler-times {\tvmax\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvmaxu\.vv} 8 } } */
/* { dg-final { scan-assembler-times {\tvfmax\.vv} 6 } } */
