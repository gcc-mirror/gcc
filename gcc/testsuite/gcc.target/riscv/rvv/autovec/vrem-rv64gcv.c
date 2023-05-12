/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vrem-template.h"

/* TODO: Implement vector type promotion.  We should have 6 vrem.vv here.  */

/* { dg-final { scan-assembler-times {\tvrem\.vv} 5 } } */
/* { dg-final { scan-assembler-times {\tvremu\.vv} 6 } } */
