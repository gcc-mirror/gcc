/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vdiv-template.h"

/* TODO: Implement vector type promotion.  We should have 6 vdiv.vv here.  */

/* { dg-final { scan-assembler-times {\tvdiv\.vv} 4 } } */
/* { dg-final { scan-assembler-times {\tvdivu\.vv} 6 } } */
