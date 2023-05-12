/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "shift-template.h"

/* TODO: For int16_t and uint16_t we need widening/promotion patterns.
   Therefore, expect only 4 vsll.vv instead of 6 for now.  */

/* { dg-final { scan-assembler-times {\tvsll\.vv} 4 } } */
/* { dg-final { scan-assembler-times {\tvsrl\.vv} 3 } } */
/* { dg-final { scan-assembler-times {\tvsra\.vv} 3 } } */
