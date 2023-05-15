/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv -mabi=ilp32d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "shift-template.h"

/* TODO: For int16_t and uint16_t we need widening/promotion patterns.
   We don't check the assembler number since lacking patterns make
   auto-vectorization inconsistent in LMUL = 1/2/4/8.  */

/* { dg-final { scan-assembler {\tvsll\.vv} } } */
/* { dg-final { scan-assembler {\tvsrl\.vv} } } */
/* { dg-final { scan-assembler {\tvsra\.vv} } } */

