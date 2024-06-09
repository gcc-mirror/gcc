/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -O3 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=zvl -ffast-math" } */

#include "cond_copysign-template.h"

/* { dg-final { scan-assembler-times {\tvfsgnj\.vv} 6 } } */
/* 1. The vectorizer wraps scalar variants of copysign into vector constants which
      expand cannot handle currently.
   2. match.pd convert .COPYSIGN (1, b) + COND_MUL to AND + XOR currently.  */
/* { dg-final { scan-assembler-times {\tvfsgnjx\.vv} 6 { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-times {\tvfsgnjn\.vv} 6 } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm} } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
