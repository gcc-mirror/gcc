/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=zvl -ffast-math" } */

#include "vadd-template.h"

/* { dg-final { scan-assembler-times {\tvadd\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvadd\.vi} 8 } } */
/* { dg-final { scan-assembler-times {\tvfadd\.vv} 9 } } */
