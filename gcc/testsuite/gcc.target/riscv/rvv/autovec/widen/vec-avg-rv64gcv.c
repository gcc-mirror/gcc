/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=scalable" } */

#include "vec-avg-template.h"

/* { dg-final { scan-assembler-times {\tvwadd\.vv} 6 } } */
/* { dg-final { scan-assembler-times {\tvwaddu\.vv} 6 } } */
/* { dg-final { scan-assembler-times {\tvadd\.vi} 6 } } */
/* { dg-final { scan-assembler-times {\tvnsrl\.wi} 6 } } */
/* { dg-final { scan-assembler-times {\tvnsra\.wi} 6 } } */
