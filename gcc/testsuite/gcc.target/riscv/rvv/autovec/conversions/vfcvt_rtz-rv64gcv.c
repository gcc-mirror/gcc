/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d --param=riscv-autovec-preference=scalable" } */

#include "vfcvt_rtz-template.h"

/* { dg-final { scan-assembler-times {\tvfcvt\.rtz} 2 } } */
