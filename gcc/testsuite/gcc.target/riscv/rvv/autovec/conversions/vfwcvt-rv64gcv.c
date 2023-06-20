/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax" } */

#include "vfwcvt-template.h"

/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.f\.v} 4 } } */
