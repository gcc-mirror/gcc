/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d --param=riscv-autovec-preference=scalable" } */

#include "vfwcvt-itof-template.h"

/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.x\.v} 3 } } */
/* { dg-final { scan-assembler-times {\tvfwcvt\.f\.xu\.v} 3 } } */
