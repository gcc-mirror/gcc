/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d --param=riscv-autovec-preference=fixed-vlmax -ffast-math" } */

#include "vneg-template.h"

/* { dg-final { scan-assembler-times {\tvneg\.v} 4 } } */
/* { dg-final { scan-assembler-times {\tvfneg\.v} 3 } } */
