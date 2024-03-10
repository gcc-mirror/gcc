/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d --param=riscv-autovec-preference=scalable" } */

#include "vfncvt-ftoi-template.h"

/* { dg-final { scan-assembler-times {\tvfncvt\.rtz\.x\.f\.w} 3 } } */
/* { dg-final { scan-assembler-times {\tvfncvt\.rtz\.xu\.f\.w} 3 } } */
