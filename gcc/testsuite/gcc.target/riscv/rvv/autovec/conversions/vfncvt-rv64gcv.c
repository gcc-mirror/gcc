/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=zvl" } */

#include "vfncvt-template.h"

/* { dg-final { scan-assembler-times {\tvfncvt\.f\.f\.w} 3 } } */
/* { dg-final { scan-assembler-times {\tvfncvt\.rod\.f\.f\.w} 1 } } */
