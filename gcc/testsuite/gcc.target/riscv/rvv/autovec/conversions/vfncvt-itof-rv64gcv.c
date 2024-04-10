/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable" } */

#include "vfncvt-itof-template.h"

/* { dg-final { scan-assembler-times {\tvfncvt\.f\.x\.w} 5 } } */
/* { dg-final { scan-assembler-times {\tvfncvt\.f\.xu\.w} 1 } } */
