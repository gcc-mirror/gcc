/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv32gcv_zvfh -mabi=ilp32d -fno-trapping-math -mrvv-vector-bits=scalable" } */

#include "vfncvt-ftoi-template.h"

/* The vectorizer only performs int -> float conversions with
   intermediate types with -fno-trapping-math.  */
/* { dg-final { scan-assembler-times {\tvfncvt\.rtz\.x\.f\.w} 9 } } */
/* { dg-final { scan-assembler-times {\tvfncvt\.rtz\.xu\.f\.w} 3 } } */
