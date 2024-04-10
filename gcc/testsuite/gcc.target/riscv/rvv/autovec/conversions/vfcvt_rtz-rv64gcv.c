/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable" } */

#include "vfcvt_rtz-template.h"

/* { dg-final { scan-assembler-times {\tvfcvt\.rtz\.x\.f\.v} 3 } } */
/* { dg-final { scan-assembler-times {\tvfcvt\.rtz\.xu\.f\.v} 3 } } */
