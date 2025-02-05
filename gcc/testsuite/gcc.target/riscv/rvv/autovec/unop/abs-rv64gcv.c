/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=zvl -ffast-math" } */

#include "abs-template.h"

/* { dg-final { scan-assembler-times {\tvseti?vli\s+[a-z0-9,]+,ta,ma} 7 } } */
/* { dg-final { scan-assembler-times {\tvrsub\.vi} 4 } } */
/* { dg-final { scan-assembler-times {\tvmax\.vv\sv[0-9]+,v[0-9]+,v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {\tvfabs.v} 3 } } */
