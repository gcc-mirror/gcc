/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=zvl -ffast-math" } */

#include "vneg-template.h"

/* { dg-final { scan-assembler-times {\tvrsub\.vi} 4 } } */
/* { dg-final { scan-assembler-times {\tvfneg\.v} 3 } } */
