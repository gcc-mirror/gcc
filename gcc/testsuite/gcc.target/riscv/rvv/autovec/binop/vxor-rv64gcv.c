/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

#include "vxor-template.h"

/* { dg-final { scan-assembler-times {\tvxor\.vv} 16 } } */
/* { dg-final { scan-assembler-times {\tvxor\.vi} 8 } } */
