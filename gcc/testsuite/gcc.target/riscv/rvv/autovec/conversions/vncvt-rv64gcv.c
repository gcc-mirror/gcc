/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

#include "vncvt-template.h"

/* { dg-final { scan-assembler-times {\tvnsrl.wi} 10 } } */
