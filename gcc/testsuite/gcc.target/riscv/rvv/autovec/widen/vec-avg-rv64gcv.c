/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable" } */

#include "vec-avg-template.h"

/* { dg-final { scan-assembler-times {csrwi\s*vxrm,\s*2} 6 } } */
/* { dg-final { scan-assembler-times {vaaddu\.vv} 6 } } */
/* { dg-final { scan-assembler-times {vaadd\.vv} 6 } } */
