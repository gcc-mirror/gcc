/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -ffast-math" } */

#include "cond_widen_complicate-3.h"

TEST_TYPE (double, float)

/* { dg-final { scan-assembler-times {\tvfwmul\.vv} 1 } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm\t} } } */
