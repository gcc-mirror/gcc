/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable" } */

#include "cond_widen_complicate-3.h"

TEST_TYPE (int32_t, int16_t)

/* { dg-final { scan-assembler-times {\tvwmul\.vv} 1 } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm\t} } } */
