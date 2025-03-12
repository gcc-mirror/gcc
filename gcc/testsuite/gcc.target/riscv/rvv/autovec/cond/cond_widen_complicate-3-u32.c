/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable" } */

#include "cond_widen_complicate-3.h"

TEST_TYPE (uint64_t, uint32_t)

/* { dg-final { scan-assembler-times {\tvwmulu\.vv} 1 } } */
/* { dg-final { scan-assembler-not {\tvmerge\.vvm\t} } } */
