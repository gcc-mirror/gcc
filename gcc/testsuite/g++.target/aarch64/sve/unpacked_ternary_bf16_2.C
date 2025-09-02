/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=2048 -fno-trapping-math" } */

#include "unpacked_ternary_bf16_1.C"

/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b} 4 } } */

/* { dg-final { scan-assembler-times {\tbfmla\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfmls\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
