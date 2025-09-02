/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=2048" } */

#include "unpacked_cond_ternary_bf16_1.C"

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tand} 8 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/z, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tbfmla\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tbfmls\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
