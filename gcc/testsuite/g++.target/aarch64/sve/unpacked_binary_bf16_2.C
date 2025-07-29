/* { dg-do compile }*/
/* { dg-options "-O2 -ffinite-math-only -fno-signed-zeros -fno-trapping-math -msve-vector-bits=2048 " } */

#include "unpacked_binary_bf16_1.C"

/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b} 10 } } */

/* { dg-final { scan-assembler-times {\tbfadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfsub\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfmul\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tbfminnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
