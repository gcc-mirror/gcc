/* { dg-do compile }*/
/* { dg-options "-O -ffinite-math-only -fno-signed-zeros -msve-vector-bits=2048 " } */

#include "unpacked_cond_binary_bf16_1.C"

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 15 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 15 } } */
/* { dg-final { scan-assembler-times {\tand} 30 } } */

/* { dg-final { scan-assembler-times {\tbfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tbfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tbfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */

/* { dg-final { scan-assembler-times {\tbfminnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tbfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */

// There's no BFSUBR.
/* { dg-final { scan-assembler-times {\tsel\t} 2 } } */
