/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 -fno-trapping-math" } */

#include "unpacked_fmls_1.c"

/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b} 3 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 4 } } */

/* { dg-final { scan-assembler-times {\t(fmls|fmsb)\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\t(fmls|fmsb)\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
