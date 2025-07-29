/* { dg-do compile } */
/* { dg-options "-O2 -moverride=sve_width=2048 -ftree-vectorize -fno-trapping-math" } */

#include "unpacked_frintp_1.c"

/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b} 3 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 2 } } */

/* { dg-final { scan-assembler-times {\tfrintp\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfrintp\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
