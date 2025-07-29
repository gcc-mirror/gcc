/* { dg-do compile } */
/* { dg-options "-O2 -moverride=sve_width=2048 -mtune=generic -ftree-vectorize" } */

#include "unpacked_cond_frinta_1.c"

/* Test that we don't drop SELs without -fno-trapping-math.  */

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 4 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 3 } } */

/* { dg-final { scan-assembler-times {\tfrinta\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfrinta\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tsel\t} 6 } } */
