/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048" } */

#include "unpacked_cond_fdiv_1.c"

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 3 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 6 } } */
/* { dg-final { scan-assembler-times {\tand} 9 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 7 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 7 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 7 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfdivr\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfdivr\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
