/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048" } */

#include "unpacked_cond_fmul_1.c"

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 5 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 10 } } */
/* { dg-final { scan-assembler-times {\tand} 15 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 10 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 10 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 10 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
