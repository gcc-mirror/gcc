/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048" } */

#include "unpacked_cond_fadd_1.c"

/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.s} 11 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.d} 22 } } */
/* { dg-final { scan-assembler-times {\tand} 33 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 19 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 19 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 19 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 5 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 10 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1.0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1.0\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
