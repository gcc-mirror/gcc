/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 --param=aarch64-autovec-preference=sve-only -fno-trapping-math" } */

#include "unpacked_fcm_1.c"

/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.d} } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 32 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 32 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 32 } } */

/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmle\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmlt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfcmgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfcmuo\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
