/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=2048 -fno-trapping-math" } */

#include "unpacked_cvtf_1.c"

/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b} 14 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
