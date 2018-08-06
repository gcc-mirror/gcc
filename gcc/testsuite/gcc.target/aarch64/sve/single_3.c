/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fopenmp-simd -msve-vector-bits=1024" } */

#define N 128
#include "single_1.c"

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, #4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, #6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.h, #1\.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #2\.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #3\.0e\+0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, vl128\n} 2 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.h, vl64\n} 3 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s, vl32\n} 3 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.d, vl16\n} 3 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 } } */

/* { dg-final { scan-assembler-not {\twhile} } } */
/* { dg-final { scan-assembler-not {\tb} } } */
/* { dg-final { scan-assembler-not {\tcmp} } } */
/* { dg-final { scan-assembler-not {\tindex} } } */
