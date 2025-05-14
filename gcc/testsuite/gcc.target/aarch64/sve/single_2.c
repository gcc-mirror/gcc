/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fopenmp-simd -msve-vector-bits=512 -fno-tree-loop-distribute-patterns" } */

#define N 64
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

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, vl64\n} 9 { target aarch64_big_endian } } } */

/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h,} 3 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 3 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tstr\tz[0-9]+, \[x0\]} 2 { target aarch64_big_endian } } } */
/* { dg-final { scan-assembler-times {\tstr\tz[0-9]+, \[x0\]} 11 { target aarch64_little_endian } } } */

/* { dg-final { scan-assembler-not {\twhile} } } */
/* { dg-final { scan-assembler-not {\tb} } } */
/* { dg-final { scan-assembler-not {\tcmp} } } */
/* { dg-final { scan-assembler-not {\tindex} } } */
