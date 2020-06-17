/* { dg-do compile { target aarch64_little_endian } } */
/* { dg-options "-O2 -ftree-vectorize -fopenmp-simd -msve-vector-bits=128 -fno-tree-loop-distribute-patterns" } */

#define N 16

#include "single_1.c"

/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.16b, 0x1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.16b, 0x2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.8h, 0x3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.8h, 0x4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.4s, 0x5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmovi\tv[0-9]+\.4s, 0x6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #7\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #8\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfmov\tv[0-9]+\.8h, 1\.0e\+0\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tfmov\tv[0-9]+\.4s, 2\.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tv[0-9]+\.2d, 3\.0e\+0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tstr\tq[0-9]+,} 11 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tstr\tq[0-9]+,} 10 } } */

/* { dg-final { scan-assembler-not {\twhile} } } */
/* { dg-final { scan-assembler-not {\tb} } } */
/* { dg-final { scan-assembler-not {\tcmp} } } */
/* { dg-final { scan-assembler-not {\tindex} } } */
/* { dg-final { scan-assembler-not {\tptrue\t} { xfail *-*-* } } } */
