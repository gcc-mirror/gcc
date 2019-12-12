/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" } */

#include "loop_add_4.c"

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.b, p[0-7]+/z, \[x[0-9]+, x[0-9]+\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b, p[0-7]+, \[x[0-9]+, x[0-9]+\]} 8 } } */

/* The induction vector is invariant for steps of -16 and 16.  */
/* { dg-final { scan-assembler-not {\tsub\tz[0-9]+\.b, z[0-9]+\.b, #} } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, #} 6 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 8 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 1\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 1\]} 8 } } */

/* The (-)17 * 16 is out of range.  */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, z[0-9]+\.h, #} 2 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, #} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 10 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 2\]} 8 } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, z[0-9]+\.s, #} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, #} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 8 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, x[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 3\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 3\]} 8 } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, z[0-9]+\.d, #} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, #} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 8 } } */
