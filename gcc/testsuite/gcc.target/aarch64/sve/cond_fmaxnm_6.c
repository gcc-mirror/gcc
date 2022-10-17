/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "cond_fmaxnm_2.c"

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #0\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1\.0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #1\.0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #2\.0} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #2\.0} 1 } } */

/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmaxnm\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-not {\tmov\tz} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
