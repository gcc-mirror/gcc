/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define FACTOR 8
#include "adr_1.c"

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b,} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b,} 2 } } */
/* { dg-final { scan-assembler-not {\tadr\tz[0-9]+\.b,} } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h,} 2 } } */
/* { dg-final { scan-assembler-not {\tadr\tz[0-9]+\.h,} } } */

/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler-not {\tlsl\tz[0-9]+\.s,} } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.s, \[z[0-9]+\.s, z[0-9]+\.s, lsl 3\]} 2 } } */

/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-not {\tlsl\tz[0-9]+\.d,} } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, lsl 3\]} 2 } } */
