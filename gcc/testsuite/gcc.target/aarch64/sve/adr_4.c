/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define FACTOR 16
#include "adr_1.c"

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]\.[bhsd],} 8 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]\.[bhsd],} 8 } } */
/* { dg-final { scan-assembler-not {\tadr\tz[0-9]\.[bhsd],} } } */
