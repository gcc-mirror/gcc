/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#define OP(x,y,z) ((~(x) & (z)) | ((y) & ~(z)))

#include "bitsel_1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */

/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.[bhsd]} } } */
/* { dg-final { scan-assembler-not {\tbic\tz[0-9]+\.[bhsd]} } } */

/* { dg-final { scan-assembler-times {\tbsl1n\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */
