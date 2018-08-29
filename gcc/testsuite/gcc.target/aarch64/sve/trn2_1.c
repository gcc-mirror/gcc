/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=256" } */

#define BIAS 1
#include "trn1_1.c"

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\ttrn2\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\ttrn2\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\ttrn2\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h} 2 } } */
/* { dg-final { scan-assembler-times {\ttrn2\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b} 1 } } */
