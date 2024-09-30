/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=128 --param aarch64-autovec-preference=sve-only" } */

#include "cond_xorsign_1.c"

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, z[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, z[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, z[0-9]+\.d,} 1 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\tfmul} } } */
/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
