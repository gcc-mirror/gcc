/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define INDEX32 uint32_t
#define INDEX64 uint64_t

#include "gather_load_1.c"

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+.s, uxtw 2\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 3 } } */
