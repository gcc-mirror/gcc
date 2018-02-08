/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math --save-temps" } */

#define INDEX32 uint32_t
#define INDEX64 uint64_t

#include "mask_scatter_store_1.c"

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 2\]\n} 36 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+\.s, uxtw 2\]\n} 9 } } */

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, x[0-9]+, lsl 3\]\n} 36 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+\.d, lsl 3\]\n} 9 } } */
