/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -fwrapv --save-temps" } */

#define INDEX16 uint16_t
#define INDEX32 uint32_t

#include "scatter_store_8.c"

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, uxtw\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, uxtw 1\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, uxtw 2\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+.d, uxtw 3\]\n} 3 } } */

/* { dg-final { scan-assembler-times {\tuxt.\tz} 8 } } */
/* { dg-final { scan-assembler-times {\tuxth\tz[0-9]+\.s,} 8 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.s,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 } } */
