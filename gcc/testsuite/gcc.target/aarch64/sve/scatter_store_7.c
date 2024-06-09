/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps --param aarch64-vect-compare-costs=0" } */

#define INDEX16 uint16_t
#define INDEX32 uint32_t

#include "scatter_store_6.c"

/* { dg-final { scan-assembler-times {\tuunpkhi\tz[0-9]+\.s, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tuunpklo\tz[0-9]+\.s, z[0-9]+\.h\n} 3 } } */
/* { dg-final { scan-assembler-times {\tuunpkhi\tz[0-9]+\.d, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tuunpklo\tz[0-9]+\.d, z[0-9]+\.s\n} 3 } } */
/* Either extension type is OK here.  */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, [us]xtw 2\]\n} 6 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 6 } } */
