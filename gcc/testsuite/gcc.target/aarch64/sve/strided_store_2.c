/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define INDEX8 uint8_t
#define INDEX16 uint16_t
#define INDEX32 uint32_t
#define INDEX64 uint64_t

#include "strided_store_1.c"

/* 8 and 16 bits are signed because the multiplication promotes to int.
   Using uxtw for all 9 would be OK.  */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, sxtw 2\]\n} 6 } } */
/* The 32-bit loop needs to honor the defined overflow in uint32_t,
   so we vectorize the offset calculation.  This means that the
   64-bit version needs two copies.  */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x[0-9]+, z[0-9]+.s, uxtw 2\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x[0-9]+, z[0-9]+.d, lsl 3\]\n} 15 } } */
