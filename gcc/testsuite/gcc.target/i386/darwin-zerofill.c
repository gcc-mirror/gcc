/* { dg-do compile { target i?86-apple-darwin* x86_64-apple-darwin* } } */
/* { dg-options "-fno-common" } */

/* { dg-final { scan-assembler ".zerofill __DATA, __bss11, _ji, 4000000, 11" } } */
/* { dg-final { scan-assembler ".zerofill __TEXT, __bss8, _cj, 4000000, 8" } } */
/* PR33120 */

int ji[1000000] __attribute((aligned(2048)));
const int cj[1000000] __attribute((aligned(256)));
