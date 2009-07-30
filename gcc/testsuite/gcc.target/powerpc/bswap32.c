/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "lwbrx" } } */
/* { dg-final { scan-assembler "stwbrx" } } */

unsigned int ui;
unsigned int load_bswap32 (unsigned int *p) { return __builtin_bswap32 (*p); }
void store_bswap32 (unsigned int a) { ui = __builtin_bswap32 (a); }
