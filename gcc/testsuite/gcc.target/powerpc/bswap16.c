/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "lhbrx" } } */
/* { dg-final { scan-assembler "sthbrx" } } */

unsigned short us;
unsigned int load_bswap16 (unsigned short *p) { return __builtin_bswap16 (*p); }
void store_bswap16 (unsigned int a) { us = __builtin_bswap16 (a); }
