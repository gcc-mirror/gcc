/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -mcpu=cell" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_ppu_ok } */
/* { dg-final { scan-assembler "ldbrx" } } */
/* { dg-final { scan-assembler "stdbrx" } } */

unsigned long ul;
unsigned long load_bswap64 (unsigned long *p) { return __builtin_bswap64 (*p); }
void store_bswap64 (unsigned long a) { ul = __builtin_bswap64 (a); }
