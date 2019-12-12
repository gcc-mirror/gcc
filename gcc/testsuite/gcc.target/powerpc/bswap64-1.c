/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -mno-popcntd -mdejagnu-cpu=power5" } */
/* { dg-require-effective-target lp64 } */
/* { dg-final { scan-assembler "lwbrx" } } */
/* { dg-final { scan-assembler "stwbrx" } } */

unsigned long ul;
unsigned long load_bswap64 (unsigned long *p) { return __builtin_bswap64 (*p); }
void store_bswap64 (unsigned long a) { ul = __builtin_bswap64 (a); }
