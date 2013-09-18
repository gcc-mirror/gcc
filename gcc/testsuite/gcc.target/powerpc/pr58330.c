/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O -mno-popcntb" } */
/* { dg-final { scan-assembler-not "stwbrx" } } */

void
write_reverse (unsigned long *addr, unsigned long val)
{
  unsigned long reverse = __builtin_bswap64 (val);
  __atomic_store_n (addr, reverse, __ATOMIC_RELAXED);
}
