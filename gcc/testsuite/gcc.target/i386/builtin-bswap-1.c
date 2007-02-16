/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "builtin_bswap" } } */

long foo (long a)
{
  long b;

#if __LP64__
  b = __builtin_bswap64 (a);
#else
  b = __builtin_bswap32 (a);
#endif

  return b;
}
