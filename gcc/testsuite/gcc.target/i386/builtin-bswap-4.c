/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "bswap\[ \t\]" } } */

short foo (short x)
{
  return __builtin_bswap16 (x);
}
