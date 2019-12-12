/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "rotldi" } } */

unsigned long f (unsigned long x)
{
  return ((x << 1) | (x >> 63)) & 0xffffffff;
}
