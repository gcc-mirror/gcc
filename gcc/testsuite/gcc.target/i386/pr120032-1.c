/* { dg-do compile } */
/* { dg-options "-O2 -mlzcnt" } */

unsigned int
ZSTD_countLeadingZeros32_fallback(unsigned int val)
{
  static const unsigned int DeBruijnClz[32]
    = { 0, 9, 1, 10, 13, 21, 2, 29,
	11, 14, 16, 18, 22, 25, 3, 30,
	8, 12, 20, 28, 15, 17, 24, 7,
	19, 27, 23, 6, 26, 5, 4, 31};
  if (val == 0)
    __builtin_abort ();
  val |= val >> 1;
  val |= val >> 2;
  val |= val >> 4;
  val |= val >> 8;
  val |= val >> 16;
  return 31 - DeBruijnClz[(val * 0x07C4ACDDU) >> 27];
}

/* { dg-final { scan-assembler "lzcnt" } } */
