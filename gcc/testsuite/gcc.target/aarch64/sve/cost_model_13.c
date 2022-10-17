/* { dg-options "-O3 -mtune=neoverse-v1" } */

int
f11 (short *restrict x, int n)
{
  short res = 0;
  for (int i = 0; i < n; ++i)
    res += x[i];
  return res;
}

/* We should use SVE rather than Advanced SIMD.  */
/* { dg-final { scan-assembler {\tld1h\tz[0-9]+\.h,} } } */
/* { dg-final { scan-assembler {\tadd\tz[0-9]+\.h,} } } */
/* { dg-final { scan-assembler-not {\tldr\tq[0-9]+,} } } */
/* { dg-final { scan-assembler-not {\tv[0-9]+\.8h,} } } */
