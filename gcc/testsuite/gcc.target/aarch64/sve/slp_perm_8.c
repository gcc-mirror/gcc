/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

void
f (short *restrict s, signed char *restrict c)
{
  for (int i = 0; i < 8; i += 2)
    {
      s[i] = c[i];
      s[i + 1] = c[i];
    }
}

/* Ideally this would use LD1SB, but currently we use LD1B and
   sign-extend it after the permute.  */
/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.h, vl6\n} } } */
/* { dg-final { scan-assembler {\tld1s?b\tz[0-9]+\.h} } } */
/* { dg-final { scan-assembler {\ttrn1\tz[0-9]+\.h,} } } */
