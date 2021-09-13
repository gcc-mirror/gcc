/* { dg-options "-O3 -msve-vector-bits=128" } */

void
f (float *x)
{
  for (int i = 0; i < 100; ++i)
    if (x[i] > 1.0f)
      x[i] -= 1.0f;
}

/* { dg-final { scan-assembler {\tld1w\tz} } } */
/* { dg-final { scan-assembler {\tfcmgt\tp} } } */
/* { dg-final { scan-assembler {\tfsub\tz} } } */
/* { dg-final { scan-assembler {\tst1w\tz} } } */
