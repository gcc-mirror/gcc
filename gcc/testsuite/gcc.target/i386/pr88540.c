/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4.1" } */

void test(double* __restrict d1, double* __restrict d2, double* __restrict d3)
{
  for (int n = 0; n < 2; ++n)
    d3[n] = d1[n] < d2[n] ? d1[n] : d2[n];
}

/* { dg-final { scan-assembler "minpd" { xfail *-*-* } } } */
