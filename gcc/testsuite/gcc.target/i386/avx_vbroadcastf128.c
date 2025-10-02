/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O3" } */
/* { dg-final { scan-assembler-not "vpermpd"} } */
/* { dg-final { scan-assembler {(?n)vbroadcastf(?:128|64x2)} } } */

void
foo (double* __restrict a, double*  b, double* c, int n)
{
  for (int i = 0; i != n; i+=4)
    {
      a[i] += b[i] * c[i];
      a[i+1] += b[i+1] * c[i+1];
      a[i+2] += b[i] * c[i+2];
      a[i+3] += b[i+1] * c[i+3];
    }

}
