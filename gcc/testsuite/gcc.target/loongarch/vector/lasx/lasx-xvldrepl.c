/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler-times "xvldrepl.w" 2} } */

#define N 258

float a[N], b[N], c[N];

void
test ()
{
  for (int i = 0; i < 256; i++)
    {
      a[i] = c[0] * b[i] + c[1];
    }
}
