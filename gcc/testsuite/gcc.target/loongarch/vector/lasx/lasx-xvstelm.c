/* { dg-do compile } */
/* { dg-options "-O3 -mlasx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "xvstelm.w" 8} } */

#define LEN 256

float a[LEN], b[LEN], c[LEN];

void
test ()
{
  for (int i = 0; i < LEN; i += 2)
    a[i] = b[i] + c[i];
}
