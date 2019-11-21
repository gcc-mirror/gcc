/* { dg-options "-O3 -mstrict-align" } */

void
f (unsigned int *restrict x, unsigned int *restrict y,
   unsigned char *restrict z, unsigned int n)
{
  for (unsigned int i = 0; i < n % 4; ++i)
    x[i] = x[i] + y[i] + z[i];
}
