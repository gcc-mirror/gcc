/* { dg-options "-O3" } */

double __attribute__((noipa))
foo (double m, float *ptr)
{
  for (int i = 0; i < 256; i++)
    m = __builtin_fmax (m, ptr[i]);
  return m;
}
