/* { dg-do compile } */
/* { dg-additional-options "-mavx512bw" { target { x86_64-*-* i?86-*-* } } } */

void f(int w, int *out, double *d)
{
  for (int j = 0; j < w; j++)
    {
      const int i = (j >= w / 2);
      out[j] += d[i];
    }
}
