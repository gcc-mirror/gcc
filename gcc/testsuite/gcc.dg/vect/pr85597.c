/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mfma" { target { x86_64-*-* i?86-*-* } } } */

extern double fma (double, double, double);

static inline void
bar (int i, double *D, double *S)
{
  while (i-- > 0)
    {
      D[0] = fma (1, S[0], D[0]);
      D[1] = fma (1, S[1], D[1]);
      D[2] = fma (1, S[2], D[2]);
      D[3] = fma (1, S[3], D[3]);
      D += 4;
      S += 4;
    }
}

void
foo (double *d, double *s)
{
  bar (10, d, s);
}

