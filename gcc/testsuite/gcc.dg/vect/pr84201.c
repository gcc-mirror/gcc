/* { dg-do compile } */
/* { dg-additional-options "-Ofast --param vect-induction-float=0" } */

void foo (float *a, float f, float s, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i] = f;
      f += s;
    }
}

void bar (double *a, double f, double s, int n)
{
  for (int i = 0; i < n; ++i)
    {
      a[i] = f;
      f += s;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 2 "vect" } } */
