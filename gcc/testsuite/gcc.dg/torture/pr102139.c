/* { dg-do run } */
/* { dg-additional-options "-ftree-slp-vectorize" } */

typedef double aligned_double __attribute__((aligned(2*sizeof(double))));

void __attribute__((noipa))
bar (int aligned, double *p)
{
  if (aligned)
    {
      *(aligned_double *)p = 3.;
      p[1] = 4.;
    }
  else
    {
      p[2] = 0.;
      p[3] = 1.;
    }
}

void __attribute__((noipa))
foo (int i)
{
  if (i)
    __builtin_exit (0);
}
void __attribute__((noipa))
baz (double *p)
{
  p[0] = 0.;
  p[1] = 1.;
  foo (1);
  *(aligned_double *)p = 3.;
  p[1] = 4.;
}

double x[8] __attribute__((aligned(2*sizeof (double))));
int main()
{
  bar (0, &x[1]);
  baz (&x[1]);
  return 0;
}
