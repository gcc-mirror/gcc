/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort (void);

static void
foo (double a, double weight, const double *ring, double *phase)
{
  *phase = *ring * weight;
}

void
foo2 (void)
{
  foo (0, 1, (double *) 0, (double *) 0);
}

int
main (void)
{
  double t1 = 1, c1;
  foo (0, 1, &t1, &c1);
  if (c1 < 0.5)
    abort();

  return 0;
}
