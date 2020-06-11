/* PR middle-end/70550 */
/* { dg-do compile } */
/* { dg-additional-options "-Wuninitialized" } */

void bar (int);

void
foo (void)
{
  int i, j, k, l, m, n, o, p, q;
  #pragma omp task				/* { dg-bogus "is used uninitialized" } */
  {
    i = 2;
    bar (i);
  }
  #pragma omp taskloop				/* { dg-bogus "is used uninitialized" } */
  for (j = 0; j < 10; j++)
    {
      k = 7;
      bar (k);
    }
  #pragma omp task firstprivate (l)		/* { dg-warning "is used uninitialized" } */
  {
    l = 2;
    bar (l);
  }
  #pragma omp taskloop firstprivate (m)		/* { dg-warning "is used uninitialized" } */
  for (j = 0; j < 10; j++)
    {
      m = 7;
      bar (m);
    }
  #pragma omp task shared (n)			/* { dg-bogus "is used uninitialized" } */
  {
    n = 2;
    bar (n);
  }
  #pragma omp taskloop shared (o)		/* { dg-bogus "is used uninitialized" } */
  for (j = 0; j < 10; j++)
    {
      o = 7;
      bar (o);
    }
  #pragma omp task private (p)			/* { dg-bogus "is used uninitialized" } */
  {
    p = 2;
    bar (p);
  }
  #pragma omp taskloop shared (q)		/* { dg-bogus "is used uninitialized" } */
  for (j = 0; j < 10; j++)
    {
      q = 7;
      bar (q);
    }
}
