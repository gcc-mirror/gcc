// { dg-options "-fopenmp" }

void bar (void *);

__attribute__((noinline, noclone)) void
foo (void *qx, void *rx, void *sx, int n)
{
  unsigned short (*q)[n], (*r)[n], (*s)[n], (*p)[n];
  q = (typeof (q)) qx;
  r = (typeof (r)) rx;
  s = (typeof (s)) sx;
  int t = 1;
  int o = -1;
  #pragma omp for
  for (p = q; p != r; p += t)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p += o)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = p + t)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = p + o)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = t + p)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = o + p)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p += 2)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p -= 2)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = p + 3)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = p - 3)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = 4 + p)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = -5 + p)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
}
