void bar (short *);

void
foo (short *q, short *r, short *s, long t)
{
  short *p;
  #pragma omp for
  for (p = q; p != r; p = p + 5)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = p - 2)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = t + p)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = -t + p)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = q; p != r; p += t)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
  #pragma omp for
  for (p = s; p != r; p -= 7)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (p);
}
