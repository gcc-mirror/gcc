void bar (short *);

void
foo (short *q, short *r, short *s)
{
  short *p;
  #pragma omp for
  for (p = q; p != r; p++)
    bar (p);
  #pragma omp for
  for (p = s; p != r; p--)
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = p + 1)
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = p - 1)
    bar (p);
  #pragma omp for
  for (p = q; p != r; p = 1 + p)
    bar (p);
  #pragma omp for
  for (p = s; p != r; p = -1 + p)
    bar (p);
  #pragma omp for
  for (p = q; p != r; p += 1)
    bar (p);
  #pragma omp for
  for (p = s; p != r; p -= 1)
    bar (p);
}
