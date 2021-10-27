int *qux (int *);

void
foo (void)
{
  int a[1024];
  int *p;
  short *q;
  __PTRDIFF_TYPE__ r;
  #pragma omp parallel for collapse(2)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = (short *) p + 64; q < (short *) p + 128; q++)	/* { dg-error "outer iteration variable 'p' used in initializer expression has type other than 'short int ?\\\*'" } */
      ;
  #pragma omp parallel for collapse(2)
  for (p = &a[0]; p < &a[512]; p++)
    for (r = &a[32] - p; r < 32; r++)				/* { dg-error "initializer expression refers to iteration variable 'p'" } */
      ;
  #pragma omp parallel for collapse(2)
  for (r = 0; r < 64; r++)
    for (p = &a[0] + r; p < &a[32] + 3 * r; p++)		/* { dg-error "initializer expression refers to iteration variable 'r'" } */
      ;
}

void
bar (void)
{
  int a[1024];
  int *p, *q, *r;
  #pragma omp parallel for collapse(2)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = p + (&a[16] - qux (p)); q < &a[32]; q++)		/* { dg-error "initializer expression refers to iteration variable 'p'" } */
      ;
  #pragma omp parallel for collapse(3)
  for (p = &a[0]; p < &a[512]; p++)
    for (q = &a[0]; q < &a[512]; q++)
      for (r = p; r < q + 32; r++)				/* { dg-error "two different outer iteration variables 'p' and 'q' used in a single loop" } */
	;
}
