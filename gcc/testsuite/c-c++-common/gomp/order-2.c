void
f1 (int *a)
{
  int i;
  #pragma omp for order				/* { dg-error "expected" } */
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for simd order :			/* { dg-error "expected" } */
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp simd order ( foobar )		/* { dg-error "expected" } */
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for simd order( concurrent	/* { dg-error "expected" } */
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for simd order( concurrent : foo )/* { dg-error "expected" } */
  for (i = 0; i < 128; i++)
    a[i]++;
}

void
f2 (int *a)
{
  int i;
  #pragma omp teams
  #pragma omp distribute order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp taskloop order (concurrent)	/* { dg-error "'order' is not valid for '#pragma omp taskloop'" } */
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for order(concurrent) ordered	/* { dg-error "'order' clause must not be used together with 'ordered'" } */
  for (i = 0; i < 128; i++)
    {
      #pragma omp ordered
      a[i]++;
    }
  #pragma omp for ordered order(concurrent)	/* { dg-error "'order' clause must not be used together with 'ordered'" } */
  for (i = 0; i < 128; i++)
    {
      #pragma omp ordered
      a[i]++;
    }
  #pragma omp for ordered (1) order(concurrent)	/* { dg-error "'order' clause must not be used together with 'ordered'" } */
  for (i = 0; i < 128; i++)
    {
      #pragma omp ordered depend (sink: i - 1)
      #pragma omp ordered depend (source)
    }
  #pragma omp for order(concurrent)ordered (1)	/* { dg-error "'order' clause must not be used together with 'ordered'" } */
  for (i = 0; i < 128; i++)
    {
      #pragma omp ordered depend (sink: i - 1)
      #pragma omp ordered depend (source)
    }
}
