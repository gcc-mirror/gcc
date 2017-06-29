void bar (int);

int a[256];

void
foo (int j)
{
  int i;
  #pragma omp for
  for (i = 0; i != 64; i = i + 4)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i = i - 4)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (i);
  #pragma omp for
  for (i = 0; i != 64; i = j + i)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i = -16 + i)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (i);
  #pragma omp for
  for (i = 0; i != 64; i += j)		/* { dg-error "increment is not constant 1 or -1" } */
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i -= 8)	/* { dg-error "increment is not constant 1 or -1" } */
    bar (i);
  #pragma omp single
  {
    #pragma omp simd
    for (i = 0; i != 64; i = i + 16)	/* { dg-error "increment is not constant 1 or -1" } */
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i = i - 2)	/* { dg-error "increment is not constant 1 or -1" } */
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 0; i != 64; i = j + i)	/* { dg-error "increment is not constant 1 or -1" } */
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i = -j + i)	/* { dg-error "increment is not constant 1 or -1" } */
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 0; i != 64; i += 8)	/* { dg-error "increment is not constant 1 or -1" } */
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i -= j)	/* { dg-error "increment is not constant 1 or -1" } */
      a[i] = a[i] + 1;
  }
}
