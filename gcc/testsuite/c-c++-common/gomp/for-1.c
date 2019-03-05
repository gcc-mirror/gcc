void bar (int);

int a[256];

void
foo (void)
{
  int i;
  #pragma omp for
  for (i = 0; i != 64; i++)
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i--)
    bar (i);
  #pragma omp for
  for (i = 0; i != 64; i = i + 1)
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i = i - 1)
    bar (i);
  #pragma omp for
  for (i = 0; i != 64; i = 1 + i)
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i = -1 + i)
    bar (i);
  #pragma omp for
  for (i = 0; i != 64; i += 1)
    bar (i);
  #pragma omp for
  for (i = 128; i != 64; i -= 1)
    bar (i);
  #pragma omp single
  {
    #pragma omp simd
    for (i = 0; i != 64; i++)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i--)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 0; i != 64; i = i + 1)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i = i - 1)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 0; i != 64; i = 1 + i)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i = -1 + i)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 0; i != 64; i += 1)
      a[i] = a[i] + 1;
    #pragma omp simd
    for (i = 128; i != 64; i -= 1)
      a[i] = a[i] + 1;
  }
}
