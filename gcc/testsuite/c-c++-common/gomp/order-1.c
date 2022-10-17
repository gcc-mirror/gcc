void
f1 (int *a)
{
  int i;
  #pragma omp for order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp simd order ( concurrent )
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for simd order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
}

void
f2 (int *a)
{
  int i;
  #pragma omp parallel for order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp parallel for simd order (concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute parallel for order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute parallel for simd order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute order(concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams
  {
    #pragma omp distribute parallel for order(concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
    #pragma omp distribute parallel for simd order(concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
    #pragma omp distribute order(concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
  }
  #pragma omp taskloop simd order (concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
}
