void
f1 (int *a)
{
  int i;
  #pragma omp for order(reproducible:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp simd order ( reproducible : concurrent )
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for simd order(reproducible :concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
}

void
f2 (int *a)
{
  int i;
  #pragma omp parallel for order(reproducible: concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp parallel for simd order (reproducible:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute parallel for order(reproducible:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute parallel for simd order(reproducible:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute order(reproducible:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams
  {
    #pragma omp distribute parallel for order(reproducible:concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
    #pragma omp distribute parallel for simd order(reproducible:concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
    #pragma omp distribute order(reproducible:concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
  }
  #pragma omp taskloop simd order (reproducible:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
}

void
f3 (int *a)
{
  int i;
  #pragma omp for order(unconstrained:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp simd order ( unconstrained : concurrent )
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp for simd order(unconstrained :concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
}

void
f4 (int *a)
{
  int i;
  #pragma omp parallel for order(unconstrained: concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp parallel for simd order (unconstrained:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute parallel for order(unconstrained:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute parallel for simd order(unconstrained:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams distribute order(unconstrained:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
  #pragma omp teams
  {
    #pragma omp distribute parallel for order(unconstrained:concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
    #pragma omp distribute parallel for simd order(unconstrained:concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
    #pragma omp distribute order(unconstrained:concurrent)
    for (i = 0; i < 128; i++)
      a[i]++;
  }
  #pragma omp taskloop simd order (unconstrained:concurrent)
  for (i = 0; i < 128; i++)
    a[i]++;
}
