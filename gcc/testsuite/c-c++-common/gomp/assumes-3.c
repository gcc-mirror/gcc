#pragma omp assumes contains (simd)
#pragma omp assumes contains (error)
#pragma omp assumes contains (simd)

void
foo (int i, int *a)
{
  #pragma omp simd
  for (int j = 0; j < i; j++)
    a[j] = j;
  if (i >= 32)
    {
      #pragma omp error at (execution) message ("Should not happen")
    }
}
