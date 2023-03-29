// { dg-do compile { target c++11 } }

[[omp::directive (assumes contains (simd))]];
[[omp::directive (assumes contains (error))]];
[[omp::directive (assumes, contains (simd))]];

void
foo (int i, int *a)
{
  [[omp::directive (simd)]]
  for (int j = 0; j < i; j++)
    a[j] = j;
  if (i >= 32)
    {
      [[omp::directive (error at (execution) message ("Should not happen"))]];
    }
}
