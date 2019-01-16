void bar (int *);

void
foo (int *a)
{
  int i, j, k, u = 0, v = 0, w = 0, x = 0, y = 0, z = 0;
  #pragma omp parallel master default(none) private (k)
  bar (&k);
  #pragma omp parallel default(none) firstprivate(a) shared(x, y, z)
  {
    #pragma omp master taskloop reduction (+:x) default(none) firstprivate(a)
    for (i = 0; i < 64; i++)
      x += a[i];
    #pragma omp master taskloop simd reduction (+:y) default(none) firstprivate(a) private (i)
    for (i = 0; i < 64; i++)
      y += a[i];
    #pragma omp master taskloop simd collapse(2) reduction (+:z) default(none) firstprivate(a) private (i, j)
    for (j = 0; j < 1; j++)
      for (i = 0; i < 64; ++i)
	z += a[i];
  }
  #pragma omp parallel master taskloop reduction (+:u) default(none) firstprivate(a)
  for (i = 0; i < 64; i++)
    u += a[i];
  #pragma omp parallel master taskloop simd reduction (+:v) default(none) firstprivate(a)
  for (i = 0; i < 64; i++)
    v += a[i];
  #pragma omp parallel master taskloop simd collapse(2) reduction (+:w) default(none) firstprivate(a)
  for (j = 0; j < 1; j++)
    for (i = 0; i < 64; ++i)
      w += a[i];
}
