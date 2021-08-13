void bar (int *);

void
foo (int *a, int f)
{
  int i, j, k, u = 0, v = 0, w = 0, x = 0, y = 0, z = 0;
  #pragma omp parallel masked default(none) private (k) filter (f) firstprivate (f)
  bar (&k);
  #pragma omp parallel masked default(none) private (k)
  bar (&k);
  #pragma omp parallel default(none) firstprivate(a, f) shared(x, y, z)
  {
    #pragma omp masked taskloop reduction (+:x) default(none) firstprivate(a) filter (f)
    for (i = 0; i < 64; i++)
      x += a[i];
    #pragma omp masked taskloop simd reduction (+:y) default(none) firstprivate(a) private (i) filter (f)
    for (i = 0; i < 64; i++)
      y += a[i];
    #pragma omp masked taskloop simd reduction (+:y) default(none) firstprivate(a) private (i)
    for (i = 0; i < 64; i++)
      y += a[i];
    #pragma omp masked taskloop simd collapse(2) reduction (+:z) default(none) firstprivate(a) private (i, j) filter (f)
    for (j = 0; j < 1; j++)
      for (i = 0; i < 64; ++i)
	z += a[i];
  }
  #pragma omp parallel masked taskloop reduction (+:u) default(none) firstprivate(a, f) filter (f)
  for (i = 0; i < 64; i++)
    u += a[i];
  #pragma omp parallel masked taskloop simd reduction (+:v) default(none) firstprivate(a, f) filter (f)
  for (i = 0; i < 64; i++)
    v += a[i];
  #pragma omp parallel masked taskloop simd collapse(2) reduction (+:w) default(none) firstprivate(a, f) filter (f)
  for (j = 0; j < 1; j++)
    for (i = 0; i < 64; ++i)
      w += a[i];
}
