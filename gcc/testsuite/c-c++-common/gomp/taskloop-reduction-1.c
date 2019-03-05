int
foo (int *a)
{
  int x = 0;
  #pragma omp taskloop reduction (+:x) nogroup	/* { dg-error "'nogroup' clause must not be used together with 'reduction' clause" } */
  for (int i = 0; i < 64; i++)
    x += a[i];
  #pragma omp taskwait
  return x;
}
