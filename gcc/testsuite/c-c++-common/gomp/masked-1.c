void bar (void);

void
foo (int x, int *a)
{
  #pragma omp masked
  bar ();
  #pragma omp masked filter (0)
  bar ();
  #pragma omp masked filter (7)
  bar ();
  #pragma omp masked filter (x)
  bar ();
  #pragma omp masked taskloop simd filter (x) grainsize (12) simdlen (4)
  for (int i = 0; i < 128; i++)
    a[i] = i;
  #pragma omp parallel masked filter (x) firstprivate (x)
  bar ();
  #pragma omp masked
  #pragma omp masked filter (0)
  #pragma omp masked filter (x)
  ;
}
