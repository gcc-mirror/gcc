int v;
extern void foo (int);

void
bar (void)
{
  int i;
  #pragma omp for reduction (task, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp sections reduction (task, +: v)
  {
    foo (-2);
    #pragma omp section
    foo (-3);
  }
  #pragma omp parallel reduction (task, +: v)
  foo (-1);
  #pragma omp parallel for reduction (task, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp parallel sections reduction (task, +: v)
  {
    foo (-2);
    #pragma omp section
    foo (-3);
  }
  #pragma omp teams distribute parallel for reduction (task, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp for reduction (default, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp sections reduction (default, +: v)
  {
    foo (-2);
    #pragma omp section
    foo (-3);
  }
  #pragma omp parallel reduction (default, +: v)
  foo (-1);
  #pragma omp parallel for reduction (default, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp parallel sections reduction (default, +: v)
  {
    foo (-2);
    #pragma omp section
    foo (-3);
  }
  #pragma omp teams distribute parallel for reduction (default, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp for reduction (default, +: v) nowait
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp sections nowait reduction (default, +: v)
  {
    foo (-2);
    #pragma omp section
    foo (-3);
  }
  #pragma omp simd reduction (default, +: v)
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp for simd reduction (default, +: v)
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp parallel for simd reduction (default, +: v)
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp teams distribute parallel for simd reduction (default, +: v)
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp taskloop reduction (default, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
  #pragma omp taskloop simd reduction (default, +: v)
  for (i = 0; i < 64; i++)
    v++;
  #pragma omp teams reduction (default, +: v)
  foo (i);
  #pragma omp teams distribute reduction (default, +: v)
  for (i = 0; i < 64; i++)
    foo (i);
}
