void
foo (int *p)
{
  int a = -1, b = -1, c = -1, d = -1, e = -1, f = -1, g = -1, h = -1;
  int i;
  #pragma omp parallel
  #pragma omp for lastprivate (conditional: a)
  for (i = 0; i < 32; i++)
    if (p[i])
      a = i;
  #pragma omp simd lastprivate (conditional: b)
  for (i = 0; i < 32; i++)
    if (p[i])
      b = i;
  #pragma omp parallel
  #pragma omp for simd lastprivate (conditional: c)
  for (i = 0; i < 32; i++)
    if (p[i])
      c = i;
  #pragma omp parallel for lastprivate (conditional: d)
  for (i = 0; i < 32; i++)
    if (p[i])
      d = i;
  #pragma omp parallel for simd lastprivate (conditional: e)
  for (i = 0; i < 32; i++)
    if (p[i])
      e = i;
}
