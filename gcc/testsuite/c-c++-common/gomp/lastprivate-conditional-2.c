void
foo (int *p)
{
  int a = -1, b = -1, c = -1, d = -1, e = -1, f = -1, g = -1, h = -1;
  int i;
  #pragma omp parallel
  #pragma omp for lastprivate (conditional: a) /* { dg-message "not supported yet" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      a = i;
  #pragma omp simd lastprivate (conditional: b) /* { dg-message "not supported yet" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      b = i;
  #pragma omp parallel
  #pragma omp for simd lastprivate (conditional: c) /* { dg-message "not supported yet" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      c = i;
  #pragma omp parallel for lastprivate (conditional: d) /* { dg-message "not supported yet" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      d = i;
  #pragma omp parallel for simd lastprivate (conditional: e) /* { dg-message "not supported yet" } */
  for (i = 0; i < 32; i++)
    if (p[i])
      e = i;
}
