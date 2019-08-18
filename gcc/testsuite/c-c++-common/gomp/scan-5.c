int
foo (int *a, int *b)
{
  int r = 0;
  #pragma omp parallel for reduction (inscan, +:r) default(none) firstprivate (a, b)
  for (int i = 0; i < 64; i++)
    {
      r += a[i];
      #pragma omp scan inclusive (r)
      b[i] = r;
    }
  return r;
}
