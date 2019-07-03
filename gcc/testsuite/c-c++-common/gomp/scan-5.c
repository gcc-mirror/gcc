int
foo (int *a, int *b)
{
  int r = 0;
  #pragma omp parallel for reduction (inscan, +:r) default(none) firstprivate (a, b)
  for (int i = 0; i < 64; i++)
    {
      r += a[i];
      #pragma omp scan inclusive (r)	/* { dg-message "sorry, unimplemented: '#pragma omp scan' not supported yet" } */
      b[i] = r;
    }
  return r;
}
