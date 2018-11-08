int a[64];

#pragma omp declare simd linear(x)
int
bar (int x, int y)
{
  int v;
  #pragma omp atomic capture
  v = a[x] += y;
  return v;
}

void
foo (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < 64; i++)
    #pragma omp atomic
    a[i] += 1;
}
