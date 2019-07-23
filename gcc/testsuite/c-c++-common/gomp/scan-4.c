int a, b;

void
f1 (int *c, int *d)
{
  int i;
  #pragma omp for simd reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan exclusive (a)
      a += c[i];
    }
}
