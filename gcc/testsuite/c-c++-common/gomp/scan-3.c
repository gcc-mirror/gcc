int a, b;

void
f1 (int *c, int *d)
{
  int i;
  #pragma omp for reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      #pragma omp scan inclusive (a)
      a += c[i];
    }
}
