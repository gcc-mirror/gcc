// { dg-do compile { target c++11 } }

void
foo ()
{
  int a[10] = {};
  #pragma omp parallel sections
  {
    #pragma omp section
    a[0]++;
    [[omp::directive (section)]] {
    a[1]++;
    } [[omp::directive (section)]]
    a[2]++;
    #pragma omp section
    { a[3]++; }
  }
  [[omp::directive (parallel sections)]]
  {
    #pragma omp section
    a[0]++;
    [[omp::directive (section)]] {
    a[1]++;
    } [[omp::directive (section)]]
    a[2]++;
    #pragma omp section
    { a[3]++; }
  }
}

int
bar (int a, int *c, int *d, int *e, int *f)
{
  int i;
  #pragma omp simd reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      d[i] = a;
      [[omp::directive (scan, exclusive (a))]]
      a += c[i];
    }
  [[omp::directive (simd reduction (inscan, +: a))]]
  for (i = 0; i < 64; i++)
    {
      a += c[i];
      #pragma omp scan inclusive (a)
      d[i] = a;
    }
  return a;
}
