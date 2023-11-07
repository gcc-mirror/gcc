/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c23" } */

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
  #pragma omp parallel sections
  {
    #pragma omp section
    a[0]++;
    a[4]++;
    l1: a[5]++;
    if (a[5] == 42) goto l1;
    [[omp::directive (section)]] {
    a[1]++;
    a[6]++;
    } [[omp::directive (section)]]
    a[2]++;
    a[7]++;
    #pragma omp section
    { a[3]++; }
    a[8]++;
  }
  [[omp::directive (parallel sections)]]
  {
    #pragma omp section
    a[0]++;
    a[4]++;
    [[omp::directive (section)]] {
    a[1]++;
    a[5]++;
    } [[omp::directive (section)]]
    a[2]++;
    l2: a[6]++;
    if (a[6] == 42)
      goto l2;
    a[7]++;
    #pragma omp section
    a[8]++;
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
  #pragma omp simd reduction (inscan, +: a)
  for (i = 0; i < 64; i++)
    {
      { int t = a;
	d[i] = t; }
      [[omp::directive (scan, exclusive (a))]]
      { int u = c[i];
	a += u; }
    }
  [[omp::directive (simd reduction (inscan, +: a))]]
  for (i = 0; i < 64; i++)
    {
      { int t = c[i];
	a += t; }
      #pragma omp scan inclusive (a)
      { int u = a;
	d[i] = u; }
    }
  return a;
}
