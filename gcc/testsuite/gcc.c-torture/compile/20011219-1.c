/* This testcase failed on IA-64 at -O2 during scheduling.  */

void * baz (unsigned long);
static inline double **
bar (long w, long x, long y, long z)
{
  long i, a = x - w + 1, b = z - y + 1;
  double **m = (double **) baz (sizeof (double *) * (a + 1));

  m += 1;
  m -= w;
  m[w] = (double *) baz (sizeof (double) * (a * b + 1));
  m[w] += 1;
  m[w] -= y;
  for (i = w + 1; i <= x; i++)
    m[i] = m[i - 1] + b;
  return m;
}

void
foo (double w[], int x, double y[], double z[])
{
  int i;
  double **a;

  a = bar (1, 50, 1, 50);
  for (i = 1; i <= x; i++)
    a[1][i] = - w[x - i] / w[x];
}
