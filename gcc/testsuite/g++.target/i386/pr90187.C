// PR target/90187
// { dg-do compile }
// { dg-options "-Ofast -ffloat-store" }

double a[64];
double *foo (void);

void
bar (int x, const double *y)
{
  int i;
  for (i = 0; i < x; i++)
    if (y[i] < a[i])
      a[i] = y[i];
}
