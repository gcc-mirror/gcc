// { dg-do compile }
// { dg-options "-O2 -fno-tree-fre" }

double m;
int n;

void
foo (void)
{
  static double a[] = { 0.0 / 0.0, 0.0 };
  int i;

  for (i = 0; i < n; ++i)
    if (m >= a[i])
      __builtin_abort ();
}
