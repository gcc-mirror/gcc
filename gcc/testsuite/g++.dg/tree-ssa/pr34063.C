// { PR tree-optimization/34063 }
// { dg-do compile }
// { dg-options "-O2" }

struct S
{
  double e[9];

  double const &
  operator() (int r, int c) const
  {
    return e[r * 3 + c];
  }
};

void
foo()
{
  S r;
  double *p;
  for (int j = 0; j < 3; j++)
    for (int k = 0; k < 3; k++)
      for (int l = k + 1; l < 3; l++)
        *p++ = r (k, 0) * r (l, j) + r (k, j) * r (l, 0);
}
