// PR tree-optimization/124135
// { dg-do compile }

void foo (char, long double *, int);
int qux (int);

long double
bar (int x)
{
  long double a;
  if (x)
    foo (0, &a, qux (42));
  return a;
}

void
baz (int x)
{
  bar (x);
}
