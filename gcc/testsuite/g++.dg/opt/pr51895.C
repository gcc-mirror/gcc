// PR middle-end/51895
// { dg-do compile }
// { dg-options "-O2" }

struct S
{
  long a;
  char b;
  S () : a (0), b (0) {}
  bool baz ();
};

__attribute__((noinline)) static bool
bar (S x, S y)
{
  y = x;
  return y.baz ();
}

bool
foo (S x)
{
  S y;
  return bar (x, y);
}
