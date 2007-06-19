// PR tree-optimization/32353
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();

struct A
{
  int f;
  A (int x) : f (x) {}
};

A
foo (const A &x, const A &y)
{
  A r (0);
  r = x.f == -111 ? y : (y.f == -111 || x.f > y.f) ? x : y;
  A s (0);
  r = r.f == -111 ? s : (r.f > s.f) ? r : s;
  return r;
}

int
main ()
{
  if (foo (A (0), A (1)).f != 1)
    abort ();
  if (foo (A (1), A (9)).f != 9)
    abort ();
  if (foo (A (9), A (1)).f != 9)
    abort ();
  if (foo (A (-4), A (-5)).f != 0)
    abort ();
  if (foo (A (-111), A (-111)).f != 0)
    abort ();
  if (foo (A (2), A (-111)).f != 2)
    abort ();
  if (foo (A (-111), A (6)).f != 6)
    abort ();
  if (foo (A (-111), A (-4)).f != 0)
    abort ();
}
