// PR tree-optimization/33458
// { dg-do compile }
// { dg-options "-O" }

inline void
foo (int *p, int n)
{
  for (; n > 0; --n, ++p)
    *p = 0;
}

struct A
{
  int x[2];
  A () { foo (x, 2); }
};

inline A
getA ()
{
  return A ();
}

struct B
{
  A a;
  B ();
};

B::B () : a (getA ())
{
}
