// PR c++/19317
// { dg-options "-O2" }
// { dg-do run }
// Origin: Dirk Mueller <mueller@kde.org>

extern "C" void abort (void);

struct A
{
  A () { d = e = 0; f = -1; }
  A (int x) : d (0), e (0), f (x) { }
  A b (const A &r) const;
  int d;
  int e;
  int f;
};

A
A::b (const A & r) const
{
  A t;
  t.f = f < r.f ? f : r.f;
  return t;
}

int
main ()
{
  A a (100);
  a = a.b (A (10));
  if (a.f != 10)
    abort ();

  A c (10);
  A d (100);
  c = d.b (c);
  if (c.f != 10)
    abort ();
}
