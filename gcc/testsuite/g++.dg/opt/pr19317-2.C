// PR c++/19317
// { dg-options "-O2" }
// { dg-do run }

extern "C" void abort (void);

struct A
{
  A () { d = e = 0; f = -1; }
  A (int x) : d (0), e (0), f (x) { }
  A b () const;
  int d;
  int e;
  int f;
};

A
A::b () const
{
  A t;
  t.f = 10 + this->f;
  return t;
}

int
main ()
{
  A a (100);
  a = a.b ();
  if (a.f != 110)
    abort ();
}
