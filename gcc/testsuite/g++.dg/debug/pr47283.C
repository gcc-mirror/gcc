// PR debug/47283
// { dg-do compile }

template <typename T> inline const T &
f1 (const T &a, const T &b)
{
  if (a < b)
    return b;
  return a;
};

struct A
{
  A (int w, int h) { a1 = w; }
  A f2 (const A &) const;
  int a1, a2;
};

inline A
A::f2 (const A &x) const
{
  return A (f1 (a1, x.a1), f1 (a2, x.a2));
};

struct B
{
  A f3 () const;
  void f4 (const A &) { b2 = 5 + b1; }
  int b1, b2;
};

struct C
{
};

struct D
{
  virtual C f5 (const C &) const;
};

struct E
{
  C f6 () const;
  int f7 () const;
  virtual B f8 (const C &) const;
  A f9 () const;
  virtual void f10 ();
  struct F { D *h; } *d;
};

void
E::f10 ()
{
  const C c = d->h->f5 (f6 ());
  B b = f8 (c);
  b.f4 (b.f3 ().f2 (f9 ()));
  f7 ();
}
