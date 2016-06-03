// PR middle-end/71387
// { dg-do compile }
// { dg-options "-Og" }

struct A
{
  A ();
  inline A (const A &);
};

struct B
{
  explicit B (unsigned long) : b(0), c(1) {}
  A a;
  unsigned long b;
  int c;
};

struct C {};

struct D
{
  explicit D (const C *) {}
};

struct E : public D
{
  E (const C *x) : D(x) {}
  virtual A foo () const = 0;
  virtual A bar () const = 0;
};

struct F : public B
{
  inline void baz ();
  F (const E *);
  const E *f;
};

inline void
F::baz ()
{
  if (b == 0)
    a = f->bar ();
  else
    a = f->foo ();
}

F::F (const E *) : B(4)
{
  baz ();
}
