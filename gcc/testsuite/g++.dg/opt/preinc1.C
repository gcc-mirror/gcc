// PR optimization/6086
// { dg-do run }
// { dg-options "-O" }
 
extern "C" void abort (void);

struct A
{
  A (int x, int y);
  int a, b;
  int foo () { return a; }
  int bar () { return b; }
};

struct B
{
  virtual ~B ();
  virtual A baz () const;
};

struct C
{
  A foo () const;
  B *c;
};

A C::foo () const
{
  int x, y;
  x = c->baz ().foo ();
  y = c->baz ().bar ();
  return A (x, y);
}

A B::baz () const
{
  return A (4, 8);
}

A::A (int x, int y)
{
  a = x;
  b = y;
}

B::~B ()
{
}

int
main ()
{
  C the_c;
  B the_b;
  the_c.c = &the_b;
  if (the_c.foo().a != 4)
    abort ();
  return 0;
}
