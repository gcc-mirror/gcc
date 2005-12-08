// PR c++/19317
// { dg-options "-O2" }
// { dg-do run }

extern "C" void abort (void);

struct A { int c; int d; int e; int f; };

A
foo (const A *x, const A *r)
{
  A t;
  t.c = -1;
  t.c += x->c < r->c ? x->c : r->c;
  t.d = 0;
  t.e = 0;
  t.f = 0;
  return t;
}

int
main ()
{
  A a;
  a.c = 10;
  a.d = 0;
  a.e = 0;
  a.f = 0;
  A b;
  b.c = 100;
  b.d = 0;
  b.e = 0;
  b.f = 0;
  a = foo (&b, &a);
  if (a.c != 9)
    abort ();
}
