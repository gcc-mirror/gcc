// PR middle-end/96194
// { dg-do compile }
// { dg-options "-O2" }

#include <new>

struct A { ~A (); };
struct B : A { float e[64]; };

B *
foo ()
{
  return new ((void *) 0) B ();
}

B *
bar (void *x, bool y)
{
  void *p = y ? x : (void *) 0;
  return new (p) B ();
}
