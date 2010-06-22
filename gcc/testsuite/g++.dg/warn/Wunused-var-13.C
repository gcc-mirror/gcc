// PR c++/44619
// { dg-do compile }
// { dg-options "-Wunused -W" }

struct S { int x, y; };

int
f1 ()
{
  struct S p;
  int S::*q = &S::x;
  p.*q = 5;
  return p.*q;
}

int
f2 (struct S *p, int S::*q)
{
  struct S *r = p;
  int S::*s = q;
  return r->*s;
}
