// PR c++/80984
// { dg-do compile }

struct A { ~A (); };

A
foo ()
{
  A a;
a:
  return a;
}
