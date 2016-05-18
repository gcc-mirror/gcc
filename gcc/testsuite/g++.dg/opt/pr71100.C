// PR c++/71100
// { dg-do compile }
// { dg-options "-O2" }

struct D { ~D (); };
struct E { D foo () { throw 1; } };

inline void
bar (D (E::*f) (), E *o)
{
  (o->*f) ();
}

void
baz (E *o)
{
  bar (&E::foo, o);
}
