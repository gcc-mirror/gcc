// PR c++/113599
// { dg-do run }

struct A { void *a; };
struct B { void *b; };
struct C : public B, public A { A c; };
static C d;

int
main ()
{
  A C::*e = &C::c;
  A *f = &(d.*e);
  A *g = &d.c;
  if (f != g)
    __builtin_abort ();
}
