#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct A
{
  virtual void f () {}
};
struct B
{
  virtual void g () { throw 1; }
};

struct C : public A, public B
{
  void f () {}
  void g () {}
};

__attribute__ ((noipa)) C *
make_c ()
{
  return new C ();
}

void
test ()
{
  C *c = make_c ();
  B *b = c;
  b->g (); // { dg-bogus "leak" }
  delete c;
}
