#include "../../gcc.dg/analyzer/analyzer-decls.h"
// { dg-additional-options "-std=c++11" }

struct Base
{
  virtual void __analyzer_foo () = 0;
};

struct Derived : public Base
{
  void __analyzer_foo () noexcept override {}
};

__attribute__ ((noipa)) Base *
make_derived ()
{
  return new Derived ();
}

void
test ()
{
  Base *f = make_derived ();
  f->__analyzer_foo (); // { dg-bogus "leak" }
  delete f;
}
