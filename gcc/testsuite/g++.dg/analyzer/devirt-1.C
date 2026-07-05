#include "../../gcc.dg/analyzer/analyzer-decls.h"
struct Base
{
  virtual int not_overridden () { return 0; }
  virtual int overridden () { return 0; }
};
struct Derived : public Base
{
  int overridden () { return 60; }
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
  __analyzer_eval (f->not_overridden () == 0); // { dg-warning "TRUE" }
  __analyzer_eval (f->overridden () == 60);    // { dg-warning "TRUE" }
  delete f;
}
