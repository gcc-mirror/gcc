#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct Base
{
  virtual void __analyzer_foo () { throw 1; }
};

extern Base *get_base (void); // unknown dynamic type

void
test ()
{
  Base *f = get_base ();
  // vptr unknown (won't devirt)
  f->__analyzer_foo (); // { dg-bogus "leak" }
}
