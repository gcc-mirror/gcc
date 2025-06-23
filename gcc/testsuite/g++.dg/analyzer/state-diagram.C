#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct foo
{
  virtual ~foo () {}

  foo (int i) : m_i (i) {}

  int m_i;
};

void test ()
{
  foo *f = new foo (42);
} // { dg-warning "leak of 'f'" }
