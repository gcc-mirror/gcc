#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct B1 { int x; };
struct B2 { int y; };
struct MultiDerived : B1, B2 {};

void test_nonfirst_base_field ()
{
  MultiDerived d;
  d.y = 20;
  B2 *p = &d;
  __analyzer_eval (p->y == 20);   // { dg-warning "TRUE" }
  __analyzer_eval (p->y == d.y);  // { dg-warning "TRUE" }
}
