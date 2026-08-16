/* /10: a failed cast to reference type throws std::bad_cast.  */

#include <typeinfo>
#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct A { virtual ~A () {} };
struct B : A { int m; };

void test_ref_success () {
  B obj;
  obj.m = 3;
  A &a = obj;
  B &b = dynamic_cast<B &> (a);
  __analyzer_eval (b.m == 3);	/* { dg-warning "TRUE" } */
}

void test_ref_failure () {
  A obj;
  A &a = obj;
  try
    {
      B &b = dynamic_cast<B &> (a);
      __analyzer_dump_path ();	/* { dg-bogus "path" } */
      (void) b;
    }
  catch (std::bad_cast &)
    {
      __analyzer_dump_path ();	/* { dg-message "path" } */
    }
}
