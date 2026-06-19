// { dg-additional-options "-std=c++11" }

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct Base {};
struct Derived : Base {};

void test_nullptr ()
{
  try {
    throw nullptr;
  }
  catch (Base *) {
    __analyzer_dump_path (); // { dg-message "path" }
  }
}
