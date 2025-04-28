#include "../../gcc.dg/analyzer/analyzer-decls.h"

// { dg-do compile { target c++11 } }

struct s1
{
  int t;
  s1() { t = 42; }
  s1(const s1& other) { t = other.t; }
};

s1 inner()
{
  return s1{}; // { dg-bogus "uninitialized" }
}

s1 middle()
{
  return inner();
}

void outer()
{
  s1 obj = middle();
  __analyzer_eval (obj.t == 42); // { dg-warning "TRUE" }
}
