// PR c++/28109
// { dg-do compile }

#include <typeinfo>

struct A;

void foo()
{
  A a;  // { dg-error "incomplete type" }
  typeid (a);
}
