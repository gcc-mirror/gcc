// PR c++/58614
// { dg-do compile { target c++11 } }

#include <initializer_list>

void foo()
{
  i;  // { dg-error "not declared" }
  auto j = { i };  // { dg-error "unable to deduce" }
}
