// PR c++/60628
// { dg-do compile { target c++11 } }

#include <initializer_list>

void foo(int i)
{
  auto x[1] = { 0 };		// { dg-error "array of .auto" }
}
