// PR c++/79205
// { dg-do compile { target c++11 } }
// { dg-options "" }

#include <tuple>

int
foo (std::tuple<int> t)
{
  auto [x0] = t;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  return x0;
}
