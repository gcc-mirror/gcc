// PR c++/107532
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

#include <tuple>

int main()
{
  int i = 42;
  auto const& v = std::get<0>(std::tuple<int&>(i)); // { dg-bogus "dangling reference" }
  (void) v;
}
