// PR c++/77786
// { dg-do compile { target c++14 } }

#include <vector>

template<int N>
void
foo (std::vector<int> a)
{
  auto const a_size = a.size();
  auto bar = [&](auto y) -> void { int a_size_2 = a_size; };
  double x = 0.0;
  bar (x);
}

int
main ()
{
  std::vector<int> a(1);
  foo<1>(a);
}
