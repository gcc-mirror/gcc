// PR c++/79228
// { dg-do compile { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }

#include <complex>

int main()
{
  using namespace std::complex_literals;
  auto a = std::abs(0.0i);
}
