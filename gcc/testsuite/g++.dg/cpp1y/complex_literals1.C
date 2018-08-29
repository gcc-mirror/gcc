// PR c++/79228
// { dg-do compile { target c++14 } }

#include <complex>

int main()
{
  using namespace std::complex_literals;
  auto a = std::abs(0.0i);
}
