// PR c++/79228
// { dg-do compile { target c++14 } }
// { dg-options "" }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }

#include <complex>

int main()
{
  auto a = std::abs(0.0i);	// { dg-error "literal operator" }
  // { dg-message "complex_literals" "" { target *-*-* } .-1 }
}
