// { dg-do compile { target c++20 } }

#include <compare>

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

void f(){}
void g(){}

int main()
{
  {
    const auto v = &f <=> &g;	// { dg-error "invalid operands" }
  }

  {
    struct A { int i; int j; };
    constexpr auto v = &A::i <=> &A::j; // { dg-error "invalid operands" }
  }

  {
    struct A { void f(); };
    constexpr auto v = &A::f <=> &A::f; // { dg-error "invalid operands" }
  }
}
