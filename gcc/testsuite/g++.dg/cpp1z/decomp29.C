// { dg-options "-std=c++17 -Wunused" }

#include <tuple>

struct A { int i,j,k; };

A f();

int z;

int main()
{
  {
    auto [i,j,k] = f();		// { dg-warning "unused" }
  }
  {
    auto [i,j,k] = f();
    z = i;
  }
  {
    auto [i,j] = std::tuple{1,2}; // { dg-warning "unused" }
  }
  // No parallel second test, because in this case i and j are variables rather
  // than mere bindings, so there isn't a link between them and using i will
  // not prevent a warning about unused j.
}
