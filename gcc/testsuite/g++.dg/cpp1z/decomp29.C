// { dg-do compile { target c++17 } }
// { dg-options "-Wunused" }

#include <tuple>

struct A { int i,j,k; };

A f();
int p[3];

int z;

int main()
{
  {
    auto [i,j,k] = f();		// { dg-warning "unused" }
  }
  {
    [[maybe_unused]] auto [i,j,k] = f();
  }
  {
    auto [i,j,k] = f();
    z = i;
  }
  {
    auto [i,j,k] = f();		// { dg-warning "unused" }
    i = 5;
  }
  {
    auto [i,j] = std::tuple{1,2}; // { dg-warning "unused" }
  }
  {
    [[maybe_unused]] auto [i,j] = std::tuple{1,2};
  }
  {
    auto [i,j] = std::tuple{1,2};
    z = i;
  }
  {
    auto [i,j] = std::tuple{1,2};
    i = 5;
  }
  {
    auto [i,j,k] = p;		// { dg-warning "unused" }
  }
  {
    [[maybe_unused]] auto [i,j,k] = p;
  }
  {
    auto [i,j,k] = p;
    z = i;
  }
  {
    auto [i,j,k] = p;		// { dg-warning "unused" }
    i = 5;
  }
}
