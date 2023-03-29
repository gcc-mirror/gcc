// { dg-do compile { target c++17 } }
// { dg-options "-Wdangling-reference" }
// Test -Wdangling-reference with std::minmax.

#include <algorithm>

using U = std::pair<const int&, const int&>;

int
fn1 ()
{
  std::pair<const int&, const int&> v = std::minmax(1, 2); // { dg-warning "dangling reference" }
  U v2 = std::minmax(1, 2); // { dg-warning "dangling reference" }
  auto v3 = std::minmax(1, 2); // { dg-warning "dangling reference" }
  return v.first + v2.second + v3.first;
}

int
fn2 ()
{
  int n = 1;
  auto p = std::minmax(n, n + 1); // { dg-warning "dangling reference" }
  int m = p.first; // ok
  int x = p.second; // undefined behavior

  // Note that structured bindings have the same issue
  auto [mm, xx] = std::minmax(n, n + 1); // { dg-warning "dangling reference" }
  (void) xx; // undefined behavior

  return m + x;
}

int
fn3 ()
{
  auto v = std::minmax({1, 2});
  return v.first;
}
