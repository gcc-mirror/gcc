// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Woverflow" }

#include <climits>

constexpr int f() { return INT_MAX; }

int main()
{
  return f() + 2; // { dg-warning "overflow" }
}

