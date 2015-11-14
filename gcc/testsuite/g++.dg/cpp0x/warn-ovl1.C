// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Woverflow" }

#include <climits>

constexpr int f() { return INT_MIN; }

int main()
{
  return -f(); // { dg-warning "overflow" }
}

