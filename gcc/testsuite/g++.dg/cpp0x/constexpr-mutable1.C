// { dg-options -std=c++11 }

struct A
{
  int i;
  mutable int j;
};

constexpr A a = { 0, 1 };
constexpr A b = a;		// { dg-error "mutable" }
constexpr int i = a.i;
constexpr int j = a.j;		// { dg-error "mutable" }
