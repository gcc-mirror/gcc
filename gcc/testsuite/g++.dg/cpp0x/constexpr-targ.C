// { dg-options -std=c++11 }

struct A
{
  constexpr operator double() { return 1.0; }
};

template <int I>
struct B
{ };

constexpr A a { };
B<a> b;				// { dg-error "template argument|invalid type" }
