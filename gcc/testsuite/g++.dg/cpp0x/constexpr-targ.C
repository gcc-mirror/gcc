// { dg-do compile { target c++11 } }

struct A
{
  constexpr operator double() { return 1.0; }
};

template <int I>
struct B
{ };

constexpr A a { };
B<a> b;			 // { dg-error "template argument|converted constant|could not convert" }
