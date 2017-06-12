// { dg-do compile { target c++11 } }

template <int> struct E { };

struct E1: E<0>, E<1> { };
struct E2: E<2>, E<3> { };

struct A1x { int n; };
struct D2: A1x, E1, E2 { };

#define SA(X) static_assert((X),#X)
SA(__is_standard_layout (D2));
