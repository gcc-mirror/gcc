// { dg-do compile { target c++17 } }

#include <typeinfo>

struct S { int m; static int s; } s;

template<int* p> class X { };
template<const char *s> class Y {};
template<const std::type_info &> class Z {};

X<&s.m> x7; // { dg-error "3:.& s.S::m. is not a valid template argument" "" { target c++17_down } }
Y<"foo"> y1;			// { dg-error "string literal" }
Z<typeid(p)> z1;		// { dg-error "" }

void f()
{
  Y<__func__> y;		// { dg-error "" }
}
