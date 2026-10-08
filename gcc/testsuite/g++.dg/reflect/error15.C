// PR c++/125631
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <tuple>

template<auto Templ, class Tuple>
const auto
make_from_tuple (Tuple&& t)
{
  return [:Templ:]{}; // { dg-error "expected|cannot use template .std::tuple. in a splice expression" }
}
auto p = make_from_tuple<^^std::tuple>(std::tuple());

template<auto N> int tv;
template<auto N> concept C = requires { true; };
template<typename T> struct S { };
template<typename T>
using A = S<T>;
template<typename> using I = int;

auto a1 = [:^^tv:];   // { dg-error "cannot use template .tv<N>. in a splice expression" }
auto a2 = [:^^C:];    // { dg-error "cannot use template .C. in a splice expression" }
auto a3 = [:^^S:];    // { dg-error "cannot use template .S. in a splice expression" }
auto a4 = [:^^A:];    // { dg-error "cannot use template .A. in a splice expression" }
auto a5 = [:^^I:];    // { dg-error "cannot use template .I. in a splice expression" }
auto a6 = [:^^C:]<int>;	// { dg-error "expected a reflection of a function or variable template" }
auto a7 = template [:^^C:]<int>;    // { dg-error "expected a reflection of a function or variable template" }

template<auto R>
void
g1 ()
{
  auto a = [:R:];  // { dg-error "cannot use template .tv<N>. in a splice expression" }
}

template<auto R>
void
g2 ()
{
  auto a = [:R:];  // { dg-error "cannot use template .C. in a splice expression" }
}

template<auto R>
void
g3 ()
{
  auto a = [:R:];  // { dg-error "cannot use template .S. in a splice expression" }
}

template<auto R>
void
g4 ()
{
  auto a = [:R:];  // { dg-error "cannot use template .A. in a splice expression" }
}

template<auto R>
void
g5 ()
{
  auto a = [:R:];  // { dg-error "cannot use template .I. in a splice expression" }
}

template<template<class> class TT>
void
g6 ()
{
  auto x = [:^^TT:];  // { dg-error "cannot use template .S. in a splice expression" }
}

template<auto R>
void
g7 ()
{
  &[:R:];  // { dg-error "cannot use template .S. in a splice expression" }
}

template<auto R> auto g8 () -> decltype([:R:]);	// { dg-error "cannot use template .tv<N>. in a splice expression" }

template<auto R>
void
g9 ()
{
  auto a = template [:R:]<int>;  // { dg-error "expected a reflection of a function or variable template" }
}

void
h ()
{
  g1<^^tv>();
  g2<^^C>();
  g3<^^S>();
  g4<^^A>();
  g5<^^I>();
  g6<S>();
  g7<^^S>();
  g8<^^tv>(); // { dg-error "no matching function" }
  g9<^^C>();
}
