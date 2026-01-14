// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S { };
using X = S;

template <auto V> constexpr int e = [:V:];  // { dg-error "expected a reflection of an expression instead of type .X." }
template <auto V> constexpr int e2 = [:V:]; // { dg-error "expected a reflection of an expression instead of type .X." }
constexpr auto h = ^^X;
constexpr auto i = e<([:^^h:])>;
constexpr auto j = e2<^^X>;
