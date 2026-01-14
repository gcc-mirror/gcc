// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template <auto V> constexpr int e = [:V:];  // { dg-error "expected a reflection of an expression instead of type .int." }
template <auto V> constexpr int e2 = [:V:]; // { dg-error "expected a reflection of an expression instead of type .int." }
constexpr auto h = ^^int;
constexpr auto i = e<([:^^h:])>;
constexpr auto j = e2<^^int>;
