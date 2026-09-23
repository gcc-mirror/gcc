// PR c++/126546
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template <class... Ts>
consteval auto first() { return ^^Ts...[0]; }

static_assert(first<int, double>() == ^^int);
