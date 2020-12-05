// PR c++/94480
// { dg-do compile { target c++20 } }

template<typename T, typename U>
constexpr bool is_same_v = __is_same (T, U);

static_assert(is_same_v<bool, decltype(requires { requires false; })>);
// { dg-bogus "evaluated to 'false" "" { target *-*-* } .-1 }
