// { dg-do compile { target c++11 } }
// { dg-options "-Wno-pedantic" }
template<unsigned u> struct size_c{ static constexpr unsigned value = u; };
template<class T> auto get_align(T t) -> size_c<alignof(t)>;
template<class T> auto get_align(T t) -> size_c<alignof(t)>;
static_assert(decltype(get_align('a'))::value == 1u, "");
