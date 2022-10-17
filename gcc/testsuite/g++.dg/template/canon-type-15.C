// { dg-do compile { target c++11 } }
template<unsigned u> struct size_c{ static constexpr unsigned value = u; };
namespace g {
template<class T> auto return_size(T t) -> size_c<sizeof(t)>;
template<class T> auto return_size(T t) -> size_c<sizeof(t)>;
}
static_assert(decltype(g::return_size('a'))::value == 1u, "");
