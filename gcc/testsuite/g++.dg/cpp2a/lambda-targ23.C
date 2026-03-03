// PR c++/123408
// { dg-do compile { target c++20 } }

template <typename> constexpr int zero = 0;
template <auto> using int_alias = int;

template <typename Arg>
using inner = int_alias<[](auto) { return 0; }((Arg)0)>;

template <typename Arg>
constexpr int outer = [](auto) { return zero<inner<Arg>>; }(0);

using type = decltype(outer<int>);
using type = const int;
