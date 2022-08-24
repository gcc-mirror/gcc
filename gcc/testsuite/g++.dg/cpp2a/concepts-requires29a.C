// PR c++/103105
// { dg-do compile { target c++20 } }

template<class...> struct list;

template<bool> struct A;

template<class T, class... Ts>
using wrap = A<1 != (0 + ... + requires { T() = Ts(); })>;

template<class... Ts> using type = list<wrap<Ts, Ts...>...>;

using ty0 = type<>;
using ty0 = list<>;

using ty1 = type<int>;
using ty1 = list<A<true>>;

using ty2 = type<int, int>;
using ty2 = list<A<true>, A<true>>;

using ty3 = type<int, int, int>;
using ty3 = list<A<true>, A<true>, A<true>>;
