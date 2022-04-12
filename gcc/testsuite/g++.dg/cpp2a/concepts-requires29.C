// PR c++/103105
// { dg-do compile { target c++20 } }

template<bool> struct A;

template<class... Ts>
using wrap = A<1 != (0 + ... + requires { Ts(); })>;

template<class... Ts> using type = wrap<Ts...>;

using ty0 = type<>;
using ty0 = A<true>;

using ty1 = type<int>;
using ty1 = A<false>;

using ty2 = type<int, int>;
using ty2 = A<true>;
