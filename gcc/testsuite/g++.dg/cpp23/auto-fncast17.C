// PR c++/110025
// { dg-do compile { target c++23 } }

template<class...> struct tuple;

template<auto V>
using constant_t = int;

template<auto... V>
using constants_t = tuple<constant_t<auto(V)>...>;

using ty0 = constants_t<>;
using ty1 = constants_t<1>;
using ty2 = constants_t<1, 2>;
using ty3 = constants_t<1, 2, 3>;
