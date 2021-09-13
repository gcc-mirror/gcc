// { dg-do compile { target c++17 } }

template <class...> struct A {};

template <template <class> class... Ts>
using B = A<decltype(Ts{0})...>;
