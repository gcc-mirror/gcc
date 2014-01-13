// PR c++/58856
// { dg-require-effective-target c++11 }

template <typename T>
struct U1 {};

template <typename T1, typename... Ts>
using U2 = U1<T1>;

template <typename T1, typename... Ts>
using U3 = U2<T1, Ts...>;
