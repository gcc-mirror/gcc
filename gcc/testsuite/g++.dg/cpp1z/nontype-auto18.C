// { dg-do compile { target c++11 } }

template <int> struct K { };

struct S { using type = int; };
template <class T = S, typename T::type M> int g(K<M>);
int a = g(K<42>{});
