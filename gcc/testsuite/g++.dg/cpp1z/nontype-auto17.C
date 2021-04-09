// { dg-do compile { target c++11 } }

template <int> struct K { };

template <class T = void, typename T::type M> int f(K<M>); // { dg-error "void" }
int a = f(K<42>{}); // { dg-error "no match" }

struct S { using type = void; };
template <class T = S, typename T::type M> int g(K<M>); // { dg-message "deduction" }
int b = g(K<42>{}); // { dg-error "no match" }
