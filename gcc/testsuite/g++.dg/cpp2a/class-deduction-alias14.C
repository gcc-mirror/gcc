// PR c++/105841
// { dg-do compile { target c++20 } }

template<class T, int N>
struct A { A(...); };

template<class T, class... Ts>
A(T, Ts...) -> A<T, sizeof...(Ts)>;

template<class T, int N=0>
using B = A<T, N>;

B b(0, 0);
