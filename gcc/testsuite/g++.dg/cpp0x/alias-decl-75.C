// PR c++/112633
// { dg-do compile { target c++11 } }

struct A { using type = void; };

template<class>
using ty1 = A;

template<class T>
using ty2 = typename ty1<T>::type;

template<class T>
ty2<T> f();
