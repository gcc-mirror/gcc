// { dg-do compile { target concepts } }

using Bool = bool;
template <class T>
const Bool b = true;

template <class T>
concept BoolC = b<T>;

template <BoolC T> struct A { };

A<int> a;
