// PR c++/35325
// { dg-options "" }

template<int> struct A {};

template<typename> struct B : A<sizeof(0=0r)> {};

template<typename> struct C : A<sizeof(0=0r)> {};
