// PR c++/32505
template <class T> struct A { };
A<int*> a;
template <class T> struct A<T*> { }; // { dg-error "A<int\\*>" }
