// { dg-do assemble  }

template <class T> struct A {};
template <class T> struct A<T*>;
A<int*> ai; // { dg-error "" } incomplete type
