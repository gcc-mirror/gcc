// PR c++/11858

template <typename T> struct S { static typename T::x f (); }; // { dg-error "" }
template <class T> int f (int [sizeof(T::f())]);
int const i = f<S<int> >(0); // { dg-error "no matching function" }
