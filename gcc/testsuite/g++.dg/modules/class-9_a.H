// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

struct A;
A* foo();

template <typename T> struct B;
template <typename T> B<T>* bar();
