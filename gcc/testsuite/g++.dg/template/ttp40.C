// Verify we check arity early before deduction without explicit
// template arguments.
// PR c++/84075

template<class T>
struct trait {
  static const int value = T::value; // { dg-bogus "not a member of 'B'" }
};

template<class T, int N = trait<T>::value>
struct A { };

template<class T>
void f(A<T, 42>, int); // #1

struct B { };

template<template<class> class TT>
void f(TT<B>); // #2

int main() {
  A<int, 42> a;
  f(a, 0); // OK, deduction for #2 short-circuited and A<B> not specialized,
	   // which would have resulted in a hard error
}
