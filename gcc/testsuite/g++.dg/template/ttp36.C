// Verify we propagate cv-quals when level-lowering a bound ttp.

template<class T>
struct B {
  template<template<class> class TT>
  void f(TT<T>*);

  template<template<class> class TT>
  void f(const TT<T>*); // { dg-bogus "cannot be overloaded" }
};

template struct B<int>;
