// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<class T>
struct A {
  static void f() { T::nonexistent; }
};

template<>
inline void A<int>::f() { }
