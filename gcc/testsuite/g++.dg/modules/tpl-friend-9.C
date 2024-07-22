// PR c++/114275
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

template<class> struct A {
  template<class> friend struct B;
  friend void C();
};
A<int> a;
void C() {}
template<class> struct B { };
