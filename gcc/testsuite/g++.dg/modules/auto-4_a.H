// PR c++/114795
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template<class T>
struct A {
  auto f() { return T(); }
};

template<class T>
void g() {
  A<int> a;
  a.f();
}
