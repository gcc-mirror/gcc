// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
  concept bool C() { return __is_class(T); }

template<int N>
  concept bool Int() { return true; }

template<template<typename> class X>
  concept bool Template() { return true; }

void f1(Int) { }      // { dg-error "does not constrain a type" }
void f2(Template) { } // { dg-error "does not constrain a type" }

struct S { };

struct S1 {
  void f1(auto x) { }
  void f2(C x) { }

  void f3(auto x) { }
  void f3(C x) { }
};

template<C T>
  struct S2 {
    void f1(auto x) { }
    void f2(C x) { }

    void h1(auto x);
    void h2(C x);

    template<C U>
      void g(T t, U u) { }
  };

int main() {
  S s;

  S1 s1;
  s1.f2(0); // { dg-error "matching" }

  S2<S> s2;
  s2.f2(0); // { dg-error "matching" }
  s2.h2(0); // { dg-error "matching" }

  s2.g(s, 0); // { dg-error "matching" }
  s2.g(0, s); // { dg-error "matching" }
}
