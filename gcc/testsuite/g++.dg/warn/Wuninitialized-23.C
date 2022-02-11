// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }
// Test that we don't warn in an uninstantiated template.

struct A {
  int *fn() { return nullptr; }
};

template<typename T>
struct B {
  B() : p(a->fn()) { }
  A *a;
  int *p;
};

template<typename T>
struct C {
  C() : p(a->fn()) { } // { dg-warning "member .C<int>::a. is used uninitialized" }
  A *a;
  int *p;
};

C<int> c;
