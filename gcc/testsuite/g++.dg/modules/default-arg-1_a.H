// PR c++/99274
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

void f(int a, int b = 123);
template <typename T> void g(T a, T b = {});

template <typename T, typename U = int> struct A;
template <typename T, int N = 5> struct B;

struct S {
  template <typename T = int> void x();
  void y(int n = 123);
};

struct nontrivial { nontrivial(int); };
void h(nontrivial p = nontrivial(123));
