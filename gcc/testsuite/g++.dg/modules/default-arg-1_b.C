// PR c++/99274
// { dg-additional-options "-fmodules-ts" }
// Propagate default args from import to existing decls

void f(int a, int b);
template <typename T> void g(T a, T b);
template <typename T, typename U> struct A;
template <typename T, int N> struct B;
struct S {
  template <typename T = int> void x();
  void y(int n = 123);
};
struct nontrivial { nontrivial(int); };
void h(nontrivial p);

import "default-arg-1_a.H";

template <typename = A<int>, typename = B<int>> struct Q;

int main() {
  f(1);
  g(2);
  h();
  S{}.x();
  S{}.y();
}
