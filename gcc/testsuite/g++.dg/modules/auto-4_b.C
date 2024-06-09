// PR c++/114795
// { dg-additional-options "-fmodules-ts -fno-module-lazy" }

template<class T>
struct A {
  auto f() { return T(); }
};

A<int> a;

import "auto-4_a.H";

int main() {
  g<int>(); // { dg-bogus "before deduction of 'auto'" "" { target *-*-* } 0 }
}
