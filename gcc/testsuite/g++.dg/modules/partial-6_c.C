// PR c++/113814
// { dg-additional-options "-fmodules-ts" }

import "partial-6_b.H";

template <typename>
struct TestTTP2;

int main() {
  int a = f<double>().a;
  int b = g<TestTTP2>().b;
}
