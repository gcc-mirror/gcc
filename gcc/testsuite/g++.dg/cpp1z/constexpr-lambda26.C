// PR c++/87765
// { dg-do compile { target c++17 } }
// { dg-additional-options "-fchecking" }
// { dg-ice "cxx_eval_constant_expression" }

template <int N>
using foo = int;

struct A {
  constexpr int bar() const { return 42; }
};

void baz(A a) {
  [=](auto c) { return foo<a.bar()> { }; }; }
