// PR c++/115757
// { dg-additional-options "-fmodules-ts -Wunused" }
// { dg-module-cmi test }

export module test;

export template <typename T>
void foo(T n [[maybe_unused]]) {
  int x [[maybe_unused]];
}
