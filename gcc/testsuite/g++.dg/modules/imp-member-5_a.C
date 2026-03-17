// PR c++/124311
// { dg-additional-options "-fmodules" }
// { dg-module-cmi A }

export module A;
export struct S {
  int x = 0;
  S() = default;
};
