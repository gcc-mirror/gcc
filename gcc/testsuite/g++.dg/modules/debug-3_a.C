// PR c++/102607
// { dg-additional-options "-fmodules-ts -g" }
// { dg-module-cmi mod }

export module mod;
export struct B {
  virtual ~B() = default;
};
