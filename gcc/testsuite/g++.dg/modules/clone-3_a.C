// { dg-additional-options "-fmodules-ts -fdeclone-ctor-dtor" }
// { dg-module-cmi M }

export module M;

struct A {};
export struct B : virtual A {
  inline B (int) {}
};
