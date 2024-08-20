// { dg-additional-options "-fmodules-ts -fdeclone-ctor-dtor" }
// { dg-module-cmi M }

export module M;
export struct S {
  inline S(int) {}
};
