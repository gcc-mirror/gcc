// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M }

export module M;

export extern "C++" template <typename> struct A {
  template <typename> friend struct B;
};
