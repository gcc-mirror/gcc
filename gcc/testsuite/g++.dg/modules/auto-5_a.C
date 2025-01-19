// PR c++/118049
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi A }

module;
template <typename T> struct S {
  auto foo() {}
};
export module A;
template struct S<char>;
