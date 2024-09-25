// PR c++/116496
// { dg-additional-options "-fmodules-ts -std=c++20 -Wno-global-module" }
// { dg-module-cmi A }

module;
template <typename T> struct S {};
export module A;
template <typename T> struct S<T*> {};
template <typename T> requires false struct S<T*> {};
