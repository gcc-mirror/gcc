// PR c++/116496
// { dg-additional-options "-fmodules-ts -std=c++20 -Wno-global-module" }
// { dg-module-cmi B }

module;
template <typename T> struct S {};
export module B;
import A;
template <typename T> requires true struct S<T*> {};
