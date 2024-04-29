// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi X }

export module X;
export import M;

A<int> x;
export extern "C++" template <typename T> struct B { using type = T; };
