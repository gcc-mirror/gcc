// { dg-additional-options "-fmodules-ts -Wno-global-module" }

module;
template <typename T> struct S;
export module C;
S(const char*) -> S<const char*>;
