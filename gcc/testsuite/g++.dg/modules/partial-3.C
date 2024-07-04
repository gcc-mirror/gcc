// PR c++/114630
// { dg-additional-options "-fmodules-ts -std=c++20 -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;

template <typename T> struct S {};

template <typename T> struct S<T*> {};
template <typename T> requires (sizeof(T) == 4) struct S<T*> {};

template <typename T> int V = 0;

template <typename T> int V<T*> = 1;
template <typename T> requires (sizeof(T) == 4) int V<T*> = 2;

export module M;

// The whole GMF should be discarded here
// { dg-final { scan-lang-dump "Wrote 0 clusters" module } }
