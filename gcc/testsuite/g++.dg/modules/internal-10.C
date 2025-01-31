// { dg-additional-options "-fmodules -std=c++20" }
// { dg-module-cmi !M }

// These are not TU-local according to the standard,
// but we treat as TU-local due to ABI limitations.

export module M;

struct A {
  static inline auto x = []{};

  template <typename T>
  static inline auto y = []{};
};

struct B {
  template <typename T>
  static inline decltype([]{}) x = {};  // { dg-bogus "TU-local" "" { xfail *-*-* } }
};

template <typename T>
struct C {
  template <typename U>
  static inline decltype([]{}) x = {};  // { dg-bogus "TU-local" "" { xfail *-*-* } }
};
