// PR c++/118961
// { dg-additional-options "-fmodule-header -std=c++20" }
// { dg-module-cmi {} }
// Test we correctly emit the bodies of cloned constructors.

template <typename>
struct S {
  S() requires true {}
};

inline S<int> foo() { return {}; }
