// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

module;
template <typename>
struct S {
  S() {}
};
export module M;
extern template class S<int>;
S<int> s;
