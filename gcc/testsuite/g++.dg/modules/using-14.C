// PR c++/114600
// { dg-additional-options "-fmodules-ts -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;
namespace std {
  template<class T> struct A { int n; };
  template<class T> A<T> f();
  namespace __swappable_details { using std::f; }
}
export module M;

// The whole GMF should be discarded here
// { dg-final { scan-lang-dump "Wrote 0 clusters" module } }
