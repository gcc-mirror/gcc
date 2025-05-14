// PR c++/114630
// { dg-additional-options "-fmodules-ts -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi M }

module;
template <typename> struct allocator {
  allocator() {}
};
template class allocator<wchar_t>;

// Deferred instantiation of GM virtual functions or friend functions
// should not place newly discovered declarations in the module purview.
template <typename T>
void go() {
  extern T fn_decl();
}
template <typename T>
struct S {
  friend void x() {}
  virtual void f() {
    go<char>();
  }
};
inline S<int> s;

export module M;

// The whole GMF should be discarded here
// { dg-final { scan-lang-dump "Wrote 0 clusters" module } }
