// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !mod }

export module mod;

namespace {
  void internal() {}
}

export extern "C++" auto foo() {
  struct X {
    // `foo` is not attached to a named module, and as such
    // `X::f` should be implicitly `inline` here
    void f() {  // { dg-error "exposes TU-local entity" }
      internal();
    }
  };
  return X{};
}
