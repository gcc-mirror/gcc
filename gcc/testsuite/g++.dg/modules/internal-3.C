// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi !M }
// TU-local entities in the GMF can be exposed.

module;

static inline void foo() {}

export module M;

inline void bar() {  // { dg-error "exposes TU-local entity" }
  foo();
}

// OK
void qux() {
  foo();
}
