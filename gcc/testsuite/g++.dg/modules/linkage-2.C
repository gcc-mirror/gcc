// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !M }

export module M;

// Same as a linkage-1 except within an anonymous namespace;
// now these declarations cannot possibly be defined outside this TU,
// so we should error.

namespace {
  auto f() {
    struct A {};
    return A{};
  }
  decltype(f()) g();  // { dg-error "used but never defined" }

  struct {} s;
  decltype(s) h();  // { dg-error "used but never defined" }
}

export void use() {
  g();
  h();
}

// Additionally, unnamed types have no linkage but are also TU-local, and thus
// cannot be exposed in a module interface unit.  The non-TU-local entity 's'
// here is an exposure of this type.
struct {} s;  // { dg-error "exposes TU-local entity" }
