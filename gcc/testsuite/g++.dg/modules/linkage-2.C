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

// { dg-prune-output "not writing module" }
