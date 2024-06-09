// PR c++/99573
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !foo }

export module foo;
namespace std {
  enum class align_val_t : decltype(sizeof(int)) {};  // { dg-error "in module .foo. conflicts with builtin" }
}

// { dg-prune-output "not writing module" }
