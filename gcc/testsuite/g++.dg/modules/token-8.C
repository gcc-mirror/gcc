// { dg-additional-options "-fmodules-ts" }

#define bob fred
export module foo.bar:bob;
// { dg-error "module partition 'bob' cannot be an object-like macro" "" { target *-*-* } .-1 }

// { dg-module-cmi !foo.bar:bob }
// { dg-module-cmi !foo.bar:fred }
// { dg-prune-output "not writing module" }
