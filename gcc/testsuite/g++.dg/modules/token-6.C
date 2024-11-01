// { dg-additional-options "-fmodules-ts" }

#define bob fred
export module bob;
// { dg-error "module name 'bob' cannot be an object-like macro" "" { target *-*-* } .-1 }

// { dg-module-cmi !bob }
// { dg-module-cmi !fred }
// { dg-prune-output "not writing module" }
