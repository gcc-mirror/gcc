// { dg-additional-options "-fmodules-ts" }

#define garply fred
export module foo.bar:baz.garply;
// { dg-error "module partition 'garply' cannot be an object-like macro" "" { target *-*-* } .-1 }

// { dg-module-cmi !foo.bar:baz.garply }
// { dg-module-cmi !foo.bar:baz.fred }
// { dg-prune-output "not writing module" }
