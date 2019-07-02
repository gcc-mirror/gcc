// { dg-additional-options -fmodules-ts }
#define MODULE module   // { dg-error "must not be from macro" }
export MODULE bob;
// { dg-module-cmi !bob }
// { dg-prune-output "not writing module" }
