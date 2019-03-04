// { dg-additional-options -fmodules-ts }
#define MODULE module   // { dg-error "must not be from macro" }
export MODULE bob;
// { dg-module-bmi !bob }
// { dg-prune-output "not writing module" }
