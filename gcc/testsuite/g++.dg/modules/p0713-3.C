// { dg-additional-options "-fmodules-ts" }
int k;
module frob; // { dg-error "only permitted as" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
