// { dg-additional-options "-fmodules-ts" }
int k;
module frob; // { dg-error "not permitted" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
