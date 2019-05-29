// { dg-additional-options "-fmodules-ts" }
int k;
module frob; // { dg-error "global module fragment not present" }
// { dg-prune-output "failed reading" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
