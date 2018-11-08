// { dg-additional-options "-fmodules-ts" }
int k;
module frob; // { dg-error "global module fragment not present" }
// { dg-error "failed to read" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
