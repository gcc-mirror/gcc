// { dg-additional-options "-fmodules-ts" }
int i;
import bazza;
// { dg-error "failed to read" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
