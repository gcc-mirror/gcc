// { dg-additional-options "-fmodules-ts" }
#define import import
import malcolm;
// { dg-error "failed reading" "" { target *-*-* } 0 }

// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error:" }
