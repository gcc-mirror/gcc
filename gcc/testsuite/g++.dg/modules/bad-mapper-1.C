//  { dg-additional-options "-fmodule-mapper=|this-will-not-work" }
import bob; // { dg-error "unexpected end of file" }
// { dg-error "exit status" "" { target *-*-* } .-1 }
// { dg-prune-output "No such file" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
