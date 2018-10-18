//  { dg-additional-options "-fmodules-ts -fmodule-mapper=|this-will-not-work" }
import bob; // { dg-error "failed exec.*No such file" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
