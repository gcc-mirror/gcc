//  { dg-additional-options "-fmodule-mapper=not-a-host:3838" }
import bob; // { dg-error "failed resolving.*: Unknown host" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
