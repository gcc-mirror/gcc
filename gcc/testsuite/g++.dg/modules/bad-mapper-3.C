//  { dg-additional-options "-fmodule-mapper=localhost:172477262" }
import bob; // { dg-error "failed resolving.*: Invalid argument" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
