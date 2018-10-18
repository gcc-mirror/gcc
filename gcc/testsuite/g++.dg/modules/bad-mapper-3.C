//  { dg-additional-options "-fmodules-ts -fmodule-mapper=localhost:172477262" }
import bob; // { dg-error "failed connecting socket .*: Invalid argument" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
