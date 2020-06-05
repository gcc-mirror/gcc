//  { dg-additional-options "-fmodules-ts -fmodule-mapper=localhost:172477262" }
import unique3.bob;
// { dg-error "failed connecting socket .* 'localhost:" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
