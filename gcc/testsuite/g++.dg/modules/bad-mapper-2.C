//  { dg-additional-options "-fmodules-ts -fmodule-mapper=not-a-host:3838" }
import unique2.bob;
// { dg-error "failed .* mapper 'not-a-host" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "compilation terminated" }
