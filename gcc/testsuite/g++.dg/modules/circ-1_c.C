// { dg-additional-options "-fmodules-ts" }
export module Bob; // { dg-message "declared here" }
// No need to dg-module-cmi

import Kevin;
// { dg-error "failed to read" "" { target *-*-* } 0 }
// { dg-error "cannot import module" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
