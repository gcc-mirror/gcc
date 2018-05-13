// { dg-additional-options "-fmodules-ts" }
export module Bob; // { dg-message "declared here" }
// No need to dg-module-bmi

import Kevin; // { dg-error "failed to import module" }
// { dg-error "jumping off" "" { target *-*-* } .-1 }
// { dg-error "cannot import module" "Kevin.nms:" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
