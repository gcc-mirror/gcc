// { dg-options "-fmodules-ts" }

import bob; // { dg-error "failed to import" }
// { dg-error "declining opportunity" "" { target *-*-* } .-1 }
// { dg-error "TS/ATOM mismatch" "bob.nms:" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
