// { dg-additional-options -O2 }

// { dg-error "compilation options differ" "opt.nms:" { target *-*-* } 0 }

import opt; // { dg-error "failed to import" }
// { dg-error "jumping off" "" { target *-*-* } .-1 }
// { dg-prune-output "compilation terminated" }
