// { dg-additional-options -O2 }

// { dg-error "compilation options differ" "" { target *-*-* } 0 }

import opt;

// { dg-error "failed to read BMI" "" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error" }
