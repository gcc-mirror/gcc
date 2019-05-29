// { dg-additional-options "-fmodules-ts -O2" }

// { dg-error "compilation options differ" "" { target *-*-* } 0 }

import opt;

// { dg-error "failed reading" "" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error" }
