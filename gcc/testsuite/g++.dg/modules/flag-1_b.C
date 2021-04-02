// { dg-additional-options "-fmodules-ts -std=c++2a" }

// { dg-error "language dialect differs" "" { target *-*-* } 0 }

import opt;

// { dg-error "failed to read" "" { target *-*-* } 0 }
// { dg-prune-output "compilation terminated" }
// { dg-prune-output "fatal error" }
