// Don't segfault on missing module BMI

import bob;  // { dg-error "failed to import" }
// { dg-error "declining" "" { target *-*-* } .-1 }
// { dg-prune-output "compilation terminated" }

