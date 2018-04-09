// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }

import bob;  // { dg-error "failed to import" }
// { dg-error "declining" "" { target *-*-* } .-1 }
// { dg-prune-output "compilation terminated" }

