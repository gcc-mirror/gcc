// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }

import bob;  // { dg-error "failed to import" }
// { dg-error "jumping off" "" { target *-*-* } .-1 }
// { dg-prune-output "compilation terminated" }

