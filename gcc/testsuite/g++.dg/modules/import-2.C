// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }

import bob;  // { dg-error "failed to import" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "server error" }
// { dg-prune-output "compilation terminated" }

