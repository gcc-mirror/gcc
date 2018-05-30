// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }

import bob;
// { dg-message "module imported at" "" { target *-*-* } 0 }
// { dg-error "unknown BMI file" "" { target *-*-* } 0 }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "server error" }
// { dg-prune-output "compilation terminated" }

