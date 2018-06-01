// { dg-additional-options -fmodules-atom }

// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }
// { dg-module-bmi "!bill" }

import bill;  // { dg-error "mapper cannot provide" }
import bob;  // { dg-error "mapper cannot provide" }
// { dg-prune-output "unknown BMI file" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

