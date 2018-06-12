// { dg-additional-options -fmodules-atom }

// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }
// { dg-module-bmi "!bill" }

import bill;  // { dg-error "module 'bill' is unknown" }
import bob;  // { dg-error "module 'bob' is unknown" }
// { dg-prune-output "mapper cannot provide" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

