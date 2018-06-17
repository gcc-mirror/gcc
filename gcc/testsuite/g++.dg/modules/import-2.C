// { dg-additional-options -fmodules-atom }

// Don't segfault on missing module BMI

// { dg-module-bmi "!bob" }
// { dg-module-bmi "!bill" }

import bill;
// { dg-error "failed to read module 'bill.nms'" "" { target *-*-* } 0 }
import bob;
// { dg-error "failed to read module 'bob.nms'" "" { target *-*-* } 0 }

// { dg-prune-output "mapper cannot provide" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

