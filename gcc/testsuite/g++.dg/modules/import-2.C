// { dg-additional-options "-fmodules-ts" }
// Don't segfault on missing module BMI

// { dg-module-cmi "!bob" }
// { dg-module-cmi "!bill" }

import bill;
// { dg-regexp "In module imported at \[^\n]*import-2.C:7:.:\nbill: error: failed to read compiled module: \[^\n]*\n" }

// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }

