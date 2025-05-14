// { dg-additional-options "-fmodules-ts" }
// { dg-require-effective-target pthread }

import foo;

// { dg-regexp "In module imported at \[^\n]*oacc-1_c.C:4:1:\nfoo: error: module contains OpenACC, use '-fopenacc' to enable\n" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
