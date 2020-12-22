// { dg-additional-options "-fmodules-ts" }

import foo;

// { dg-regexp "In module imported at \[^\n]*omp-1_c.C:3:1:\nfoo: error: module contains OpenMP, use '-fopenmp' to enable\n" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
