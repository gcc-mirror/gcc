// { dg-additional-options "-fmodules-ts" }
// { dg-require-effective-target pthread }

import foo;

// { dg-regexp "In module imported at \[^\n]*omp-3_d.C:4:1:\nfoo: error: module contains OpenMP, use '-fopenmp' or '-fopenmp-simd' to enable\n" }
// { dg-prune-output "failed to read" }
// { dg-prune-output "fatal error:" }
// { dg-prune-output "compilation terminated" }
