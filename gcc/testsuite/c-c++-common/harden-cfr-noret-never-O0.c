/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -O0 -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we don't insert checking before noreturn calls.  -O0 is tested
   separately because h is not found to be noreturn without optimization.  */

#include "torture/harden-cfr-noret.c"

/* No out-of-line checks.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 0 "hardcfr" } } */
/* Only one inline check at the end of f and of h2.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "hardcfr" } } */
