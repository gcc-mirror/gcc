/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -foptimize-sibling-calls -fdump-tree-hardcfr -O0" } */

/* -fhardcfr-check-returning-calls gets implicitly disabled because,
   -at O0, -foptimize-sibling-calls has no effect.  */

#define NO_OPTIMIZE

#include "torture/harden-cfr-throw.C"

/* { dg-final { scan-tree-dump-times "hardcfr_check" 12 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 1 "hardcfr" } } */
