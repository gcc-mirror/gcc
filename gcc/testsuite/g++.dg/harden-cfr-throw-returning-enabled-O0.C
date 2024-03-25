/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-returning-calls -fdump-tree-hardcfr -O0" } */

/* Explicitly enable -fhardcfr-check-returning-calls -at O0.  */

#include "torture/harden-cfr-throw.C"

/* Same expectations as those in torture/harden-cfr-throw-returning.C.  */

/* { dg-final { scan-tree-dump-times "hardcfr_check" 10 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 2 "hardcfr" } } */
