/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -fdump-tree-hardcfr --param hardcfr-max-blocks=9 --param hardcfr-max-inline-blocks=5 -ffat-lto-objects -w" } */

/* Check the instrumentation and the parameters without checking before
   noreturn calls.  */

#include "harden-cfr.c"

/* Inlined checking thus trap for f.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
/* Out-of-line checking for g (param).  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 1 "hardcfr" } } */
/* No checking for h (too many blocks) or main (no edges to exit block).  */
