/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=no-xthrow -fdump-tree-hardcfr --param hardcfr-max-blocks=9 --param hardcfr-max-inline-blocks=5 -ffat-lto-objects -w" } */

/* Check the instrumentation and the parameters with checking before
   all noreturn calls that aren't expected to throw.  */

#include "harden-cfr.c"

/* Inlined checking thus trap for f.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
/* Out-of-line checking for g (param), and before both noreturn calls in main.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 3 "hardcfr" } } */
/* No checking for h (too many blocks).  */
