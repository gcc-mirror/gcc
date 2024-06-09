/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fno-hardcfr-check-returning-calls -fhardcfr-check-noreturn-calls=no-xthrow -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we insert cleanups for checking around the bodies of
   maybe-throwing functions, and also checking before noreturn
   calls.  */

#include "harden-cfr-throw.C"

/* { dg-final { scan-tree-dump-times "hardcfr_check" 12 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 1 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "Bypassing" 0 "hardcfr" } } */
