/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -fno-hardcfr-check-exceptions  -fno-hardcfr-check-returning-calls -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we do not insert cleanups for checking around the bodies
   of maybe-throwing functions.  h4 doesn't get any checks, because we
   don't have noreturn checking enabled.  */

#include "harden-cfr-throw.C"

/* { dg-final { scan-tree-dump-times "hardcfr_check" 0 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 6 "hardcfr" } } */
