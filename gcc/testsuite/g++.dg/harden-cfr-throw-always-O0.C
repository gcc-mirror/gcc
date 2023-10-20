/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=always -fdump-tree-hardcfr -ffat-lto-objects -O0" } */

/* Check that we insert cleanups for checking around the bodies of
   maybe-throwing functions, and also checking before noreturn
   calls.  h2 and h2b get an extra resx without ehcleanup.  */

#define NO_OPTIMIZE

#include "torture/harden-cfr-throw.C"

/* { dg-final { scan-tree-dump-times "hardcfr_check" 16 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 1 "hardcfr" } } */
