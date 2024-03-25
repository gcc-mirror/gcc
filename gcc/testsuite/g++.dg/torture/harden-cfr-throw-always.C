/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fno-hardcfr-check-returning-calls -fhardcfr-check-noreturn-calls=always -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we insert cleanups for checking around the bodies of
   maybe-throwing functions, and also checking before noreturn
   calls.  */

#include "harden-cfr-throw.C"

/* { dg-final { scan-tree-dump-times "hardcfr_check" 14 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 1 "hardcfr" } } */
/* h, h2, h2b, and h4.  */
/* { dg-final { scan-tree-dump-times "Bypassing" 4 "hardcfr" } } */
