/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=nothrow -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that C makes for implicit nothrow in noreturn handling.  */

#define ATTR_NOTHROW_OPT

#include "../../c-c++-common/torture/harden-cfr-noret.c"

/* One out-of-line check before the noreturn call in f, and another at the end
   of f.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 2 "hardcfr" } } */
/* One inline check in h, before the noreturn call, and another in h2, before
   or after the call, depending on noreturn detection.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "hardcfr" } } */
