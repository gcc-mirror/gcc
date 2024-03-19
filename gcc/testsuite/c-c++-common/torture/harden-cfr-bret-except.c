/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fexceptions -fdump-tree-hardcfr -ffat-lto-objects" } */
/* { dg-require-effective-target untyped_assembly } */

/* Check that, with exceptions enabled, even in C, the calls initiated by
   builtin_apply are enclosed in cleanup handlers that add extra checks.
   Unfortunately, declaring foobar as nothrow is not enough to avoid the
   handler around the builtin_apply call, so the other bret tests all use
   -fno-exceptions.  */

#include "harden-cfr-bret.c"

/* With exceptions, we get an extra check per function, to check before
   propagating exceptions, so it's 3 in f and 2 in g.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 5 "hardcfr" } } */
/* The extra check in g also removes the possibility of inlining the check.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 0 "hardcfr" } } */
