/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -fno-exceptions -fdump-tree-hardcfr -ffat-lto-objects" } */
/* { dg-require-effective-target untyped_assembly } */

/* Check that, even disabling checks before noreturn calls (leaving returning
   calls enabled), we still get checks before __builtin_return.  */

#include "harden-cfr-bret.c"

/* Out-of-line checking, before both builtin_return and return in f.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
/* Inline checking before builtin_return in g.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
