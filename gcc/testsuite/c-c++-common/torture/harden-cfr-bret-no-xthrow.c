/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=no-xthrow -fno-exceptions -fdump-tree-hardcfr -ffat-lto-objects" } */
/* { dg-require-effective-target untyped_assembly } */

/* Check that, even enabling checks before no-xthrow-throwing noreturn calls
   (leaving returning calls enabled), we get checks before __builtin_return
   without duplication (__builtin_return is both noreturn and a returning
   call).  */

#include "harden-cfr-bret.c"

/* Out-of-line checking, before both builtin_return and return in f.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
/* Inline checking before builtin_return in g.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
