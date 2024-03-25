/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check the noreturn handling of a builtin call with never.  */

#include "harden-cfr-abrt.c"

/* No out-of-line checking.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 0 "hardcfr" } } */
/* Inline checking only before return in f.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
