/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=always -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check the noreturn handling of a builtin call with always.  */

#include "harden-cfr-abrt.c"

/* Out-of-line checking, before both builtin_abort and return in f.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
/* Inline checking before builtin_abort in g.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
