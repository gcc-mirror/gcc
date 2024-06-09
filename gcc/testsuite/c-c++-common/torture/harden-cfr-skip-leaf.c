/* { dg-do run } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-skip-leaf -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Test skipping instrumentation of leaf functions.  */

#include "harden-cfr.c"

/* { dg-final { scan-tree-dump-times "__builtin_trap" 0 "hardcfr" } } */
/* Only main isn't leaf.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
