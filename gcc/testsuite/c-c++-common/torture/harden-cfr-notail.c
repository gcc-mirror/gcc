/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fno-hardcfr-check-exceptions -fno-hardcfr-check-returning-calls -fdump-tree-hardcfr -ffat-lto-objects" } */

#include "harden-cfr-tail.c"

/* Inline checking after the calls, disabling tail calling.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 5 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "Inserting inline check before stmt" 0 "hardcfr" } } */
