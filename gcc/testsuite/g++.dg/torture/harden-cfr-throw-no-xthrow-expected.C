/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fno-hardcfr-check-returning-calls -fhardcfr-check-noreturn-calls=no-xthrow -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we insert cleanups for checking around the bodies of
   maybe-throwing functions, and also checking before noreturn
   calls.  */

extern void __attribute__ ((__noreturn__, __expected_throw__)) g (void);
extern void __attribute__ ((__noreturn__, __expected_throw__)) g2 (void);

#include "harden-cfr-throw.C"

/* In f and h3, there are checkpoints at return and exception escape.  .  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 4 "hardcfr" } } */
/* Other functions get a single cleanup checkpoint.  */
/* { dg-final { scan-tree-dump-times "builtin_trap" 5 "hardcfr" } } */
