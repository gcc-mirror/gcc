/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we don't insert checking before noreturn calls.  -O0 is tested
   separately because h is not found to be noreturn without optimization, which
   affects codegen for h2, so h2 is omitted here at -O0.  */

#if !__OPTIMIZE__
# define OMIT_H2
#endif

#include "harden-cfr-noret.c"


/* No out-of-line checks.  */
/* { dg-final { scan-tree-dump-times "hardcfr_check" 0 "hardcfr" } } */
/* Only one inline check at the end of f.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
