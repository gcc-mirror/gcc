/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fhardcfr-check-noreturn-calls=never -foptimize-sibling-calls -fdump-tree-hardcfr -ffat-lto-objects" } */

/* Check that we insert cleanups for checking around the bodies of
   maybe-throwing functions.  These results depend on checking before
   returning calls, which is only enabled when sibcall optimizations
   are enabled, so change the optimization mode to -O1 for f and f2,
   so that -foptimize-sibling-calls can take effect and enable
   -fhardcfr-check-returning-calls, so that we get the same results.
   There is a separate test for -O0.  */

#if ! __OPTIMIZE__
void __attribute__ ((__optimize__ (1, "-foptimize-sibling-calls"))) f(int i);
void __attribute__ ((__optimize__ (1, "-foptimize-sibling-calls"))) f2(int i);
void __attribute__ ((__optimize__ (1, "-foptimize-sibling-calls"))) h3(void);
#endif

#include "harden-cfr-throw.C"

/* f gets out-of-line checks before the unwrapped tail call and in the
   else edge.  */
/* f2 gets out-of-line checks before both unwrapped tail calls.  */
/* h gets out-of-line checks before the implicit return and in the
   cleanup block.  */
/* h2 and h2b get out-of-line checks before the cleanup returning
   call, and in the cleanup block.  */
/* h3 gets an inline check before the __cxa_end_catch returning call.  */
/* h4 gets an inline check in the cleanup block.  */

/* { dg-final { scan-tree-dump-times "hardcfr_check" 10 "hardcfr" } } */
/* { dg-final { scan-tree-dump-times "builtin_trap" 2 "hardcfr" } } */
