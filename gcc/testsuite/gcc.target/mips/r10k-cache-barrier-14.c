/* { dg-options "(-mips16) -O2 -mr10k-cache-barrier=store" } */

/* Test that indirect calls are protected.  */

MIPS16 void foo (void) { } /* { dg-message "sorry, unimplemented" } */
