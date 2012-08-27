/* { dg-options "(-mips16) -mr10k-cache-barrier=store -ffat-lto-objects" } */

/* Test that indirect calls are protected.  */

MIPS16 void foo (void) { } /* { dg-message "sorry, unimplemented" } */
