/* { dg-options "(-mips16) -mr10k-cache-barrier=store -ffat-lto-objects" } */

/* Test that indirect calls are protected.  */
/* This is testing for errors which can only happen in assembly generation.
   dg-error does not guarantee assembly generation, so we need to do it
   manually by using -ffat-lto-objects.  */

MIPS16 void foo (void) { } /* { dg-message "sorry, unimplemented" } */
