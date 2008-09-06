/* { dg-do compile { target mips16_attribute } } */
/* { dg-mips-options "-O2 -mr10k-cache-barrier=store" } */
/* { dg-add-options mips16_attribute } */

/* Test that indirect calls are protected.  */

MIPS16 void foo (void) { } /* { dg-message "sorry, unimplemented" } */
