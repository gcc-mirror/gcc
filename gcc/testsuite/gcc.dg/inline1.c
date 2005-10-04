/* { dg-do compile } */
/* { dg-options "-Wall -std=gnu89" } */
/* This test is expected to fail with an error for the redefinition of foo.
   This violates the constraint of 6.9#3 (no more than one external definition
   of an identifier with internal linkage in the same translation unit).  */
static inline int foo(void) { return 1; } /* { dg-error "previous definition of" } */
static inline int foo(void) { return 0; } /* { dg-error "redefinition of" } */
+
