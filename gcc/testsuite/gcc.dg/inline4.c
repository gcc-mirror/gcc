/* { dg-do compile } */
/* { dg-options "-Wall -std=gnu89" } */
/* This testcase should fail since we're redefining foo in the same
   translation unit.  */
int foo (void) { return 2; } /* { dg-error "previous definition of" } */
extern inline int foo (void) { return 1; } /* { dg-error "redefinition of" } */
