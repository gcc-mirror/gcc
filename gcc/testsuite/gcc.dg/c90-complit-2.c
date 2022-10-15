/* Test C2x storage class specifiers in compound literals not permitted for
   C90, but without a duplicate diagnostic, just the diagnostic for compound
   literals not being permitted in C90 at all.  */
/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors" } */

int *ps = &(static int) { 1 }; /* { dg-error "ISO C90 forbids compound literals" } */
int ss = sizeof (static int) { 1 }; /* { dg-error "ISO C90 forbids compound literals" } */
