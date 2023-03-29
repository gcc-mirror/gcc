/* Test C2x storage class specifiers in compound literals not permitted for
   C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int *ps = &(static int) { 1 }; /* { dg-error "forbids storage class specifiers in compound literals" } */
int ss = sizeof (static int) { 1 }; /* { dg-error "forbids storage class specifiers in compound literals" } */
