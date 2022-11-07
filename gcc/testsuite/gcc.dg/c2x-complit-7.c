/* Test C2x storage class specifiers in compound literals diagnosed with
   -Wc11-c2x-compat, but not errors with -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -Wc11-c2x-compat -pedantic-errors" } */

int *ps = &(static int) { 1 }; /* { dg-warning "forbids storage class specifiers in compound literals" } */
int ss = sizeof (static int) { 1 }; /* { dg-warning "forbids storage class specifiers in compound literals" } */
