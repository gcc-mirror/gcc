/* Test C23 storage class specifiers in compound literals diagnosed with
   -Wc11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wc11-c23-compat" } */

int *ps = &(static int) { 1 }; /* { dg-warning "forbids storage class specifiers in compound literals" } */
int ss = sizeof (static int) { 1 }; /* { dg-warning "forbids storage class specifiers in compound literals" } */
