/* Test C23 storage class specifiers in compound literals not permitted for
   C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

int *ps = &(static int) { 1 }; /* { dg-warning "forbids storage class specifiers in compound literals" } */
int ss = sizeof (static int) { 1 }; /* { dg-warning "forbids storage class specifiers in compound literals" } */
