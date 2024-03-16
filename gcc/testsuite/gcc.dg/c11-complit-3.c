/* Test C23 storage class specifiers in compound literals not permitted for
   C11, but -Wno-c11-c23-compat disables the -pedantic diagnostic for that.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-c11-c23-compat" } */

int *ps = &(static int) { 1 };
int ss = sizeof (static int) { 1 };
