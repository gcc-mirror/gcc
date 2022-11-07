/* Test C2x storage class specifiers in compound literals not permitted for
   C11, but -Wno-c11-c2x-compat disables the -pedantic diagnostic for that.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-c11-c2x-compat" } */

int *ps = &(static int) { 1 };
int ss = sizeof (static int) { 1 };
