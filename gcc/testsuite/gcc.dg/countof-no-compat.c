/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wno-c23-c2y-compat" } */

int a[1];
int b[_Countof(a)];
