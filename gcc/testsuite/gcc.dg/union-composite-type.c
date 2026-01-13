/* { dg-do compile } */
/* { dg-options "-Wpedantic" } */

/* An old extension */

int f(union { int a; });
int f(int a);			/* { dg-warning "not truly compatible" } */

