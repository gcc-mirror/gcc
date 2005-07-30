/* Test GNU parameter forward declarations.  Warning with
   -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

int f1(int a; int a); /* { dg-warning "warning: ISO C forbids forward parameter declarations" } */
int f2(int a; int a) { return 0; } /* { dg-warning "warning: ISO C forbids forward parameter declarations" } */
