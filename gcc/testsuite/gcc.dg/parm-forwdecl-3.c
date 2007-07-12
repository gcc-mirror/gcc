/* Test GNU parameter forward declarations.  Error with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

int f1(int a; int a); /* { dg-error "ISO C forbids forward parameter declarations" } */
int f2(int a; int a) { return 0; } /* { dg-error "ISO C forbids forward parameter declarations" } */
