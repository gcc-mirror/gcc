/* Test diagnostics for old-style definition not matching prior
   prototype are present and give correct location for that prototype
   (bug 15698).  Prototype at inner scope.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void f() { void g(void); } /* { dg-error "prototype declaration" } */
void g(a) int a; {} /* { dg-error "number of arguments doesn't match prototype" } */
