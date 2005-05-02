/* Test diagnostics for old-style definition not matching prior
   prototype are present and give correct location for that prototype
   (bug 15698).  Prototype not last declaration.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void f();
void f(int); /* { dg-error "error: prototype declaration" } */
void f();
void f(a) long a; {} /* { dg-error "error: argument 'a' doesn't match prototype" } */
