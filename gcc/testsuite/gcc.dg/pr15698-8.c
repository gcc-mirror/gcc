/* Test diagnostics for old-style definition not matching prior
   prototype are present and give correct location for that prototype
   (bug 15698).  Prototype refined at inner scope with only refinement
   conflicting with definition.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int f(int (*)[]);
int g() { int f(int (*)[2]); } /* { dg-error "prototype declaration" } */
int f(a) int (*a)[3]; { return 0; } /* { dg-error "argument 'a' doesn't match prototype" } */
