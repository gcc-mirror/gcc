/* Test diagnostics for old-style definition not matching prior
   prototype are present and give correct location for that prototype
   (bug 15698).  Prototyped built-in function, wrong number of
   arguments.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

char *strchr(a) const char *a; { return 0; } /* { dg-warning "warning: number of arguments doesn't match built-in prototype" } */
