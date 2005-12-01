/* PR middle-end/18096  */
/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-O2" } */

void f(char*);

void mkcatdefs(char *fname) 
{
  char line [2147483647];
  f(line);
} /* { dg-warning "too large" "stack frame too large" } */

