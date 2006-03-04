/* PR middle-end/18096  */
/* { dg-do compile { target { powerpc*-*-* && ilp32 } } } */
/* { dg-options "-O2" } */

void f(char*);

void mkcatdefs(char *fname) 
{ /* { dg-warning "too large" "stack frame too large" } */
  char line [2147483647];
  f(line);
} 

