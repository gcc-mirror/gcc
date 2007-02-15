/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors -Wall" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-error "error: implicit declaration of function" } */
}
