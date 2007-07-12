/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic-errors -Wimplicit-function-declaration" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-warning "implicit declaration of function" } */
}
