/* { dg-do compile } */
/* { dg-options "-std=c89 -Wimplicit-function-declaration" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-warning "implicit declaration of function" } */
}
