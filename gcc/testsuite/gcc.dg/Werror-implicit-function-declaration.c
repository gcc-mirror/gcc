/* { dg-do compile } */
/* { dg-options "-std=c89 -Werror-implicit-function-declaration" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-error "implicit declaration of function" } */
}
