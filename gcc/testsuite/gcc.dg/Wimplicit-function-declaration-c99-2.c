/* { dg-do compile } */
/* { dg-options "-std=c99" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-error "implicit declaration of function" } */
}
