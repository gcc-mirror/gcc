/* { dg-do compile } */
/* { dg-options "-fpermissive -std=c99" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-warning "implicit declaration of function" } */
}
