/* { dg-do compile } */
/* { dg-options "-std=c89" } */

void f(void) 
{ 
  puts("Hello"); /* { dg-bogus "warning: implicit declaration of function" } */
}
