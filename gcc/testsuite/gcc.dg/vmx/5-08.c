/* { dg-do compile } */
#include <altivec.h>
vector unsigned int a;
vector unsigned int b;
void
f(void) 
{
  a = ((vector unsigned int){1,1,1,1});
  b = ((vector unsigned int){1,2,3,4});
}
