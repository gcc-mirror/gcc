/* { dg-do compile } */
#include <altivec.h>
vector unsigned int
f(vector signed char a)
{
  return (vector unsigned int)(a); 
}
