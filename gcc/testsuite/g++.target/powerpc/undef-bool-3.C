/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11 -maltivec" } */

/* Test to ensure that "bool" is not #define'd in altivec.h for C++ when
   we require strict ANSI.  We should compile without errors.  */

#include <altivec.h>

bool foo (int x)
{
  return x == 2;
}

