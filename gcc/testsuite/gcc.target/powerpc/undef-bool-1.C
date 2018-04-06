/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11 -DNO_WARN_X86_INTRINSICS" } */

/* Test to ensure that "bool" gets undef'd in xmmintrin.h when
   we require strict ANSI.  */

#include <xmmintrin.h>

bool foo (int x)
{
  return x == 2;
}

