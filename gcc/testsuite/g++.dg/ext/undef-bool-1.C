/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -DNO_WARN_X86_INTRINSICS -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to ensure that "bool" gets undef'd in xmmintrin.h when
   we require strict ANSI.  */

#include <xmmintrin.h>

bool foo (int x)
{
  return x == 2;
}

