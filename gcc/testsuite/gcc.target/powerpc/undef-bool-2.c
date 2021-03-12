/* { dg-do compile } */
/* { dg-options "-O2 -std=c11 -DNO_WARN_X86_INTRINSICS -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */

/* Test to ensure that "bool" gets undef'd in xmmintrin.h when
   we require strict ANSI.  Subsequent use of bool needs stdbool.h.
   altivec.h should eventually avoid defining bool, vector, and
   pixel, following distro testing.  */

#include <xmmintrin.h>

bool foo (int x) /* { dg-error "unknown type name 'bool'" } */
{
  return x == 2;
}

