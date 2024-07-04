/* { dg-do compile } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify there is no warning message.  */

#define NO_WARN_X86_INTRINSICS 1
#include <emmintrin.h>

__m128i
foo (__m128i A)
{
  return _mm_bsrli_si128 (A, 0);
}
