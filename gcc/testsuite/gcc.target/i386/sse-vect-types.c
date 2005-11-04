/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O0 -msse2" } */

/* Test the intrinsics without optimization.  All of them are
   defined as inline functions in mmintrin.h that reference the proper
   builtin functions.  Defining away "static" and "__inline" results in
   all of them being compiled as proper functions.  */

#define static
#define __inline

#include <xmmintrin.h>

__m128d foo1(__m128d z, __m128d  a, int N) { 
  int i;
  for (i=0; i<N; i++) {
    a = _mm_add_ps(z, a); /* { dg-error "incompatible type" } */
  }
  return a;
}
