/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O0 -mssse3 -msse4a" } */

/* Test that the intrinsics compile without optimization.  All of them are
   defined as inline functions in mmintrin.h that reference the proper
   builtin functions.  Defining away "static" and "__inline" results in
   all of them being compiled as proper functions.  */

#define static
#define __inline

/* Following intrinsics require immediate arguments. */

/* emmintrin.h */
#define __builtin_ia32_psllwi128(A, B) __builtin_ia32_psllwi128(A, 1)
#define __builtin_ia32_psrlqi128(A, B) __builtin_ia32_psrlqi128(A, 1)
#define __builtin_ia32_psrlwi128(A, B) __builtin_ia32_psrlwi128(A, 1)
#define __builtin_ia32_psrldi128(A, B) __builtin_ia32_psrldi128(A, 1)
#define __builtin_ia32_psrldqi128(A, B) __builtin_ia32_psrldqi128(A, 8)
#define __builtin_ia32_pslldqi128(A, B) __builtin_ia32_pslldqi128(A, 8)
#define __builtin_ia32_psrawi128(A, B) __builtin_ia32_psrawi128(A, 1)
#define __builtin_ia32_psradi128(A, B) __builtin_ia32_psradi128(A, 1)
#define __builtin_ia32_psllqi128(A, B) __builtin_ia32_psllqi128(A, 1)
#define __builtin_ia32_pslldi128(A, B) __builtin_ia32_pslldi128(A, 1)

/* xmmintrin.h */
#define __builtin_prefetch(P, A, I) __builtin_prefetch(P, A, _MM_HINT_NTA)
#define __builtin_ia32_pshufw(A, N) __builtin_ia32_pshufw(A, 0)
#define __builtin_ia32_vec_set_v4hi(A, D, N) \
  __builtin_ia32_vec_set_v4hi(A, D, 0)
#define __builtin_ia32_vec_ext_v4hi(A, N) __builtin_ia32_vec_ext_v4hi(A, 0)
#define __builtin_ia32_shufps(A, B, C) __builtin_ia32_shufps(A, B, 0)

#include <ammintrin.h>
#include <tmmintrin.h>

