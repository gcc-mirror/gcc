/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -mssse3 -msse4a" } */

/* Test that the intrinsics compile with optimization.  All of them are
   defined as inline functions in mmintrin.h that reference the proper
   builtin functions.  Defining away "static" and "__inline" results in
   all of them being compiled as proper functions.  */

#define static
#define __inline

/* Following intrinsics require immediate arguments. */

/* ammintrin.h */
#define __builtin_ia32_extrqi(X, I, L)  __builtin_ia32_extrqi(X, 1, 1)
#define __builtin_ia32_insertqi(X, Y, I, L) __builtin_ia32_insertqi(X, Y, 1, 1)

/* tmmintrin.h */
#define __builtin_ia32_palignr128(X, Y, N) __builtin_ia32_palignr128(X, Y, 8)
#define __builtin_ia32_palignr(X, Y, N) __builtin_ia32_palignr(X, Y, 8)

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
#define __builtin_ia32_pshufhw(A, N) __builtin_ia32_pshufhw(A, 0)
#define __builtin_ia32_pshuflw(A, N) __builtin_ia32_pshuflw(A, 0)
#define __builtin_ia32_pshufd(A, N) __builtin_ia32_pshufd(A, 0)
#define __builtin_ia32_vec_set_v8hi(A, D, N) \
  __builtin_ia32_vec_set_v8hi(A, D, 0)
#define __builtin_ia32_vec_ext_v8hi(A, N) __builtin_ia32_vec_ext_v8hi(A, 0)
#define __builtin_ia32_shufpd(A, B, N) __builtin_ia32_shufpd(A, B, 0)

/* xmmintrin.h */
#define __builtin_prefetch(P, A, I) __builtin_prefetch(P, A, _MM_HINT_NTA)
#define __builtin_ia32_pshufw(A, N) __builtin_ia32_pshufw(A, 0)
#define __builtin_ia32_vec_set_v4hi(A, D, N) \
  __builtin_ia32_vec_set_v4hi(A, D, 0)
#define __builtin_ia32_vec_ext_v4hi(A, N) __builtin_ia32_vec_ext_v4hi(A, 0)
#define __builtin_ia32_shufps(A, B, N) __builtin_ia32_shufps(A, B, 0)

#include <ammintrin.h>
#include <tmmintrin.h>
