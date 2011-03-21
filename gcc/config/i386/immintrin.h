/* Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _IMMINTRIN_H_INCLUDED
#define _IMMINTRIN_H_INCLUDED

#ifdef __MMX__
#include <mmintrin.h>
#endif

#ifdef __SSE__
#include <xmmintrin.h>
#endif

#ifdef __SSE2__
#include <emmintrin.h>
#endif

#ifdef __SSE3__
#include <pmmintrin.h>
#endif

#ifdef __SSSE3__
#include <tmmintrin.h>
#endif

#if defined (__SSE4_2__) || defined (__SSE4_1__)
#include <smmintrin.h>
#endif

#if defined (__AES__) || defined (__PCLMUL__)
#include <wmmintrin.h>
#endif

#ifdef __AVX__
#include <avxintrin.h>
#endif

#ifdef __RDRND__
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_rdrand16_step (unsigned short *__P)
{
  return __builtin_ia32_rdrand16_step (__P);
}

extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_rdrand32_step (unsigned int *__P)
{
  return __builtin_ia32_rdrand32_step (__P);
}
#endif /* __RDRND__ */

#ifdef  __x86_64__
#ifdef __FSGSBASE__
extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_readfsbase_u32 (void)
{
  return __builtin_ia32_rdfsbase32 ();
}

extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_readfsbase_u64 (void)
{
  return __builtin_ia32_rdfsbase64 ();
}

extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_readgsbase_u32 (void)
{
  return __builtin_ia32_rdgsbase32 ();
}

extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_readgsbase_u64 (void)
{
  return __builtin_ia32_rdgsbase64 ();
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_writefsbase_u32 (unsigned int __B)
{
  __builtin_ia32_wrfsbase32 (__B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_writefsbase_u64 (unsigned long long __B)
{
  __builtin_ia32_wrfsbase64 (__B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_writegsbase_u32 (unsigned int __B)
{
  __builtin_ia32_wrgsbase32 (__B);
}

extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_writegsbase_u64 (unsigned long long __B)
{
  __builtin_ia32_wrgsbase64 (__B);
}
#endif /* __FSGSBASE__ */

#ifdef __RDRND__
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
_rdrand64_step (unsigned long long *__P)
{
  return __builtin_ia32_rdrand64_step (__P);
}
#endif /* __RDRND__ */
#endif /* __x86_64__  */

#ifdef __F16C__
extern __inline float __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_cvtsh_ss (unsigned short __S)
{
  __v8hi __H = __extension__ (__v8hi){ __S, 0, 0, 0, 0, 0, 0, 0 };
  __v4sf __A = __builtin_ia32_vcvtph2ps (__H);
  return __builtin_ia32_vec_ext_v4sf (__A, 0);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtph_ps (__m128i __A)
{
  return (__m128) __builtin_ia32_vcvtph2ps ((__v8hi) __A);
}

extern __inline __m256 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtph_ps (__m128i __A)
{
  return (__m256) __builtin_ia32_vcvtph2ps256 ((__v8hi) __A);
}

#ifdef __OPTIMIZE__
extern __inline unsigned short __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_cvtss_sh (float __F, const int __I)
{
  __v4sf __A =  __extension__ (__v4sf){ __F, 0, 0, 0 };
  __v8hi __H = __builtin_ia32_vcvtps2ph (__A, __I);
  return (unsigned short) __builtin_ia32_vec_ext_v8hi (__H, 0);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_cvtps_ph (__m128 __A, const int __I)
{
  return (__m128i) __builtin_ia32_vcvtps2ph ((__v4sf) __A, __I);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm256_cvtps_ph (__m256 __A, const int __I)
{
  return (__m128i) __builtin_ia32_vcvtps2ph256 ((__v8sf) __A, __I);
}
#else
#define _cvtss_sh(__F, __I)						\
  (__extension__ 							\
   ({									\
      __v4sf __A =  __extension__ (__v4sf){ __F, 0, 0, 0 };		\
      __v8hi __H = __builtin_ia32_vcvtps2ph (__A, __I);			\
      (unsigned short) __builtin_ia32_vec_ext_v8hi (__H, 0);		\
    }))

#define _mm_cvtps_ph(A, I) \
  ((__m128i) __builtin_ia32_vcvtps2ph ((__v4sf)(__m128) A, (int) (I)))

#define _mm256_cvtps_ph(A, I) \
  ((__m128i) __builtin_ia32_vcvtps2ph256 ((__v8sf)(__m256) A, (int) (I)))
#endif

#endif /* __F16C__ */

#endif /* _IMMINTRIN_H_INCLUDED */
