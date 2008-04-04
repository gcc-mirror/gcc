/* Copyright (C) 2008 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

/* Implemented from the specification included in the Intel C++ Compiler
   User Guide and Reference, version 10.1.  */

#ifndef _WMMINTRIN_H_INCLUDED
#define _WMMINTRIN_H_INCLUDED

/* We need definitions from the SSE2 header file.  */
#include <emmintrin.h>

#if !defined (__AES__) && !defined (__PCLMUL__)
# error "AES/PCLMUL instructions not enabled"
#else

/* AES */

#ifdef __AES__
/* Performs 1 round of AES decryption of the first m128i using 
   the second m128i as a round key.  */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesdec_si128 (__m128i __X, __m128i __Y)
{
  return (__m128i) __builtin_ia32_aesdec128 ((__v2di)__X, (__v2di)__Y);
}

/* Performs the last round of AES decryption of the first m128i 
   using the second m128i as a round key.  */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesdeclast_si128 (__m128i __X, __m128i __Y)
{
  return (__m128i) __builtin_ia32_aesdeclast128 ((__v2di)__X,
						 (__v2di)__Y);
}

/* Performs 1 round of AES encryption of the first m128i using 
   the second m128i as a round key.  */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesenc_si128 (__m128i __X, __m128i __Y)
{
  return (__m128i) __builtin_ia32_aesenc128 ((__v2di)__X, (__v2di)__Y);
}

/* Performs the last round of AES encryption of the first m128i
   using the second m128i as a round key.  */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesenclast_si128 (__m128i __X, __m128i __Y)
{
  return (__m128i) __builtin_ia32_aesenclast128 ((__v2di)__X, (__v2di)__Y);
}

/* Performs the InverseMixColumn operation on the source m128i 
   and stores the result into m128i destination.  */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aesimc_si128 (__m128i __X)
{
  return (__m128i) __builtin_ia32_aesimc128 ((__v2di)__X);
}

/* Generates a m128i round key for the input m128i AES cipher key and
   byte round constant.  The second parameter must be a compile time
   constant.  */
#ifdef __OPTIMIZE__
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_aeskeygenassist_si128 (__m128i __X, const int __C)
{
  return (__m128i) __builtin_ia32_aeskeygenassist128 ((__v2di)__X, __C);
}
#else
#define _mm_aeskeygenassist_si128(X, C)					\
  ((__m128i) __builtin_ia32_aeskeygenassist128 ((__v2di)(__m128i)(X),	\
						(int)(C)))
#endif
#endif  /* __AES__ */

/* PCLMUL */

#ifdef __PCLMUL__
/* Performs carry-less integer multiplication of 64-bit halves of
   128-bit input operands.  The third parameter inducates which 64-bit
   haves of the input parameters v1 and v2 should be used. It must be
   a compile time constant.  */
#ifdef __OPTIMIZE__
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_clmulepi64_si128 (__m128i __X, __m128i __Y, const int __I)
{
  return (__m128i) __builtin_ia32_pclmulqdq128 ((__v2di)__X,
						(__v2di)__Y, __I);
}
#else
#define _mm_clmulepi64_si128(X, Y, I)					\
  ((__m128i) __builtin_ia32_pclmulqdq128 ((__v2di)(__m128i)(X),		\
					  (__v2di)(__m128i)(Y), (int)(I)))
#endif
#endif  /* __PCLMUL__  */

#endif /* __AES__/__PCLMUL__ */

#endif /* _WMMINTRIN_H_INCLUDED */
