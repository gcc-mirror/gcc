/* Copyright (C) 2003-2023 Free Software Foundation, Inc.

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

/* Implemented from the specification included in the Intel C++ Compiler
   User Guide and Reference, version 9.0.  */

#ifndef NO_WARN_X86_INTRINSICS
/* This header is distributed to simplify porting x86_64 code that
   makes explicit use of Intel intrinsics to powerpc64le.
   It is the user's responsibility to determine if the results are
   acceptable and make additional changes as necessary.
   Note that much code that uses Intel intrinsics can be rewritten in
   standard C or GNU C extensions, which are more portable and better
   optimized across multiple targets.

   In the specific case of X86 SSE3 intrinsics, the PowerPC VMX/VSX ISA
   is a good match for most SIMD operations.  However the Horizontal
   add/sub requires the data pairs be permuted into a separate
   registers with vertical even/odd alignment for the operation.
   And the addsub operation requires the sign of only the even numbered
   elements be flipped (xored with -0.0).
   For larger blocks of code using these intrinsic implementations,
   the compiler be should be able to schedule instructions to avoid
   additional latency.

   In the specific case of the monitor and mwait instructions there are
   no direct equivalent in the PowerISA at this time.  So those
   intrinsics are not implemented.  */
#error "Please read comment above.  Use -DNO_WARN_X86_INTRINSICS to disable this warning."
#endif

#ifndef _PMMINTRIN_H_INCLUDED
#define _PMMINTRIN_H_INCLUDED

/* We need definitions from the SSE2 and SSE header files*/
#include <emmintrin.h>

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_addsub_ps (__m128 __X, __m128 __Y)
{
  const __v4sf __even_n0 = {-0.0, 0.0, -0.0, 0.0};
  __v4sf __even_neg_Y = vec_xor(__Y, __even_n0);
  return (__m128) vec_add (__X, __even_neg_Y);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_addsub_pd (__m128d __X, __m128d __Y)
{
  const __v2df __even_n0 = {-0.0, 0.0};
  __v2df __even_neg_Y = vec_xor(__Y, __even_n0);
  return (__m128d) vec_add (__X, __even_neg_Y);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hadd_ps (__m128 __X, __m128 __Y)
{
  __vector unsigned char __xform2 = {
      0x00, 0x01, 0x02, 0x03,
      0x08, 0x09, 0x0A, 0x0B,
      0x10, 0x11, 0x12, 0x13,
      0x18, 0x19, 0x1A, 0x1B
    };
  __vector unsigned char __xform1 = {
      0x04, 0x05, 0x06, 0x07,
      0x0C, 0x0D, 0x0E, 0x0F,
      0x14, 0x15, 0x16, 0x17,
      0x1C, 0x1D, 0x1E, 0x1F
    };
  return (__m128) vec_add (vec_perm ((__v4sf) __X, (__v4sf) __Y, __xform2),
			   vec_perm ((__v4sf) __X, (__v4sf) __Y, __xform1));
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hsub_ps (__m128 __X, __m128 __Y)
{
  __vector unsigned char __xform2 = {
      0x00, 0x01, 0x02, 0x03,
      0x08, 0x09, 0x0A, 0x0B,
      0x10, 0x11, 0x12, 0x13,
      0x18, 0x19, 0x1A, 0x1B
    };
  __vector unsigned char __xform1 = {
      0x04, 0x05, 0x06, 0x07,
      0x0C, 0x0D, 0x0E, 0x0F,
      0x14, 0x15, 0x16, 0x17,
      0x1C, 0x1D, 0x1E, 0x1F
    };
  return (__m128) vec_sub (vec_perm ((__v4sf) __X, (__v4sf) __Y, __xform2),
			   vec_perm ((__v4sf) __X, (__v4sf) __Y, __xform1));
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hadd_pd (__m128d __X, __m128d __Y)
{
  return (__m128d) vec_add (vec_mergeh ((__v2df) __X, (__v2df)__Y),
				  vec_mergel ((__v2df) __X, (__v2df)__Y));
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hsub_pd (__m128d __X, __m128d __Y)
{
  return (__m128d) vec_sub (vec_mergeh ((__v2df) __X, (__v2df)__Y),
			    vec_mergel ((__v2df) __X, (__v2df)__Y));
}

#ifdef _ARCH_PWR8
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_movehdup_ps (__m128 __X)
{
  return (__m128)vec_mergeo ((__v4su)__X, (__v4su)__X);
}
#endif

#ifdef _ARCH_PWR8
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_moveldup_ps (__m128 __X)
{
  return (__m128)vec_mergee ((__v4su)__X, (__v4su)__X);
}
#endif

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_loaddup_pd (double const *__P)
{
  return (__m128d) vec_splats (*__P);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_movedup_pd (__m128d __X)
{
  return _mm_shuffle_pd (__X, __X, _MM_SHUFFLE2 (0,0));
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_lddqu_si128 (__m128i const *__P)
{
  return (__m128i) (vec_vsx_ld(0, (signed int const *)__P));
}

/* POWER8 / POWER9 have no equivalent for _mm_monitor nor _mm_wait.  */

#endif /* _PMMINTRIN_H_INCLUDED */
