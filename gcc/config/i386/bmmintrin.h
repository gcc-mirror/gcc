/* Copyright (C) 2007 Free Software Foundation, Inc.

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
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */

#ifndef _BMMINTRIN_H_INCLUDED
#define _BMMINTRIN_H_INCLUDED

#ifndef __SSE5__
# error "SSE5 instruction set not enabled"
#else

/* We need definitions from the SSE4A, SSE3, SSE2 and SSE header files.  */
#include <ammintrin.h>
#include <mmintrin-common.h>

/* Floating point multiply/add type instructions */
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_ps(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fmaddps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_pd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fmaddpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_ss(__m128 __A, __m128 __B, __m128 __C)
{
  return  (__m128) __builtin_ia32_fmaddss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_sd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fmaddsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_ps(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fmsubps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_pd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fmsubpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_ss(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fmsubss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_msub_sd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fmsubsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_ps(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fnmaddps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_pd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fnmaddpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_ss(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fnmaddss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmacc_sd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fnmaddsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_ps(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fnmsubps ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_pd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fnmsubpd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_ss(__m128 __A, __m128 __B, __m128 __C)
{
  return (__m128) __builtin_ia32_fnmsubss ((__v4sf)__A, (__v4sf)__B, (__v4sf)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_nmsub_sd(__m128d __A, __m128d __B, __m128d __C)
{
  return (__m128d) __builtin_ia32_fnmsubsd ((__v2df)__A, (__v2df)__B, (__v2df)__C);
}

/* Integer multiply/add intructions. */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maccs_epi16(__m128i __A, __m128i __B, __m128i __C)
{
  return (__m128i) __builtin_ia32_pmacssww ((__v8hi)__A,(__v8hi)__B, (__v8hi)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_epi16(__m128i __A, __m128i __B, __m128i __C)
{
  return (__m128i) __builtin_ia32_pmacsww ((__v8hi)__A, (__v8hi)__B, (__v8hi)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maccsd_epi16(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacsswd ((__v8hi)__A, (__v8hi)__B, (__v4si)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maccd_epi16(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacswd ((__v8hi)__A, (__v8hi)__B, (__v4si)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maccs_epi32(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacssdd ((__v4si)__A, (__v4si)__B, (__v4si)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macc_epi32(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacsdd ((__v4si)__A, (__v4si)__B, (__v4si)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maccslo_epi32(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacssdql ((__v4si)__A, (__v4si)__B, (__v2di)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macclo_epi32(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacsdql ((__v4si)__A, (__v4si)__B, (__v2di)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maccshi_epi32(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacssdqh ((__v4si)__A, (__v4si)__B, (__v2di)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_macchi_epi32(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmacsdqh ((__v4si)__A, (__v4si)__B, (__v2di)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maddsd_epi16(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmadcsswd ((__v8hi)__A,(__v8hi)__B,(__v4si)__C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_maddd_epi16(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pmadcswd ((__v8hi)__A,(__v8hi)__B,(__v4si)__C);
}

/* Packed Integer Horizontal Add and Subtract */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddw_epi8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddbw ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddd_epi8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddbd ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddq_epi8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddbq ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddd_epi16(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddwd ((__v8hi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddq_epi16(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddwq ((__v8hi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddq_epi32(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phadddq ((__v4si)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddw_epu8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddubw ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddd_epu8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddubd ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddq_epu8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddubq ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddd_epu16(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phadduwd ((__v8hi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddq_epu16(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phadduwq ((__v8hi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_haddq_epu32(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phaddudq ((__v4si)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hsubw_epi8(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phsubbw ((__v16qi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hsubd_epi16(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phsubwd ((__v8hi)__A);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_hsubq_epi32(__m128i __A)
{
  return  (__m128i) __builtin_ia32_phsubdq ((__v4si)__A);
}

/* Vector conditional move and permute */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_cmov_si128(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pcmov (__A, __B, __C);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_perm_epi8(__m128i __A, __m128i __B, __m128i __C)
{
  return  (__m128i) __builtin_ia32_pperm ((__v16qi)__A, (__v16qi)__B, (__v16qi)__C);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_perm_ps(__m128 __A, __m128 __B, __m128i __C)
{
  return  (__m128) __builtin_ia32_permps ((__m128)__A, (__m128)__B, (__v16qi)__C);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_perm_pd(__m128d __A, __m128d __B, __m128i __C)
{
  return  (__m128d) __builtin_ia32_permpd ((__m128d)__A, (__m128d)__B, (__v16qi)__C);
}

/* Packed Integer Rotates and Shifts */

/* Rotates - Non-Immediate form */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_rot_epi8(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_protb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_rot_epi16(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_protw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_rot_epi32(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_protd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_rot_epi64(__m128i __A,  __m128i __B)
{
  return (__m128i)  __builtin_ia32_protq ((__v2di)__A, (__v2di)__B);
}


/* Rotates - Immediate form */
#ifdef __OPTIMIZE__
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_roti_epi8(__m128i __A, const int __B)
{
  return  (__m128i) __builtin_ia32_protbi ((__v16qi)__A, __B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_roti_epi16(__m128i __A, const int __B)
{
  return  (__m128i) __builtin_ia32_protwi ((__v8hi)__A, __B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_roti_epi32(__m128i __A, const int __B)
{
  return  (__m128i) __builtin_ia32_protdi ((__v4si)__A, __B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_roti_epi64(__m128i __A, const int __B)
{
  return  (__m128i) __builtin_ia32_protqi ((__v2di)__A, __B);
}
#else
#define _mm_roti_epi8(A, N) \
  ((__m128i) __builtin_ia32_protbi ((__v16qi)(__m128i)(A), (int)(N)))
#define _mm_roti_epi16(A, N) \
  ((__m128i) __builtin_ia32_protwi ((__v8hi)(__m128i)(A), (int)(N)))
#define _mm_roti_epi32(A, N) \
  ((__m128i) __builtin_ia32_protdi ((__v4si)(__m128i)(A), (int)(N)))
#define _mm_roti_epi64(A, N) \
  ((__m128i) __builtin_ia32_protqi ((__v2di)(__m128i)(A), (int)(N)))
#endif

/* pshl */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_shl_epi8(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshlb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_shl_epi16(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshlw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_shl_epi32(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshld ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_shl_epi64(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshlq ((__v2di)__A, (__v2di)__B);
}

/* psha */
extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sha_epi8(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshab ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sha_epi16(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshaw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sha_epi32(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshad ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_sha_epi64(__m128i __A,  __m128i __B)
{
  return  (__m128i) __builtin_ia32_pshaq ((__v2di)__A, (__v2di)__B);
}

/* Compare and Predicate Generation */

/* com (floating point, packed single) */
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_ps(__m128 __A, __m128 __B)
{
  return  (__m128) __builtin_ia32_comeqps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_ps(__m128 __A, __m128 __B)
{
  return  (__m128) __builtin_ia32_comltps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comleps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comunord_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comunordps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comuneqps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnlt_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comunltps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnle_ps(__m128 __A, __m128 __B)
{
  return (__m128)  __builtin_ia32_comunleps ((__v4sf)__A, (__v4sf)__B);
}


extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comord_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comordps ((__v4sf)__A, (__v4sf)__B);
}


extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comueq_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comueqps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnge_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comungeps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comngt_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comungtps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comfalseps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comoneq_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comneqps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comgeps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comgtps ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_ps(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comtrueps ((__v4sf)__A, (__v4sf)__B);
}

/* com (floating point, packed double) */

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comeqpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comltpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comlepd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comunord_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comunordpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comuneqpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnlt_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comunltpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnle_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comunlepd ((__v2df)__A, (__v2df)__B);
}


extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comord_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comordpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comueq_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comueqpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnge_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comungepd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comngt_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comungtpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comfalsepd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comoneq_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comneqpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comgepd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comgtpd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_pd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comtruepd ((__v2df)__A, (__v2df)__B);
}

/* com (floating point, scalar single) */
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_ss(__m128 __A, __m128 __B)
{
  return (__m128)  __builtin_ia32_comeqss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comltss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comless ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comunord_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comunordss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comuneqss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnlt_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comunltss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnle_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comunless ((__v4sf)__A, (__v4sf)__B);
}


extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comord_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comordss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comueq_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comueqss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnge_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comungess ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comngt_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comungtss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comfalsess ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comoneq_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comneqss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comgess ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comgtss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_ss(__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_comtruess ((__v4sf)__A, (__v4sf)__B);
}

/* com (floating point, scalar double) */

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comeqsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comltsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comlesd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comunord_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comunordsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comuneqsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnlt_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comunltsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnle_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comunlesd ((__v2df)__A, (__v2df)__B);
}


extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comord_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comordsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comueq_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comueqsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comnge_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comungesd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comngt_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comungtsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comfalsesd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comoneq_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comneqsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comgesd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comgtsd ((__v2df)__A, (__v2df)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_sd(__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_comtruesd ((__v2df)__A, (__v2df)__B);
}


/*pcom (integer, unsinged bytes) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltub ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomleub ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtub ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgeub ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomequb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomnequb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalseub ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epu8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrueub ((__v16qi)__A, (__v16qi)__B);
}

/*pcom (integer, unsinged words) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltuw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomleuw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtuw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgeuw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomequw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomnequw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalseuw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epu16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrueuw ((__v8hi)__A, (__v8hi)__B);
}

/*pcom (integer, unsinged double words) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltud ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomleud ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtud ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgeud ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomequd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomnequd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalseud ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epu32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrueud ((__v4si)__A, (__v4si)__B);
}

/*pcom (integer, unsinged quad words) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltuq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomleuq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtuq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgeuq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomequq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomnequq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalseuq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epu64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrueuq ((__v2di)__A, (__v2di)__B);
}

/*pcom (integer, signed bytes) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomleb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgeb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomeqb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomneqb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalseb ((__v16qi)__A, (__v16qi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epi8(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrueb ((__v16qi)__A, (__v16qi)__B);
}

/*pcom (integer, signed words) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomlew ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgew ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomeqw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomneqw ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalsew ((__v8hi)__A, (__v8hi)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epi16(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtruew ((__v8hi)__A, (__v8hi)__B);
}

/*pcom (integer, signed double words) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomled ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomged ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomeqd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomneqd ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalsed ((__v4si)__A, (__v4si)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epi32(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrued ((__v4si)__A, (__v4si)__B);
}

/*pcom (integer, signed quad words) */

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comlt_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomltq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comle_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomleq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comgt_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgtq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comge_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomgeq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comeq_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomeqq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comneq_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomneqq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comfalse_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomfalseq ((__v2di)__A, (__v2di)__B);
}

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_comtrue_epi64(__m128i __A, __m128i __B)
{
  return (__m128i) __builtin_ia32_pcomtrueq ((__v2di)__A, (__v2di)__B);
}

/* FRCZ */
extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_frcz_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_frczps ((__v4sf)__A);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_frcz_pd (__m128d __A)
{
  return (__m128d) __builtin_ia32_frczpd ((__v2df)__A);
}

extern __inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_frcz_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_frczss ((__v4sf)__A, (__v4sf)__B);
}

extern __inline __m128d __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_frcz_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_frczsd ((__v2df)__A, (__v2df)__B);
}

#endif /* __SSE5__ */

#endif /* _BMMINTRIN_H_INCLUDED */
