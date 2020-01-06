/* Copyright (C) 2011-2020 Free Software Foundation, Inc.

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

#ifndef _VISINTRIN_H_INCLUDED
#define _VISINTRIN_H_INCLUDED

#ifdef __VIS__

typedef int __v2si __attribute__ ((__vector_size__ (8)));
typedef int __v1si __attribute__ ((__vector_size__ (4)));
typedef short __v4hi __attribute__ ((__vector_size__ (8)));
typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef unsigned char __v8qi __attribute__ ((__vector_size__ (8)));
typedef unsigned char __v4qi __attribute__ ((__vector_size__ (4)));
typedef int __i64 __attribute__ ((__mode__ (DI)));

#if __VIS__ >= 0x200

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_write_gsr (__i64 __A)
{
  __builtin_vis_write_gsr (__A);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_read_gsr (void)
{
  return __builtin_vis_read_gsr ();
}

#endif /* __VIS__ >= 0x200 */

extern __inline void *
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_alignaddr (void *__A, long __B)
{
  return __builtin_vis_alignaddr (__A, __B);
}

extern __inline void *
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_alignaddrl (void *__A, long __B)
{
  return __builtin_vis_alignaddrl (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_faligndatadi (__i64 __A, __i64 __B)
{
  return __builtin_vis_faligndatadi (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_faligndatav2si (__v2si __A, __v2si __B)
{
  return __builtin_vis_faligndatav2si (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_faligndatav4hi (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_faligndatav4hi (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_faligndatav8qi (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_faligndatav8qi (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fexpand (__v4qi __A)
{
  return __builtin_vis_fexpand (__A);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmul8x16 (__v4qi __A, __v4hi __B)
{
  return __builtin_vis_fmul8x16 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmul8x16au (__v4qi __A, __v2hi __B)
{
  return __builtin_vis_fmul8x16au (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmul8x16al (__v4qi __A, __v2hi __B)
{
  return __builtin_vis_fmul8x16al (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmul8sux16 (__v8qi __A, __v4hi __B)
{
  return __builtin_vis_fmul8sux16 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmul8ulx16 (__v8qi __A, __v4hi __B)
{
  return __builtin_vis_fmul8ulx16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmuld8sux16 (__v4qi __A, __v2hi __B)
{
  return __builtin_vis_fmuld8sux16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmuld8ulx16 (__v4qi __A, __v2hi __B)
{
  return __builtin_vis_fmuld8ulx16 (__A, __B);
}

extern __inline __v4qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpack16 (__v4hi __A)
{
  return __builtin_vis_fpack16 (__A);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpack32 (__v2si __A, __v8qi __B)
{
  return __builtin_vis_fpack32 (__A, __B);
}

extern __inline __v2hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpackfix (__v2si __A)
{
  return __builtin_vis_fpackfix (__A);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmerge (__v4qi __A, __v4qi __B)
{
  return __builtin_vis_fpmerge (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_pdist (__v8qi __A, __v8qi __B, __i64 __C)
{
  return __builtin_vis_pdist (__A, __B, __C);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge8 (void *__A, void *__B)
{
  return __builtin_vis_edge8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge8l (void *__A, void *__B)
{
  return __builtin_vis_edge8l (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge16 (void *__A, void *__B)
{
  return __builtin_vis_edge16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge16l (void *__A, void *__B)
{
  return __builtin_vis_edge16l (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge32 (void *__A, void *__B)
{
  return __builtin_vis_edge32 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge32l (void *__A, void *__B)
{
  return __builtin_vis_edge32l (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmple16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmple16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmple32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmple32 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpne16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmpne16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpne32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmpne32 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpgt16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmpgt16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpgt32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmpgt32 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpeq16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmpeq16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpeq32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmpeq32 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadd16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpadd16 (__A, __B);
}

extern __inline __v2hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadd16s (__v2hi __A, __v2hi __B)
{
  return __builtin_vis_fpadd16s (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadd32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpadd32 (__A, __B);
}

extern __inline __v1si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadd32s (__v1si __A, __v1si __B)
{
  return __builtin_vis_fpadd32s (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsub16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpsub16 (__A, __B);
}

extern __inline __v2hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsub16s (__v2hi __A, __v2hi __B)
{
  return __builtin_vis_fpsub16s (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsub32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpsub32 (__A, __B);
}

extern __inline __v1si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsub32s (__v1si __A, __v1si __B)
{
  return __builtin_vis_fpsub32s (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_array8 (long __A, long __B)
{
  return __builtin_vis_array8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_array16 (long __A, long __B)
{
  return __builtin_vis_array16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_array32 (long __A, long __B)
{
  return __builtin_vis_array32 (__A, __B);
}

#if __VIS__ >= 0x200

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_bmask (long __A, long __B)
{
  return __builtin_vis_bmask (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_bshuffledi (__i64 __A, __i64 __B)
{
  return __builtin_vis_bshuffledi (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_bshufflev2si (__v2si __A, __v2si __B)
{
  return __builtin_vis_bshufflev2si (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_bshufflev4hi (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_bshufflev4hi (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_bshufflev8qi (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_bshufflev8qi (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge8n (void *__A, void *__B)
{
  return __builtin_vis_edge8n (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge8ln (void *__A, void *__B)
{
  return __builtin_vis_edge8ln (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge16n (void *__A, void *__B)
{
  return __builtin_vis_edge16n (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge16ln (void *__A, void *__B)
{
  return __builtin_vis_edge16ln (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge32n (void *__A, void *__B)
{
  return __builtin_vis_edge32n (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge32ln (void *__A, void *__B)
{
  return __builtin_vis_edge32ln (__A, __B);
}

#endif /* __VIS__ >= 0x200 */

#if __VIS__ >= 0x300

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_cmask8 (long __A)
{
  return __builtin_vis_cmask8 (__A);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_cmask16 (long __A)
{
  return __builtin_vis_cmask16 (__A);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_cmask32 (long __A)
{
  return __builtin_vis_cmask32 (__A);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fchksm16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fchksm16 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fsll16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fsll16 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fslas16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fslas16 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fsrl16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fsrl16 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fsra16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fsra16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fsll32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fsll32 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fslas32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fslas32 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fsrl32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fsrl32 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fsra32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fsra32 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_pdistn (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_pdistn (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fmean16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fmean16 (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadd64 (__i64 __A, __i64 __B)
{
  return __builtin_vis_fpadd64 (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsub64 (__i64 __A, __i64 __B)
{
  return __builtin_vis_fpsub64 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadds16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpadds16 (__A, __B);
}

extern __inline __v2hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadds16s (__v2hi __A, __v2hi __B)
{
  return __builtin_vis_fpadds16s (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubs16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpsubs16 (__A, __B);
}

extern __inline __v2hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubs16s (__v2hi __A, __v2hi __B)
{
  return __builtin_vis_fpsubs16s (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadds32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpadds32 (__A, __B);
}

extern __inline __v1si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadds32s (__v1si __A, __v1si __B)
{
  return __builtin_vis_fpadds32s (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubs32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpsubs32 (__A, __B);
}

extern __inline __v1si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubs32s (__v1si __A, __v1si __B)
{
  return __builtin_vis_fpsubs32s (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fucmple8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fucmple8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fucmpne8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fucmpne8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fucmpgt8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fucmpgt8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fucmpeq8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fucmpeq8 (__A, __B);
}

extern __inline float
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fhadds (float __A, float __B)
{
  return __builtin_vis_fhadds (__A, __B);
}

extern __inline double
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fhaddd (double __A, double __B)
{
  return __builtin_vis_fhaddd (__A, __B);
}

extern __inline float
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fhsubs (float __A, float __B)
{
  return __builtin_vis_fhsubs (__A, __B);
}

extern __inline double
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fhsubd (double __A, double __B)
{
  return __builtin_vis_fhsubd (__A, __B);
}

extern __inline float
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fnhadds (float __A, float __B)
{
  return __builtin_vis_fnhadds (__A, __B);
}

extern __inline double
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fnhaddd (double __A, double __B)
{
  return __builtin_vis_fnhaddd (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_umulxhi (__i64 __A, __i64 __B)
{
  return __builtin_vis_umulxhi (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_xmulx (__i64 __A, __i64 __B)
{
  return __builtin_vis_xmulx (__A, __B);
}

extern __inline __i64
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_xmulxhi (__i64 __A, __i64 __B)
{
  return __builtin_vis_xmulxhi (__A, __B);
}

#endif /* __VIS__ >= 0x300 */

#if __VIS__ >= 0x400

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadd8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpadd8 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpadds8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpadds8 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpaddus8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpaddus8 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpaddus16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpaddus16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpcmple8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpcmple8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpcmpgt8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpcmpgt8 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpcmpule16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpcmpule16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpcmpugt16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpcmpugt16 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpcmpule32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpcmpule32 (__A, __B);
}

extern __inline long
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpcmpugt32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpcmpugt32 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmax8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpmax8 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmax16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpmax16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmax32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpmax32 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmaxu8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpmaxu8 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmaxu16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpmaxu16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmaxu32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpmaxu32 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmin8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpmin8 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmin16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpmin16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpmin32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpmin32 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpminu8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpminu8 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpminu16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpminu16 (__A, __B);
}

extern __inline __v2si
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpminu32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fpminu32 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsub8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpsub8 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubs8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpsubs8 (__A, __B);
}

extern __inline __v8qi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubus8 (__v8qi __A, __v8qi __B)
{
  return __builtin_vis_fpsubus8 (__A, __B);
}

extern __inline __v4hi
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fpsubus16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fpsubus16 (__A, __B);
}

#endif /* __VIS__ >= 0x400 */

#endif /* __VIS__ */

#endif  /* _VISINTRIN_H_INCLUDED */
