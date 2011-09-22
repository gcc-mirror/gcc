/* Copyright (C) 2011 Free Software Foundation, Inc.

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

typedef int __v2si __attribute__ ((__vector_size__ (8)));
typedef short __v4hi __attribute__ ((__vector_size__ (8)));
typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef unsigned char __v8qi __attribute__ ((__vector_size__ (8)));
typedef unsigned char __v4qi __attribute__ ((__vector_size__ (4)));
typedef int __i64 __attribute__ ((__mode__ (DI)));

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

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge8 (void *__A, void *__B)
{
  return __builtin_vis_edge8 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge8l (void *__A, void *__B)
{
  return __builtin_vis_edge8l (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge16 (void *__A, void *__B)
{
  return __builtin_vis_edge16 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge16l (void *__A, void *__B)
{
  return __builtin_vis_edge16l (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge32 (void *__A, void *__B)
{
  return __builtin_vis_edge32 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_edge32l (void *__A, void *__B)
{
  return __builtin_vis_edge32l (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmple16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmple16 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmple32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmple32 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpne16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmpne16 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpne32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmpne32 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpgt16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmpgt16 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpgt32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmpgt32 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpeq16 (__v4hi __A, __v4hi __B)
{
  return __builtin_vis_fcmpeq16 (__A, __B);
}

extern __inline int
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
__vis_fcmpeq32 (__v2si __A, __v2si __B)
{
  return __builtin_vis_fcmpeq32 (__A, __B);
}

#endif  /* _VISINTRIN_H_INCLUDED */
