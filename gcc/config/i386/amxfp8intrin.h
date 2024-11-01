/* Copyright (C) 2024 Free Software Foundation, Inc.

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

#if !defined _IMMINTRIN_H_INCLUDED
#error "Never use <amxfp8intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AMXFP8INTRIN_H_INCLUDED
#define _AMXFP8INTRIN_H_INCLUDED

#if defined(__x86_64__)
#define _tile_dpbf8ps_internal(dst,src1,src2)			\
  __asm__ volatile \
  ("{tdpbf8ps\t%%tmm"#src2", %%tmm"#src1", %%tmm"#dst"|tdpbf8ps\t%%tmm"#dst", %%tmm"#src1", %%tmm"#src2"}" ::)

#define _tile_dpbhf8ps_internal(dst,src1,src2)			\
  __asm__ volatile \
  ("{tdpbhf8ps\t%%tmm"#src2", %%tmm"#src1", %%tmm"#dst"|tdpbhf8ps\t%%tmm"#dst", %%tmm"#src1", %%tmm"#src2"}" ::)

#define _tile_dphbf8ps_internal(dst,src1,src2)			\
  __asm__ volatile \
  ("{tdphbf8ps\t%%tmm"#src2", %%tmm"#src1", %%tmm"#dst"|tdphbf8ps\t%%tmm"#dst", %%tmm"#src1", %%tmm"#src2"}" ::)

#define _tile_dphf8ps_internal(dst,src1,src2)			\
  __asm__ volatile \
  ("{tdphf8ps\t%%tmm"#src2", %%tmm"#src1", %%tmm"#dst"|tdphf8ps\t%%tmm"#dst", %%tmm"#src1", %%tmm"#src2"}" ::)

#define _tile_dpbf8ps(dst,src1,src2)				\
  _tile_dpbf8ps_internal (dst,src1,src2)

#define _tile_dpbhf8ps(dst,src1,src2)				\
  _tile_dpbhf8ps_internal (dst,src1,src2)

#define _tile_dphbf8ps(dst,src1,src2)				\
  _tile_dphbf8ps_internal (dst,src1,src2)

#define _tile_dphf8ps(dst,src1,src2)				\
  _tile_dphf8ps_internal (dst,src1,src2)

#endif

#ifdef __DISABLE_AMX_FP8__
#undef __DISABLE_AMX_FP8__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_FP8__ */

#endif /* _AMXFP8INTRIN_H_INCLUDED */
