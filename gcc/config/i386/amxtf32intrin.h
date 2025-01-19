/* Copyright (C) 2024-2025 Free Software Foundation, Inc.
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
#error "Never use <amxtf32intrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AMXTF32INTRIN_H_INCLUDED
#define _AMXTF32INTRIN_H_INCLUDED

#if !defined(__AMX_TF32__)
#pragma GCC push_options
#pragma GCC target("amx-tf32")
#define __DISABLE_AMX_TF32__
#endif /* __AMX_TF32__ */

#if defined(__x86_64__)
#define _tile_mmultf32ps_internal(src1_dst,src2,src3)			\
  __asm__ volatile\
  ("{tmmultf32ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|tmmultf32ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_mmultf32ps(src1_dst,src2,src3)				\
  _tile_mmultf32ps_internal (src1_dst, src2, src3)

#endif

#ifdef __DISABLE_AMX_TF32__
#undef __DISABLE_AMX_TF32__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_TF32__ */

#endif /* _AMXTF32INTRIN_H_INCLUDED */
