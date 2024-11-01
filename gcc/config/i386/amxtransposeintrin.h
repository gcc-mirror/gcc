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
#error "Never use <amxtransposeintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AMXTRANSPOSEINTRIN_H_INCLUDED
#define _AMXTRANSPOSEINTRIN_H_INCLUDED

#if !defined(__AMX_TRANSPOSE__)
#pragma GCC push_options
#pragma GCC target("amx-transpose")
#define __DISABLE_AMX_TRANSPOSE__
#endif /* __AMX_TRANSPOSE__ */

#if defined(__x86_64__)
#define _tile_transposed_internal(dst,src)					\
  __asm__ volatile\
  ("{ttransposed\t%%tmm"#src", %%tmm"#dst"|ttransposed\t%%tmm"#dst", %%tmm"#src"}" ::)

#define _tile_2rpntlvwz0_internal(dst,base,stride)				\
  __asm__ volatile\
  ("{t2rpntlvwz0\t(%0,%1,1), %%tmm"#dst"|t2rpntlvwz0\t%%tmm"#dst", [%0+%1*1]}"  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_2rpntlvwz0t1_internal(dst,base,stride)				\
  __asm__ volatile\
  ("{t2rpntlvwz0t1\t(%0,%1,1), %%tmm"#dst"|t2rpntlvwz0t1\t%%tmm"#dst", [%0+%1*1]}" \
   :: "r" ((const void*)(base)), "r" ((long)(stride)))

#define _tile_2rpntlvwz1_internal(dst,base,stride)				\
  __asm__ volatile\
  ("{t2rpntlvwz1\t(%0,%1,1), %%tmm"#dst"|t2rpntlvwz1\t%%tmm"#dst", [%0+%1*1]}"  \
  :: "r" ((const void*)(base)), "r" ((long)(stride)))

#define _tile_2rpntlvwz1t1_internal(dst,base,stride)				\
  __asm__ volatile\
  ("{t2rpntlvwz1t1\t(%0,%1,1), %%tmm"#dst"|t2rpntlvwz1t1\t%%tmm"#dst", [%0+%1*1]}" \
  :: "r" ((const void*)(base)), "r" ((long)(stride)))

#define _tile_transposed(dst,src)						\
  _tile_transposed_internal (dst, src)

#define _tile_2rpntlvwz0(dst,base,stride)					\
  _tile_2rpntlvwz0_internal (dst, base, stride)

#define _tile_2rpntlvwz0t1(dst,base,stride)					\
  _tile_2rpntlvwz0t1_internal (dst, base, stride)

#define _tile_2rpntlvwz1(dst,base,stride)					\
  _tile_2rpntlvwz1_internal (dst, base, stride)

#define _tile_2rpntlvwz1t1(dst,base,stride)					\
  _tile_2rpntlvwz1t1_internal (dst, base, stride)

#if !defined(__AMX_BF16__)
#pragma GCC push_options
#pragma GCC target("amx-bf16")
#define __DISABLE_AMX_BF16__
#endif /* __AMX_BF16__ */

#define _tile_tdpbf16ps_internal(src1_dst,src2,src3)				\
  __asm__ volatile\
  ("{ttdpbf16ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|ttdpbf16ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_tdpbf16ps(src1_dst,src2,src3)					\
  _tile_tdpbf16ps_internal (src1_dst, src2, src3)

#ifdef __DISABLE_AMX_BF16__
#undef __DISABLE_AMX_BF16__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_BF16__ */

#if !defined(__AMX_FP16__)
#pragma GCC push_options
#pragma GCC target("amx-fp16")
#define __DISABLE_AMX_FP16__
#endif /* __AMX_FP16__ */

#define _tile_tdpfp16ps_internal(src1_dst,src2,src3)				\
  __asm__ volatile\
  ("{ttdpfp16ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|ttdpfp16ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_tdpfp16ps(src1_dst,src2,src3)					\
  _tile_tdpfp16ps_internal (src1_dst, src2, src3)

#ifdef __DISABLE_AMX_FP16__
#undef __DISABLE_AMX_FP16__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_FP16__ */

#if !defined(__AMX_COMPLEX__)
#pragma GCC push_options
#pragma GCC target("amx-complex")
#define __DISABLE_AMX_COMPLEX__
#endif /* __AMX_COMPLEX__ */

#define _tile_conjtcmmimfp16ps_internal(src1_dst,src2,src3)			\
  __asm__ volatile\
  ("{tconjtcmmimfp16ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|tconjtcmmimfp16ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_conjtfp16_internal(dst,src)					\
  __asm__ volatile\
  ("{tconjtfp16\t%%tmm"#src", %%tmm"#dst"|tconjtfp16\t%%tmm"#dst", %%tmm"#src"}" ::)

#define _tile_tcmmimfp16ps_internal(src1_dst,src2,src3)				\
  __asm__ volatile\
  ("{ttcmmimfp16ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|ttcmmimfp16ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_tcmmrlfp16ps_internal(src1_dst,src2,src3)				\
  __asm__ volatile\
  ("{ttcmmrlfp16ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|ttcmmrlfp16ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_conjtcmmimfp16ps(src1_dst,src2,src3)				\
  _tile_conjtcmmimfp16ps_internal (src1_dst, src2, src3)

#define _tile_conjtfp16(dst,src)						\
  _tile_conjtfp16_internal (dst, src)

#define _tile_tcmmimfp16ps(src1_dst,src2,src3)					\
  _tile_tcmmimfp16ps_internal (src1_dst, src2, src3)

#define _tile_tcmmrlfp16ps(src1_dst,src2,src3)					\
  _tile_tcmmrlfp16ps_internal (src1_dst, src2, src3)

#ifdef __DISABLE_AMX_COMPLEX__
#undef __DISABLE_AMX_COMPLEX__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_COMPLEX__ */

#if !defined(__AMX_TF32__)
#pragma GCC push_options
#pragma GCC target("amx-tf32")
#define __DISABLE_AMX_TF32__
#endif /* __AMX_TF32__ */

#define _tile_tmmultf32ps_internal(src1_dst,src2,src3)				\
  __asm__ volatile\
  ("{ttmmultf32ps\t%%tmm"#src3", %%tmm"#src2", %%tmm"#src1_dst"|ttmmultf32ps\t%%tmm"#src1_dst", %%tmm"#src2", %%tmm"#src3"}" ::)

#define _tile_tmmultf32ps(src1_dst,src2,src3)					\
  _tile_tmmultf32ps_internal (src1_dst, src2, src3)

#ifdef __DISABLE_AMX_TF32__
#undef __DISABLE_AMX_TF32__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_TF32__ */

#endif /* __x86_64__ */

#ifdef __DISABLE_AMX_TRANSPOSE__
#undef __DISABLE_AMX_TRANSPOSE__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_TRANSPOSE__ */

#endif /* _AMXTRANSPOSEINTRIN_H_INCLUDED */
