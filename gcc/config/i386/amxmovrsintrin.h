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

#ifndef _IMMINTRIN_H_INCLUDED
#error "Never use <amxmovrsintrin.h> directly; include <immintrin.h> instead."
#endif

#ifndef _AMX_MOVRSINTRIN_H_INCLUDED
#define _AMX_MOVRSINTRIN_H_INCLUDED

#if defined(__x86_64__)

#if !defined(__AMX_MOVRS__)
#pragma GCC push_options
#pragma GCC target("amx-movrs")
#define __DISABLE_AMX_MOVRS__
#endif /* __AMX_MOVRS__ */

#define _tile_loaddrs_internal(tdst, base, stride)	  \
__asm__ volatile					  \
  ("{tileloaddrs\t(%0,%1,1), %%tmm"#tdst		  \
     "|tileloaddrs\t%%tmm"#tdst", [%0+%1*1]}"		  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_loaddrst1_internal(tdst, base, stride)	  \
__asm__ volatile					  \
  ("{tileloaddrst1\t(%0,%1,1), %%tmm"#tdst		  \
     "|tileloaddrst1\t%%tmm"#tdst", [%0+%1*1]}"		  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_loaddrs(tdst, base, stride)		  \
  _tile_loaddrs_internal(tdst, base, stride)

#define _tile_loaddrst1(tdst, base, stride)		  \
  _tile_loaddrst1_internal(tdst, base, stride)

#ifdef __DISABLE_AMX_MOVRS__
#undef __DISABLE_AMX_MOVRS__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_MOVRS__ */

#if !defined(__AMX_MOVRS__) || !defined (__AMX_TRANSPOSE__)
#pragma GCC push_options
#pragma GCC target("amx-movrs,amx-transpose")
#define __DISABLE_AMX_MOVRS_TRANSPOSE__
#endif /* __AMX_MOVRS_TRANSPOSE__ */

#define _tile_2rpntlvwz0rs_internal(tdst, base, stride)	  \
  __asm__ volatile					  \
  ("{t2rpntlvwz0rs\t(%0,%1,1), %%tmm"#tdst		  \
     "|t2rpntlvwz0rs\t%%tmm"#tdst", [%0+%1*1]}"	  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_2rpntlvwz0rst1_internal(tdst, base, stride) \
  __asm__ volatile					  \
  ("{t2rpntlvwz0rst1\t(%0,%1,1), %%tmm"#tdst		  \
     "|t2rpntlvwz0rst1\t%%tmm"#tdst", [%0+%1*1]}"	  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_2rpntlvwz1rs_internal(tdst, base, stride)	  \
  __asm__ volatile					  \
  ("{t2rpntlvwz1rs\t(%0,%1,1), %%tmm"#tdst		  \
     "|t2rpntlvwz1rs\t%%tmm"#tdst", [%0+%1*1]}"		  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_2rpntlvwz1rst1_internal(tdst, base, stride) \
  __asm__ volatile					  \
  ("{t2rpntlvwz1rst1\t(%0,%1,1), %%tmm"#tdst		  \
     "|t2rpntlvwz1rst1\t%%tmm"#tdst", [%0+%1*1]}"	  \
   :: "r" ((const void*) (base)), "r" ((long) (stride)))

#define _tile_2rpntlvwz0rs(tdst, base, stride)		  \
  _tile_2rpntlvwz0rs_internal(tdst, base, stride)

#define _tile_2rpntlvwz0rst1(tdst, base, stride)	  \
  _tile_2rpntlvwz0rst1_internal(tdst, base, stride)

#define _tile_2rpntlvwz1rs(tdst, base, stride)		  \
  _tile_2rpntlvwz1rs_internal(tdst, base, stride)

#define _tile_2rpntlvwz1rst1(tdst, base, stride)	  \
  _tile_2rpntlvwz1rst1_internal(tdst, base, stride)

#ifdef __DISABLE_AMX_MOVRS_TRANSPOSE__
#undef __DISABLE_AMX_MOVRS_TRANSPOSE__
#pragma GCC pop_options
#endif /* __DISABLE_AMX_MOVRS_TRANSPOSE__ */

#endif /* __x86_64__ */

#endif /* _AMX_MOVRSINTRIN_H_INCLUDED */
