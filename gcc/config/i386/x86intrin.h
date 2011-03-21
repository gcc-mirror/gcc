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

#ifndef _X86INTRIN_H_INCLUDED
#define _X86INTRIN_H_INCLUDED

#include <ia32intrin.h>

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

#ifdef __SSE4A__
#include <ammintrin.h>
#endif

#if defined (__SSE4_2__) || defined (__SSE4_1__)
#include <smmintrin.h>
#endif

#if defined (__AES__) || defined (__PCLMUL__)
#include <wmmintrin.h>
#endif

/* For including AVX instructions */
#include <immintrin.h>

#ifdef __3dNOW__
#include <mm3dnow.h>
#endif

#ifdef __FMA4__
#include <fma4intrin.h>
#endif

#ifdef __XOP__
#include <xopintrin.h>
#endif

#ifdef __LWP__
#include <lwpintrin.h>
#endif

#ifdef __ABM__
#include <abmintrin.h>
#endif

#ifdef __BMI__
#include <bmiintrin.h>
#endif

#ifdef __TBM__
#include <tbmintrin.h>
#endif

#ifdef __POPCNT__
#include <popcntintrin.h>
#endif

#endif /* _X86INTRIN_H_INCLUDED */
