/* Copyright (C) 2008-2016 Free Software Foundation, Inc.

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

#ifndef __iamcu__

#include <mmintrin.h>

#include <xmmintrin.h>

#include <emmintrin.h>

#include <pmmintrin.h>

#include <tmmintrin.h>

#include <ammintrin.h>

#include <smmintrin.h>

#include <wmmintrin.h>

/* For including AVX instructions */
#include <immintrin.h>

#include <mm3dnow.h>

#include <fma4intrin.h>

#include <xopintrin.h>

#include <lwpintrin.h>

#include <bmiintrin.h>

#include <bmi2intrin.h>

#include <tbmintrin.h>

#include <lzcntintrin.h>

#include <popcntintrin.h>

#include <rdseedintrin.h>

#include <prfchwintrin.h>

#include <fxsrintrin.h>

#include <xsaveintrin.h>

#include <xsaveoptintrin.h>

#endif /* __iamcu__ */

#include <adxintrin.h>

#ifndef __iamcu__

#include <clwbintrin.h>

#include <pcommitintrin.h>

#include <clflushoptintrin.h>

#include <xsavesintrin.h>

#include <xsavecintrin.h>

#include <mwaitxintrin.h>

#include <clzerointrin.h>

#include <pkuintrin.h>

#endif /* __iamcu__ */

#endif /* _X86INTRIN_H_INCLUDED */
