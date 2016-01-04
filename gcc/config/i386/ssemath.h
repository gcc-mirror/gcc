/* Copyright (C) 2010-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef TARGET_FPMATH_DEFAULT
#define TARGET_FPMATH_DEFAULT (TARGET_SSE2 ? FPMATH_SSE : FPMATH_387)

#undef TARGET_FPMATH_DEFAULT_P
#define TARGET_FPMATH_DEFAULT_P(x) \
  (TARGET_SSE2_P(x) ? FPMATH_SSE : FPMATH_387)

#undef TARGET_SUBTARGET32_ISA_DEFAULT
#define TARGET_SUBTARGET32_ISA_DEFAULT \
   (OPTION_MASK_ISA_MMX | OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_SSE2)
