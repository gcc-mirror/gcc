/* Definitions for PowerPC single-precision floating point unit
   such as Xilinx PowerPC 405/440 APU.

   Copyright (C) 2008-2016 Free Software Foundation, Inc.
   Contributed by Michael Eager (eager@eagercon.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* Undefine definitions from rs6000.h. */
#undef TARGET_SINGLE_FLOAT
#undef TARGET_DOUBLE_FLOAT
#undef TARGET_SINGLE_FPU
#undef TARGET_SIMPLE_FPU
#undef UNITS_PER_FP_WORD

/* FPU operations supported. 
   If TARGET_SINGLE_FPU set, processor supports single fp options. */
#define TARGET_SINGLE_FLOAT (rs6000_single_float)
#define TARGET_DOUBLE_FLOAT (rs6000_double_float)
#define TARGET_SINGLE_FPU   1
#define TARGET_SIMPLE_FPU   (rs6000_simple_fpu)

/* FP word width depends on single/double fp support. */
#define UNITS_PER_FP_WORD ((TARGET_SOFT_FLOAT || TARGET_DOUBLE_FLOAT) ? 8 : 4)

