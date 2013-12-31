/* Definitions for option handling for Nios II.
   Copyright (C) 2013 Free Software Foundation, Inc.

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

#ifndef NIOS2_OPTS_H
#define NIOS2_OPTS_H

/* Enumeration of all FPU insn codes.  */
#define N2FPU_ALL_CODES							\
  N2FPU_CODE(fadds) N2FPU_CODE(fsubs) N2FPU_CODE(fmuls) N2FPU_CODE(fdivs) \
  N2FPU_CODE(fmins) N2FPU_CODE(fmaxs)					\
  N2FPU_CODE(fnegs) N2FPU_CODE(fabss) N2FPU_CODE(fsqrts)		\
  N2FPU_CODE(fsins) N2FPU_CODE(fcoss) N2FPU_CODE(ftans) N2FPU_CODE(fatans) \
  N2FPU_CODE(fexps) N2FPU_CODE(flogs)					\
  N2FPU_CODE(fcmpeqs) N2FPU_CODE(fcmpnes)				\
  N2FPU_CODE(fcmplts) N2FPU_CODE(fcmples)				\
  N2FPU_CODE(fcmpgts) N2FPU_CODE(fcmpges)				\
  									\
  N2FPU_CODE(faddd) N2FPU_CODE(fsubd) N2FPU_CODE(fmuld) N2FPU_CODE(fdivd) \
  N2FPU_CODE(fmind) N2FPU_CODE(fmaxd)					\
  N2FPU_CODE(fnegd) N2FPU_CODE(fabsd) N2FPU_CODE(fsqrtd)		\
  N2FPU_CODE(fsind) N2FPU_CODE(fcosd) N2FPU_CODE(ftand) N2FPU_CODE(fatand) \
  N2FPU_CODE(fexpd) N2FPU_CODE(flogd)					\
  N2FPU_CODE(fcmpeqd) N2FPU_CODE(fcmpned)				\
  N2FPU_CODE(fcmpltd) N2FPU_CODE(fcmpled)				\
  N2FPU_CODE(fcmpgtd) N2FPU_CODE(fcmpged)				\
  									\
  N2FPU_CODE(floatis) N2FPU_CODE(floatus)				\
  N2FPU_CODE(floatid) N2FPU_CODE(floatud)				\
  N2FPU_CODE(fixsi) N2FPU_CODE(fixsu)					\
  N2FPU_CODE(fixdi) N2FPU_CODE(fixdu)					\
  N2FPU_CODE(fextsd) N2FPU_CODE(ftruncds)				\
									\
  N2FPU_CODE(fwrx) N2FPU_CODE(fwry)					\
  N2FPU_CODE(frdxlo) N2FPU_CODE(frdxhi) N2FPU_CODE(frdy)

enum n2fpu_code {
#define N2FPU_CODE(name) n2fpu_ ## name,
  N2FPU_ALL_CODES
#undef N2FPU_CODE
  n2fpu_code_num
};

/* An enumeration to indicate the custom code status; if values within 0--255
   are registered to an FPU insn, or custom insn.  */
enum nios2_ccs_code
{
  CCS_UNUSED,
  CCS_FPU,
  CCS_BUILTIN_CALL
};

#endif

