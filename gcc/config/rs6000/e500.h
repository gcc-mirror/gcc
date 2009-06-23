/* Enable E500 support.
   Copyright (C) 2003, 2004, 2006, 2007, 2008, 2009 Free Software
   Foundation, Inc.
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

#undef TARGET_SPE_ABI
#undef TARGET_SPE
#undef TARGET_E500
#undef TARGET_FPRS
#undef TARGET_E500_SINGLE
#undef TARGET_E500_DOUBLE
#undef CHECK_E500_OPTIONS

#define TARGET_SPE_ABI rs6000_spe_abi
#define TARGET_SPE rs6000_spe
#define TARGET_E500 (rs6000_cpu == PROCESSOR_PPC8540)
#define TARGET_FPRS (rs6000_float_gprs == 0)
#define TARGET_E500_SINGLE (TARGET_HARD_FLOAT && rs6000_float_gprs == 1)
#define TARGET_E500_DOUBLE (TARGET_HARD_FLOAT && rs6000_float_gprs == 2)
#define CHECK_E500_OPTIONS						\
  do {									\
    if (TARGET_E500 || TARGET_SPE || TARGET_SPE_ABI			\
	|| TARGET_E500_SINGLE || TARGET_E500_DOUBLE)			\
      {									\
	if (TARGET_ALTIVEC)						\
	  error ("AltiVec and E500 instructions cannot coexist");	\
	if (TARGET_VSX)							\
	  error ("VSX and E500 instructions cannot coexist");		\
	if (TARGET_64BIT)						\
	  error ("64-bit E500 not supported");				\
	if (TARGET_HARD_FLOAT && TARGET_FPRS)				\
	  error ("E500 and FPRs not supported");			\
      }									\
  } while (0)
