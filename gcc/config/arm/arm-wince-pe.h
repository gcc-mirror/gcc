/* Definitions of target machine for GNU compiler,
   for ARM with PE obj format running under the WinCE operating system. 
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   
This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define ARM_WINCE			1

#include "pe.h"

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

#undef  TARGET_VERSION
#define TARGET_VERSION	fputs (" (ARM/WinCE/PE)", stderr);

/* The next three definitions are defined in pe.h,
   undefined in arm/arm-pe.h and then redefined back here!  */
#undef  LIB_SPEC
#define LIB_SPEC "-lcoredll -lcorelibc"

#define MATH_LIBRARY ""

#define LIBSTDCXX "-lc"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC ""
#define ENDFILE_SPEC ""

#undef  CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC	"-D__APCS_32__"

#undef  CC1_SPEC
#define CC1_SPEC  "%{!mapcs-32:%{!mapcs-26:-mapcs-32}}"

#undef  ASM_SPEC
#define ASM_SPEC "			\
%{mbig-endian:-EB}			\
%{mcpu=*:-m%*}				\
%{march=*:-m%*}				\
%{mapcs-*:-mapcs-%*}			\
%{mthumb-interwork:-mthumb-interwork}	\
%{!mapcs-32:%{!mapcs-26:-mapcs-32}}	\
"

/* WinCE headers require -DARM */
#undef  PE_SUBTARGET_CPP_SPEC
#define PE_SUBTARGET_CPP_SPEC "-D__pe__ -DARM -D__unaligned=__attribute__((aligned(1))) "

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned long"
