/* Definitions of target machine for GNU compiler.  ARM on semi-hosted platform
   Copyright (C) 1994, 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (richard.earnshaw@armltd.co.uk)

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

#define STARTFILE_SPEC  "crt0.o%s"

#define LIB_SPEC "-lc"

#define SUBTARGET_CPP_SPEC "-D__semi__"

#define LINK_SPEC "%{mbig-endian:-EB} -X"

#ifndef TARGET_VERSION
#define TARGET_VERSION fputs (" (ARM/semi-hosted)", stderr);
#endif

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (ARM_FLAG_APCS_32 | ARM_FLAG_APCS_FRAME)
#endif

#ifndef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC
#endif

#ifndef ASM_SPEC
#define ASM_SPEC "\
%{mbig-endian:-EB} \
%{mcpu=*:-m%*} \
%{march=*:-m%*} \
%{mapcs-*:-mapcs-%*} \
%{mapcs-float:-mfloat} \
%{msoft-float:-mno-fpu} \
%{mthumb-interwork:-mthumb-interwork} \
" SUBTARGET_EXTRA_ASM_SPEC
#endif

#include "arm/aout.h"

#undef  CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC "-D__APCS_32__"
