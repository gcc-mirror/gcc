/* Definitions of target machine for GNU compiler.  ARM on semi-hosted platform
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
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

#define CPP_PREDEFINES \
    "-Darm -D__semi__ -Acpu(arm) -Amachine(arm)"

#define CPP_SPEC "%{m6:-D__arm6__} \
%{mcpu-*:-D__%*} \
%{mcpu=*:-D__%*} \
%{mapcs-32:-D__APCS_32__ -U__APCS_26__} \
%{mapcs-26:-D__APCS_26__ -U__APCS_32__} \
%{!mapcs-32: %{!mapcs-26:-D__APCS_32__}} \
%{msoft-float:-D__SOFTFP__} \
%{mhard-float:-U__SOFTFP__} \
%{!mhard-float: %{!msoft-float:-U__SOFTFP__}} \
%{mbig-endian:-D__ARMEB__ %{mwords-little-endian:-D__ARMWEL__}} \
%{mbe:-D__ARMEB__ %{mwords-little-endian:-D__ARMWEL__}} \
%{!mbe: %{!mbig-endian:-D__ARMEL__}} \
"

#define ASM_SPEC "%{mbig-endian:-EB}"

#define LINK_SPEC "%{mbig-endian:-EB} -X"

#define TARGET_VERSION fputs (" (ARM/semi-hosted)", stderr);

#define TARGET_DEFAULT ARM_FLAG_APCS_32

#ifndef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_ARM6
#endif

#include "arm/aout.h"
