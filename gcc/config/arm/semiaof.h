/* Definitions of target machine for GNU compiler.  ARM on semi-hosted platform
   AOF Syntax assembler.
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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

#define CPP_PREDEFINES \
    "-Darm -Dsemi -Acpu(arm) -Amachine(arm)"

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

#define ASM_SPEC "%{g -g} -arch 4 \
-apcs 3%{mapcs-32:/32bit}%{mapcs-26:/26bit}%{!mapcs-26:%{!macps-32:/32bit}}"

#define LIB_SPEC "%{Eb: armlib_h.32b%s}%{!Eb: armlib_h.32l%s}"

#define TARGET_VERSION fputs (" (ARM/semi-hosted)", stderr);

#define TARGET_DEFAULT ARM_FLAG_APCS_32

/* The Norcroft C library defines size_t as "unsigned int" */
#define SIZE_TYPE "unsigned int"

#include "arm/aof.h"
