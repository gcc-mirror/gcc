/* Definitions of target machine for GNU compiler.
   Intel 386 (OSF/1 with ELF) version.
   Copyright (C) 1993 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config/i386/osfrose.h"

#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-DOSF -DOSF1 -Dunix -Di386 -Asystem(unix) -Acpu(i386) -Amachine(i386)"

#undef  CPP_SPEC
#define CPP_SPEC "\
%{!mrose: %{!mno-elf: -D__ELF__}} %{mrose: -D__ROSE__} %{mno-elf: -D__ROSE__} \
%{.S:	%{!ansi:%{!traditional:%{!traditional-cpp:%{!ftraditional: -traditional}}}}} \
%{.S:	-D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

/* Turn on -mpic-extern by default (change to later use -fpic.  */
#undef  CC1_SPEC
#define CC1_SPEC "\
%{!melf: %{!mrose: %{!mno-elf: -melf }}} \
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{pic-none:   -mno-half-pic} \
%{fpic:	      -mno-half-pic} \
%{fPIC:	      -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{pic-names*: -mhalf-pic} \
%{!pic-*: %{!fpic: %{!fPIC: -mhalf-pic}}}"

#undef	ASM_SPEC
#define ASM_SPEC       ""

#undef  LINK_SPEC
#define LINK_SPEC      "%{noshrlib: } %{glue: }"

#undef TARGET_VERSION_INTERNAL
#undef TARGET_VERSION

#undef	I386_VERSION
#define I386_VERSION " 80386, ELF objects"

#define TARGET_VERSION_INTERNAL(STREAM) fputs (I386_VERSION, STREAM)
#define TARGET_VERSION TARGET_VERSION_INTERNAL (stderr)

