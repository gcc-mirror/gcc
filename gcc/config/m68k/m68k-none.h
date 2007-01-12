/* Definitions of target machine for GNU compiler.  "naked" 68020.
   Copyright (C) 1994, 1996, 2003, 2006 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Pass flags to gas indicating which type of processor we have.  */

#undef ASM_SPEC
#define ASM_SPEC "\
%{m68851}%{mno-68851}%{m68881}%{mno-68881}%{msoft-float:-mno-68881} %{m68000}%{m68302}%{mc68000}%{m68010}%{m68020}%{mc68020}%{m68030}%{m68040}%{m68020-40:-mc68040} %{m68020-60:-mc68040} %{m68060}%{mcpu32}%{m68332}%{m5200}%{m5206e}%{m528x}%{m5307}%{m5407}%{mcfv4e} \
%{fPIC:--pcrel} %{fpic:--pcrel} %{msep-data:--pcrel} %{mid-shared-library:--pcrel} \
"

/* cc1/cc1plus always receives all the -m flags. If the specs strings above 
   are consistent with the flags in m68k.opt, there should be no need for
   any further cc1/cc1plus specs.  */

#undef CC1_SPEC
#define CC1_SPEC ""

#define CPP_SUBTARGET_SPEC ""
