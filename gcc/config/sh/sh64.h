/* Definitions of target machine for GNU compiler for SuperH SH 5.
   Copyright 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef TARGET_VERSION
#define TARGET_VERSION \
  fputs (" (SuperH SH)", stderr);

#undef ASM_SPEC
#define ASM_SPEC  "%(subtarget_asm_endian_spec) %{mrelax:-relax} \
%{m5-compact*:--isa=SHcompact} \
%{m5-32media*:--isa=SHmedia --abi=32} \
%{m5-64media*:--isa=SHmedia --abi=64} \
%{!m1:%{!m2:%{!m3*:%{!m4*:%{!m5*:--isa=SHmedia --abi=32}}}}} \
"

#undef LINK_DEFAULT_CPU_EMUL
#define LINK_DEFAULT_CPU_EMUL "32"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT  (SH5_BIT|SH4_BIT|SH_E_BIT|TARGET_ENDIAN_DEFAULT)

#undef SH_ELF_WCHAR_TYPE
#define SH_ELF_WCHAR_TYPE "int"
