/* Definitions of target machine for GNU compiler for SuperH SH 5.
   Copyright 2000, 2001 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

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

#undef TARGET_VERSION
#define TARGET_VERSION \
  fputs (" (SuperH SH)", stderr);

#undef CPP_DEFAULT_CPU_SPEC
#define CPP_DEFAULT_CPU_SPEC "-D__SH5__=32 -D__SHMEDIA__"

#undef SUBTARGET_CPP_PTR_SPEC
#define SUBTARGET_CPP_PTR_SPEC "\
%{!m1:%{!m2:%{!m3:%{!m3e:%{!m4:%{!m4-single:%{!m4-single-only:%{!m4-nofpu:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int}}}}}}}} \
%{m1|m2|m3|m3e|m4|m4-single|m4-single-only|m4-nofpu:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
"

#undef ASM_SPEC
#define ASM_SPEC  "%{ml:-little} %{mrelax:-relax} \
%{m5-compact:--isa=SHcompact} %{m5-compact-nofpu:--isa=shcompact} \
%{m5-32media:--isa=SHmedia --abi=32} %{m5-32media-nofpu:--isa=SHmedia --abi=32} \
%{m5-64media:--isa=SHmedia --abi=64} %{m5-64media-nofpu:--isa=SHmedia --abi=64} \
%{!m1:%{!m2:%{!m3:%{!m3e:%{!m4:%{!m4-single:%{!m4-single-only:%{!m4-nofpu:%{!m5-64media:%{!m5-64media-nofpu:%{!m5-32media:%{!m5-32media-nofpu:%{!m5-compact:%{!m5-compact-nofpu:--isa=SHmedia --abi=32}}}}}}}}}}}}}} \
"

#undef LINK_SPEC
#define LINK_SPEC " \
%{m5-compact:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-compact-nofpu:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-32media:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-32media-nofpu:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-64media:%{!ml:-m shelf64} %{ml:-m shlelf64}} \
%{m5-64media-nofpu:%{!ml:-m shelf64} %{ml:-m shlelf64}} \
%{!m1:%{!m2:%{!m3:%{!m3e:%{!m4:%{!m4-single:%{!m4-single-only:%{!m4-nofpu:%{!m5-64media:%{!m5-64media-nofpu:%{!m5-32media:%{!m5-32media-nofpu:%{!m5-compact:%{!m5-compact-nofpu:%{!ml:-m shelf32} %{ml:-m shlelf32}}}}}}}}}}}}}}} \
%{mrelax:-relax}"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT  (SH5_BIT|SH4_BIT|SH3E_BIT)

/* These have been overridden in svr4.h, included in elf.h.  Restore
   them.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#undef MAX_WCHAR_TYPE_SIZE
