/* Definitions for Intel 386 running GNU/KFreeBSD systems with ELF format.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Bruno Haible.

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
#define TARGET_VERSION fprintf (stderr, " (i386 KFreeBSD/ELF)");

/* FIXME: Is a KFreeBSD-specific fallback mechanism necessary?  */
#undef MD_FALLBACK_FRAME_STATE_FOR

#undef LINK_SPEC
#define LINK_SPEC "-m elf_i386_fbsd %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
	%{static:-static}}}"
