/* netware.h -- operating system specific defines to be used when 
   targeting GCC for some generic NetWare 4 system.
   Copyright (C) 1993, 1994, 2000, 2001, 2002 Free Software Foundation, Inc.

   Written by David V. Henkel-Wallace (gumby@cygnus.com)

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

/* We don't actually need any of these; the MD_ vars are ignored
   anyway for cross-compilers, and the other specs won't get picked up
   'coz the user is supposed to do ld -r (hmm, perhaps that should be
   the default).  In any case, setting them thus will catch some
   common user errors.  */

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef	LIB_SPEC
#define LIB_SPEC ""

/* Kinda useless, but what the hell */
#undef	LINK_SPEC
#define LINK_SPEC "%{h*} %{V} %{v:%{!V:-V}} \
		   %{b} %{Wl,*:%*} \
		   %{Qy:} %{!Qn:-Qy}"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC ""

#undef	RELATIVE_PREFIX_NOT_LINKDIR
#undef	LIBGCC_SPEC

/* set debugging info */
#define	DBX_DEBUGGING_INFO 1
#undef	SDB_DEBUGGING_INFO
#undef	DWARF_DEBUGGING_INFO
#undef	XCOFF_DEBUGGING_INFO
#undef	PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Support const sections and the ctors and dtors sections for g++.  */

#undef	HAS_INIT_SECTION
#undef	INIT_SECTION_ASM_OP

#undef	READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP    ".section\t.rodata"
#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"x\""
#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"x\""
