/* Definitions of target machine for GNU compiler.  IRIX version 5 with gas.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

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

/* Reenable debugging.  */
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* GNU as does handle DWARF2 directives.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

/* Override iris5.h version to invoke [cd]tors and register eh frame
   information.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared:%{!non_shared:%{!call_shared: -call_shared -no_unresolved}}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_ctors_1,__do_global_dtors} \
-_SYSTYPE_SVR4"

/* Override iris5.h versions to include crtbegin.o and crtend.o.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%(irix_startfile_spec) crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %(irix_endfile_spec)"

/* Irix 5 does not have some strange restrictions that Irix 3 had.  */
#undef SET_FILE_NUMBER
#define SET_FILE_NUMBER() ++num_source_filenames
#undef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)

/* We need to use .esize and .etype instead of .size and .type to
   avoid conflicting with ELF directives.  These are only recognized
   by gas, anyhow, not the native assembler.  */
#undef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a)					\
do {							\
  fprintf (asm_out_file, "\t.esize\t" HOST_WIDE_INT_PRINT_DEC ";", \
 	   (HOST_WIDE_INT) (a));			\
} while (0)

#undef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a)					\
do {							\
  fprintf (asm_out_file, "\t.etype\t0x%x;", (a));	\
} while (0)

/* Add -g to mips.h default to avoid confusing gas with local symbols
   generated from stabs info.  */
#undef NM_FLAGS
#define NM_FLAGS "-Bng"

/* Disable SHF_MERGE support.  Even if gas supports it, the IRIX ld does not
   without a special elspec(5) file.

   FIXME: Only do this if not using GNU ld.  */
#undef HAVE_GAS_SHF_MERGE
#define HAVE_GAS_SHF_MERGE 0
