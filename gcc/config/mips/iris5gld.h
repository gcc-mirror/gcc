/* Definitions of target machine for GNU compiler.  IRIX 5 with GNU ld.
   Copyright (C) 2004 Free Software Foundation, Inc.

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

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared: %{!non_shared: %{!call_shared: -call_shared}}}} \
%{rpath} -init __gcc_init -fini __gcc_fini \
-melf32bsmip"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%(irix_startfile_spec) irix-crti.o%s crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s irix-crtn.o%s %(irix_endfile_spec)"

/* The GNU linker supports one-only sections.  */
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

#define INIT_SECTION_ASM_OP "\t.section\t.init,0x1,0x6,4,4"
#define FINI_SECTION_ASM_OP "\t.section\t.fini,0x1,0x6,4,4"
