/* Definitions of target machine for GNU compiler.  Iris version 6 with
   GNU ld.
   Copyright (C) 1999, 2000, 2002, 2004 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>.

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

#undef LIB_SPEC
#define LIB_SPEC \
  "%{mabi=n32: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{mabi=64: %{mips4:-L/usr/lib64/mips4} %{!mips4:-L/usr/lib64/mips3} \
     -L/usr/lib64} \
   %{!mabi*: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{!shared: \
     %{p:libprof1.a%s}%{pg:libprof1.a%s} -lc}"

/* Use the default libgcc spec.  */
#undef LIBGCC_SPEC

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared: %{!non_shared: %{!call_shared: -call_shared}}}} \
%{rpath} -init __gcc_init -fini __gcc_fini \
%{mabi=32: -melf32bsmip}%{mabi=n32: -melf32bmipn32}%{mabi=64: -melf64bmip}%{!mabi*: -melf32bmipn32}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%(irix_startfile_spec) irix-crti.o%s crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s irix-crtn.o%s %(irix_endfile_spec)"

/* The GNU linker supports one-only sections.  */
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

#define INIT_SECTION_ASM_OP "\t.section\t.init,0x1,0x6,4,4"
#define FINI_SECTION_ASM_OP "\t.section\t.fini,0x1,0x6,4,4"
