/* Definitions of target machine for GNU compiler.  Iris version 6 with
   GNU ld.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>.

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
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{mabi=32: -melf32bsmip}%{mabi=n32: -melf32bmipn32}%{mabi=64: -melf64bmip}%{!mabi*: -melf32bmipn32}"

/* The GNU linker supports one-only sections.  */
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
#undef UNIQUE_SECTION_P
#define UNIQUE_SECTION_P(DECL) (DECL_ONE_ONLY (DECL))
#define UNIQUE_SECTION(DECL,RELOC)				\
do {								\
  int len;							\
  char *name, *string, *prefix;					\
								\
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
								\
  if (! DECL_ONE_ONLY (DECL))					\
    {								\
      prefix = ".";                                             \
      if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	prefix = ".text.";					\
      else if (DECL_READONLY_SECTION (DECL, RELOC))		\
	prefix = ".rodata.";					\
      else							\
	prefix = ".data.";					\
    }								\
  else if (TREE_CODE (DECL) == FUNCTION_DECL)			\
    prefix = ".gnu.linkonce.t.";				\
  else if (DECL_READONLY_SECTION (DECL, RELOC))			\
    prefix = ".gnu.linkonce.r.";				\
  else								\
    prefix = ".gnu.linkonce.d.";				\
								\
  len = strlen (name) + strlen (prefix);			\
  string = alloca (len + 1);					\
  sprintf (string, "%s%s", prefix, name);			\
								\
  DECL_SECTION_NAME (DECL) = build_string (len, string);	\
} while (0)
