/* Common configuration file for NetBSD ELF targets.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Wasabi Systems, Inc.

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


/* This defines which switch letters take arguments.  On NetBSD, most
   of the normal cases (defined by gcc.c) apply, and we also have -h*
   and -z* options (for the linker) (coming from SVR4).  */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)			\
  (DEFAULT_SWITCH_TAKES_ARG (CHAR)		\
   || (CHAR) == 'h'				\
   || (CHAR) == 'z'				\
   || (CHAR) == 'R')


/* Provide a STARTFILE_SPEC appropriate for NetBSD ELF.  Here we
   provide support for the special GCC option -static.  On ELF
   targets, we also add the crtbegin.o file, which provides part
   of the support for getting C++ file-scope static objects
   constructed before entering "main".  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC		\
  "%{!shared:			\
     %{pg:gcrt0%O%s}		\
     %{!pg:			\
       %{p:gcrt0%O%s}		\
       %{!p:crt0%O%s}}}		\
   %{!shared:crtbegin%O%s} %{shared:crtbeginS%O%s}"


/* Provide an ENDFILE_SPEC appropriate for NetBSD ELF.  Here we
   add crtend.o, which provides part of the support for getting
   C++ file-scope static objects deconstructed after exiting "main".  */

#undef ENDFILE_SPEC
#define ENDFILE_SPEC		\
  "%{!shared:crtend%O%s} %{shared:crtendS%O%s}"


/* Provide a LINK_SPEC appropriate for NetBSD ELF.  Here we provide
   support for the special GCC options -assert, -R, -rpath, -shared,
   -nostdlib, -static, -rdynamic, and -dynamic-linker.  */

#undef LINK_SPEC
#define LINK_SPEC							\
  "%{assert*} %{R*} %{rpath*}						\
   %{shared:-shared}							\
   %{!shared:								\
     -dc -dp								\
     %{!nostdlib:							\
       %{!r*:								\
	 %{!e*:-e __start}}}						\
     %{!static:								\
       %{rdynamic:-export-dynamic}					\
       %{!dynamic-linker:-dynamic-linker /usr/libexec/ld.elf_so}}	\
     %{static:-static}}"
