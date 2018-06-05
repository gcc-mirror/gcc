/* Definitions for rtems targeting a Motorola m68k using elf.
   Copyright (C) 1999, 2000, 2002 National Research Council of Canada.
   Copyright (C) 2007-2018 Free Software Foundation, Inc.
   Contributed by Charles-Antoine Gauthier (charles.gauthier@nrc.ca).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */


/* Target OS builtins.  */
#undef TARGET_OS_CPP_BUILTINS	/* Defined in m68kemb.h.  */
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("mc68000");		\
	builtin_define ("__USE_INIT_FINI__");	\
	builtin_define ("__rtems__");		\
	builtin_assert ("system=rtems");	\
    }						\
  while (0)

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{!nostdlib: %{!qrtems: crt0.o%s} crti.o%s crtbegin.o%s}"
