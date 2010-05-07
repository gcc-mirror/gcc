/* Target definitions for Darwin (Mac OS X) systems.
   Copyright (C) 2006, 2007, 2010 Free Software Foundation, Inc.
   Contributed by Apple Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Prefer DWARF2.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DARWIN_PREFER_DWARF

/* Since DWARF2 is default, conditions for running dsymutil are different.  */
#undef LINK_COMMAND_SPEC
#define LINK_COMMAND_SPEC "\
%{!fdump=*:%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %(linker) %l %X %{d} %{s} %{t} %{Z} \
    %{A} %{e*} %{m} %{r} %{x} \
    %{o*}%{!o:-o a.out} \
    %{!A:%{!nostdlib:%{!nostartfiles:%S}}} \
    %{L*} %(link_libgcc) %o %{fprofile-arcs|fprofile-generate*|coverage:-lgcov} \
    %{flto} %{fwhopr} \
    %{fopenmp|ftree-parallelize-loops=*: \
      %{static|static-libgcc|static-libstdc++|static-libgfortran: libgomp.a%s; : -lgomp } } \
    %{!nostdlib:%{!nodefaultlibs:  %(link_ssp) %G %L }} \
    %{!A:%{!nostdlib:%{!nostartfiles:%E}}} %{T*} %{F*} }}}}}}}\n\
%{!fdump=*:%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %{.c|.cc|.C|.cpp|.cp|.c++|.cxx|.CPP|.m|.mm: \
    %{g*:%{!gstabs*:%{!g0: dsymutil %{o*:%*}%{!o:a.out}}}}}}}}}}}}"

/* libSystem contains unwind information for signal frames.  */
#define DARWIN_LIBSYSTEM_HAS_UNWIND

/* The linker can generate branch islands.  */
#define DARWIN_LINKER_GENERATES_ISLANDS 1

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do {									\
    unsigned HOST_WIDE_INT _new_size = (SIZE);				\
    fprintf ((FILE), ".comm ");						\
    assemble_name ((FILE), (NAME));					\
    if (_new_size == 0) _new_size = 1;					\
    fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
	     _new_size, floor_log2 ((ALIGN) / BITS_PER_UNIT));		\
  } while (0)
