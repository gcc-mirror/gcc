/* Operating system specific defines to be used when targeting GCC for
   Windows NT 3.x.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (drupp@cs.washington.edu).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

/* The global __fltused is necessary to cause the printf/scanf routines
   for outputting/inputting floating point numbers to be loaded.  Since this
   is kind of hard to detect, we just do it all the time. */

#ifdef ASM_FILE_START
#undef ASM_FILE_START
#endif
#define ASM_FILE_START(FILE) \
  do {	fprintf (FILE, "\t.file\t");				\
	output_quoted_string (FILE, dump_base_name);		\
	fprintf (FILE, "\n");					\
        fprintf (FILE, ".global\t__fltused\n");			\
  } while (0)

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef LINK_SPEC
#define LINK_SPEC "-align:0x1000 -stack:1000000,1000000"

#undef CPP_SPEC
#define CPP_SPEC "-lang-c-c++-comments"

#undef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX ""

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX ""

#undef TOOLDIR_BASE_PREFIX
#define TOOLDIR_BASE_PREFIX ""

#ifdef STANDARD_INCLUDE_DIR
#undef STANDARD_INCLUDE_DIR
#endif
#define STANDARD_INCLUDE_DIR ""

#undef LOCAL_INCLUDE_DIR
#define LOCAL_INCLUDE_DIR ""

#undef INCLUDE_DEFAULTS
#define INCLUDE_DEFAULTS				\
  {							\
    { 0, 0, 0 }						\
  }

/* if the switch "-mwindows" is passed to ld, then specify to the Microsoft
   linker the proper switches and libraries to build a graphical program */

#define LIB_SPEC "%{mwindows:-subsystem:windows -entry:WinMainCRTStartup \
  USER32.LIB GDI32.LIB COMDLG32.LIB WINSPOOL.LIB} \
 %{!mwindows:-subsystem:console -entry:mainCRTStartup} \
 %{mcrtmt:OLDNAMES.LIB LIBCMT.LIB KERNEL32.LIB ADVAPI32.LIB} \
 %{!mcrtmt:OLDNAMES.LIB LIBC.LIB KERNEL32.LIB ADVAPI32.LIB} \
 %{g:-debugtype:coff -debug:full} \
 %{v}"

#define STDC_VALUE 0
