/* Definitions of taret machine for GNU compiler.
   Matsushita AM33/2.0
   Copyright 2001, 2002 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

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
   
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#define TARGET_OS_CPP_BUILTINS() LINUX_TARGET_OS_CPP_BUILTINS()

#undef  CPP_SPEC
#define CPP_SPEC "%{mam33:-D__AM33__} %{!mam33:-D__AM33__=2 -D__AM33_2__} \
  %{posix:-D_POSIX_SOURCE} \
  %{pthread:-D_REENTRANT -D_PTHREADS}"

#undef  ASM_SPEC
#define ASM_SPEC "%{Wa,*:%*}"

#undef  LINK_SPEC
#define LINK_SPEC "%{mrelax:--relax} %{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
   %{static:-static}"

#undef  LIB_SPEC
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!static:-rpath-link %R/lib:%R/usr/lib} \
   %{!shared: %{pthread:-lpthread} \
     %{profile:-lc_p} %{!profile: -lc}}"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} \
		       %{!p:%{profile:gcrt1.o%s} \
			 %{!profile:crt1.o%s}}}} \
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#undef  TARGET_SWITCHES
#define TARGET_SWITCHES	\
  {{ "am33",		-0x4,	N_("Target the AM33 processor") }, \
   { "am33-2",		6,	N_("Target the AM33/2.0 processor") }, \
   { "relax",		0,	N_("Enable linker relaxations") }, \
   { "", TARGET_DEFAULT, NULL }}

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT 6

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (AM33/2.0 GNU/Linux)");

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

extern int mn10300_protect_label;

#undef  PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)		\
  do						\
    {						\
      mn10300_protect_label = 1;		\
      print_operand ((FILE), (X), (CODE));	\
      mn10300_protect_label = 0;		\
    }						\
  while (0)

#undef  PRINT_OPERAND_ADDRESS
#define PRINT_OPERAND_ADDRESS(FILE, X)		\
  do						\
    {						\
      mn10300_protect_label = 1;		\
      print_operand_address ((FILE), (X));	\
      mn10300_protect_label = 0;		\
    }						\
   while (0)

#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME)		\
  do						\
    {						\
      const char * real_name;			\
						\
      real_name = (*targetm.strip_name_encoding) (NAME);	\
      if (mn10300_protect_label)		\
        asm_fprintf (FILE, "+");		\
      asm_fprintf (FILE, "%U%s", real_name);	\
    }						\
  while (0)           

