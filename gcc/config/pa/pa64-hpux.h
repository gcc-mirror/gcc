/* Definitions of target machine for GNU compiler, for HPs running
   HPUX using the 64bit runtime model.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES				\
  { "sio",	 MASK_SIO,				\
     N_("Generate cpp defines for server IO") },	\
  { "wsio",	-MASK_SIO,				\
     N_("Generate cpp defines for workstation IO") },	\
  {"gnu-ld",	 MASK_GNU_LD,				\
     N_("Assume code will be linked by GNU ld") },	\
  {"hp-ld",	-MASK_GNU_LD,				\
     N_("Assume code will be linked by HP ld") },

/* We can debug dynamically linked executables on hpux11; we also
   want dereferencing of a NULL pointer to cause a SEGV.  */
#undef LINK_SPEC
#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_GNU_LD)
#define LINK_SPEC \
  "-E %{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{shared:%{mhp-ld:-b}%{!mhp-ld:-shared}} %{mhp-ld:+Accept TypeMismatch}"
#else
#define LINK_SPEC \
  "-E %{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{shared:%{mgnu-ld:-shared}%{!mgnu-ld:-b}} %{!mgnu-ld:+Accept TypeMismatch}"
#endif

/* Like the default, except no -lg.  */
#undef LIB_SPEC
#define LIB_SPEC \
  "%{!shared:\
     %{!p:\
       %{!pg: %{!threads:-lc} %{threads:-lcma -lc_r}}\
       %{pg: -L/usr/lib/pa20_64/libp/ -lgprof -lc}}\
     %{p: -L/usr/lib/pa20_64/libp/ -lprof -lc}} /usr/lib/pa20_64/milli.a"

/* Under hpux11, the normal location of the `ld' and `as' programs is the
   /usr/ccs/bin directory.  */

#ifndef CROSS_COMPILE
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/ccs/bin"
#endif

/* Under hpux11 the normal location of the various pa20_64 *crt*.o files
   is the /usr/ccs/lib/pa20_64 directory.  Some files may also be in the
   /opt/langtools/lib/pa20_64 directory.  */

#ifndef CROSS_COMPILE
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/ccs/lib/pa20_64/"
#endif

#ifndef CROSS_COMPILE
#undef MD_STARTFILE_PREFIX_1
#define MD_STARTFILE_PREFIX_1 "/opt/langtools/lib/pa20_64/"
#endif

/* hpux11 has the new HP assembler.  It's still lousy, but it's a whole lot
   better than the assembler shipped with older versions of hpux.  */
#undef NEW_HP_ASSEMBLER
#define NEW_HP_ASSEMBLER 1

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
do {  \
     if (TARGET_64BIT) \
       fputs("\t.LEVEL 2.0w\n", FILE); \
     else if (TARGET_PA_20) \
       fputs("\t.LEVEL 2.0\n", FILE); \
     else if (TARGET_PA_11) \
       fputs("\t.LEVEL 1.1\n", FILE); \
     else \
       fputs("\t.LEVEL 1.0\n", FILE); \
     if (profile_flag)\
       fprintf (FILE, "\t.IMPORT _mcount, CODE\n");\
     if (write_symbols != NO_DEBUG) \
       output_file_directive ((FILE), main_input_filename); \
   } while (0)

/* It looks like DWARF2 will be the easiest debug format to handle on this
   platform.  */
#define OBJECT_FORMAT_ELF
#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
/* This isn't quite ready yet.  I'm seeing it mess up some line
   tables.  For example, we're getting lines starting/ending at
   impossible addresses.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 1


/* The rest of this file is copied from the generic svr4.h.  One day we
   would like to simply include svr4.h instead of copying all these
   definitions.  */

#define READONLY_DATA_SECTION_ASM_OP	"\t.section\t.rodata"

/* On svr4, we *do* have support for the .init and .fini sections, and we
   can put stuff in there to be executed before and after `main'.  We let
   crtstuff.c and other files know this by defining the following symbols.
   The definitions say how to change sections to the .init and .fini
   sections.  This is the same for all known svr4 assemblers.  */

/* For the time being, we aren't using init sections.  `P' relocations
   are currently used for function references.  However, P relocations are
   treated as data references and data references are bound by dld.sl
   immediately at program startup.  This causes an abort due to undefined
   weak symbols in crtbegin.o (e.g., __register_frame_info).  Possibly
   Q relocations might avoid this problem but the GNU assembler doesn't
   support them.  */
#if 0
#define INIT_SECTION_ASM_OP	"\t.section\t.init"
#define FINI_SECTION_ASM_OP	"\t.section\t.fini"
#else
#define EH_FRAME_IN_DATA_SECTION 1

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: %{!symbolic: crt0.o%s}}"
#endif

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

/* If using HP ld do not call pxdb.  Use size as a program that does nothing
   and returns 0.  /bin/true cannot be used because it is a script without
   an interpreter.  */
#define INIT_ENVIRONMENT "LD_PXDB=/usr/ccs/bin/size"
