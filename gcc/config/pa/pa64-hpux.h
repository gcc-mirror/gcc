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
   want dereferencing of a NULL pointer to cause a SEGV.  Do not move
   the "+Accept TypeMismatch" switch.  We check for it in collect2
   to determine which init/fini is needed.  */
#undef LINK_SPEC
#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_GNU_LD)
#define LINK_SPEC \
  "%{mhp-ld:+Accept TypeMismatch} -E %{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{shared:%{mhp-ld:-b}%{!mhp-ld:-shared}}"
#else
#define LINK_SPEC \
  "%{!mgnu-ld:+Accept TypeMismatch} -E %{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{shared:%{mgnu-ld:-shared}%{!mgnu-ld:-b}}"
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

/* Due to limitations in the target structure, it isn't currently possible
   to dynamically switch between the GNU and HP assemblers.  */
#undef TARGET_GAS

/* Configure selects the standard ELFOS defines for use with GAS.  */
#ifdef USING_ELFOS_H

/* We are using GAS.  */
#define TARGET_GAS 1

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
do {								\
  if (TARGET_64BIT)						\
    fputs("\t.LEVEL 2.0w\n", FILE);				\
  else if (TARGET_PA_20)					\
    fputs("\t.LEVEL 2.0\n", FILE);				\
  else if (TARGET_PA_11)					\
    fputs("\t.LEVEL 1.1\n", FILE);				\
  else								\
    fputs("\t.LEVEL 1.0\n", FILE);				\
  if (profile_flag)						\
    ASM_OUTPUT_TYPE_DIRECTIVE (FILE, "_mcount", "function");	\
  if (write_symbols != NO_DEBUG)				\
    {								\
      output_file_directive ((FILE), main_input_filename);	\
      fputs ("\t.version\t\"01.01\"\n", FILE);			\
    }								\
} while (0)

/* This is how we output a null terminated string.  */
#undef STRING_ASM_OP
#define STRING_ASM_OP	"\t.stringz\t"

#define TEXT_SECTION_ASM_OP	"\t.text"
#define DATA_SECTION_ASM_OP	"\t.data"
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  bss_section ();							\
  assemble_name ((FILE), (NAME));					\
  fputs ("\t.comm ", (FILE));						\
  fprintf ((FILE), "%d\n", MAX ((SIZE), ((ALIGN) / BITS_PER_UNIT)));	\
} while (0)

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  bss_section ();							\
  fprintf ((FILE), "\t.align %d\n", ((ALIGN) / BITS_PER_UNIT));		\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), "\n\t.block %d\n", (SIZE));				\
} while (0)

/* The define in pa.h doesn't work with the alias attribute.  The
   default is ok with the following define for GLOBAL_ASM_OP.  */
#undef TARGET_ASM_GLOBALIZE_LABEL

/* This is how we globalize a label.  */
#define GLOBAL_ASM_OP	"\t.globl\t"

/* Hacked version from elfos.h that doesn't output a label.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
do {								\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");		\
  ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));		\
} while (0)

/* The type of external references must be set correctly for the
   dynamic loader to work correctly.  This is equivalent to the
   HP assembler's .IMPORT directive but relates more directly to
   ELF object file types.  */
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
do {									\
  int save_referenced;							\
  save_referenced = TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL));\
  if (FUNCTION_NAME_P (NAME))						\
    ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");			\
  else									\
    ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
  TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL)) = save_referenced;\
} while (0)

/* We need set the type for external libcalls.  Also note that not all
   libcall names are passed to targetm.encode_section_info (e.g., __main).
   Thus, we also have to do the section encoding if it hasn't been done
   already.  */
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)			\
do {								\
  if (!FUNCTION_NAME_P (XSTR (FUN, 0)))				\
    hppa_encode_label (FUN);					\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, XSTR (FUN, 0), "function");	\
} while (0)

/* We need to use the HP style for internal labels.  */
#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)	\
  fprintf (FILE, "%c$%s%04d\n", (PREFIX)[0], (PREFIX) + 1, NUM)

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*%c$%s%04ld", (PREFIX)[0], (PREFIX) + 1, (long)(NUM))

#else /* USING_ELFOS_H */

/* We are not using GAS.  */
#define TARGET_GAS 0

/* HPUX 11 has the "new" HP assembler.  It's still lousy, but it's a whole
   lot better than the assembler shipped with older versions of hpux.
   However, it doesn't support weak symbols and is a bad fit with ELF.  */
#undef NEW_HP_ASSEMBLER
#define NEW_HP_ASSEMBLER 1

/* It looks like DWARF2 will be the easiest debug format to handle on this
   platform.  */
#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* This target uses the ELF object file format.  */
#define OBJECT_FORMAT_ELF

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
do {								\
  if (TARGET_64BIT)						\
    fputs("\t.LEVEL 2.0w\n", FILE);				\
  else if (TARGET_PA_20)					\
    fputs("\t.LEVEL 2.0\n", FILE);				\
  else if (TARGET_PA_11)					\
    fputs("\t.LEVEL 1.1\n", FILE);				\
  else								\
    fputs("\t.LEVEL 1.0\n", FILE);				\
  fputs("\t.SPACE $PRIVATE$,SORT=16\n\
\t.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31\n\
\t.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82\n\
\t.SPACE $TEXT$,SORT=8\n\
\t.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44\n\
\t.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n", FILE);	\
  if (profile_flag)						\
    fprintf (FILE, "\t.IMPORT _mcount, CODE\n");		\
  if (write_symbols != NO_DEBUG)				\
    output_file_directive ((FILE), main_input_filename);	\
} while (0)

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.SUBSPA $CODE$\n"
#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP "\t.SUBSPA $LIT$\n"
#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.SUBSPA $DATA$\n"
#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP "\t.SUBSPA $BSS$\n"

#endif /* USING_ELFOS_H */

/* For the time being, we aren't using init sections.  `P' relocations
   are currently used for function references.  However, P relocations are
   treated as data references and data references are bound by dld.sl
   immediately at program startup.  This causes an abort due to undefined
   weak symbols in crtbegin.o (e.g., __register_frame_info).  Possibly
   Q relocations might avoid this problem but the GNU assembler doesn't
   support them.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP

#define EH_FRAME_IN_DATA_SECTION 1

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: %{!symbolic: crt0.o%s}}"

/* Since we are not yet using .init and .fini sections, we need to
   explicitly arrange to run the global constructors and destructors.
   We could use ldd for this but it depends on LD_LIBRARY_PATH being
   correctly set.  So, we use the ld init and fini switches. However,
   we need to support different switches for the GNU and HP linkers.
   We can't check TARGET_GNU_LD in collect2, so we need a different
   test.  The +Accept switch is always the first switch when we are
   using the HP linker (see define for LINK_SPEC).  Checking for it
   is a somewhat fragile as it depends on internal details of the
   collect2 program but it is better than testing ld_file_name.

   FIXME: The GNU linker is broken.  The -init/-fini switches don't
   work and ldd can't determine the dynamic dependences of executables
   linked with GNU ld.  The init and fini routines are not executed
   although DT_INIT and DT_FINI appear ok.  As a result, defining
   LD_INIT_SWITCH and LD_FINI_SWITCH causes more harm than good when
   using GNU ld.  However, the definitions appear to work fine with
   the HP linker.  */
#if 0
#define LD_INIT_SWITCH (strcmp ("+Accept", ld2_argv[1]) ? "-init" : "+init")
#define LD_FINI_SWITCH (strcmp ("+Accept", ld2_argv[1]) ? "-fini" : "+fini")
#endif

/* If using HP ld do not call pxdb.  Use size as a program that does nothing
   and returns 0.  /bin/true cannot be used because it is a script without
   an interpreter.  */
#define INIT_ENVIRONMENT "LD_PXDB=/usr/ccs/bin/size"
