/* Definitions of target machine for GNU compiler.
   Motorola m88100 running DG/UX.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)
   Currently maintained by (gcc@dg-rtp.dg.com)

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

/* You're not seeing double!  To transition to dwarf debugging, both are
   supported.  The option -msvr4 specifies elf.  With these combinations, 
   -g means dwarf.  */
/* DWARF_DEBUGGING_INFO defined in svr4.h.  */
#undef SDB_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE \
  (TARGET_SVR4 ? DWARF_DEBUG : SDB_DEBUG)

#ifndef NO_BUGS
#define AS_BUG_IMMEDIATE_LABEL
/* The DG/UX 4.30 assembler doesn't accept the symbol `fcr63'.  */
#define AS_BUG_FLDCR
#endif

/* TODO: convert includes to ${tm_file} list in config.gcc.  */
#include "m88k/m88k.h"

/* Augment TARGET_SWITCHES with the MXDB options.  */
#define MASK_STANDARD		0x40000000 /* Retain standard information */
#define MASK_NOLEGEND		0x20000000 /* Discard legend information */
#define MASK_EXTERNAL_LEGEND	0x10000000 /* Make external legends */

#define TARGET_STANDARD		  (target_flags & MASK_STANDARD)
#define TARGET_NOLEGEND		  (target_flags & MASK_NOLEGEND)
#define TARGET_EXTERNAL_LEGEND	  (target_flags & MASK_EXTERNAL_LEGEND)

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    { "standard",			 MASK_STANDARD }, \
    { "legend",				-MASK_NOLEGEND }, \
    { "no-legend",			 MASK_NOLEGEND }, \
    { "external-legend",		 MASK_EXTERNAL_LEGEND }, \
    /* the following is used only in the *_SPEC's */ \
    { "keep-coff",			 0 },

/* Default switches */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV	 | \
			 MASK_OCS_DEBUG_INFO	 | \
			 MASK_OCS_FRAME_POSITION | \
			 MASK_STANDARD		 | \
			 MASK_SVR4)
#undef	CPU_DEFAULT
#define CPU_DEFAULT MASK_88000

/* Macros to be automatically defined.  __svr4__ is our extension.
   __CLASSIFY_TYPE__ is used in the <varargs.h> and <stdarg.h> header
   files with DG/UX revision 5.40 and later.  This allows GNU CC to
   operate without installing the header files.  */

#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm88000 -Dm88k -Dunix -DDGUX -D__CLASSIFY_TYPE__=2\
   -D__svr4__ -Asystem=unix -Acpu=m88k -Amachine=m88k"

/* If -m88100 is in effect, add -Dm88100; similarly for -m88110.
   Here, the CPU_DEFAULT is assumed to be -m88000.  If not -ansi,
   -traditional, or restricting include files to one specific source
   target, specify full DG/UX features.  */
#undef	CPP_SPEC
#define	CPP_SPEC "%(cpp_cpu) %{msvr3:-D_M88KBCS_TARGET} %{!msvr3:-D_DGUX_TARGET}"

/* Assembler support (-V, silicon filter, legends for mxdb).  */
#undef	ASM_SPEC
#define ASM_SPEC "%{pipe:%{!.s: - }\
		   %{!msvr3:%{!m88110:-KV3 }%{m88110:-KV04.00 }}}\
		  %(asm_cpu)"

/* Override svr4.h.  */
#undef	ASM_FINAL_SPEC
#undef	STARTFILE_SPEC

/* Linker and library spec's.
   -msvr4 is the default if -msvr3 is not specified.
   -static, -shared, -symbolic, -h* and -z* access AT&T V.4 link options.
   -svr4 instructs gcc to place /usr/lib/values-X[cat].o on the link line.
   -msvr3 indicates linking done in a COFF environment and the link
   script is added to the link line.  In all environments, the first
   and last objects are crtbegin.o (or bcscrtbegin.o) and crtend.o.
   When the -G link option is used (-shared and -symbolic) a final
   link is not being done.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"
#undef	LIB_SPEC
#define LIB_SPEC "%{!msvr3:%{!shared:-lstaticdgc}} %{!shared:%{!symbolic:-lc}}"
#undef	LINK_SPEC
#define LINK_SPEC "%{z*} %{h*} %{v:-V} \
		   %{static:-dn -Bstatic} \
		   %{shared:-G -dy} \
		   %{symbolic:-Bsymbolic -G -dy} \
		   %{pg:-L/usr/lib/libp}%{p:-L/usr/lib/libp}"
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%(startfile_default)"


/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define EXTRA_SPECS                                     \
  { "cpp_cpu",          CPP_CPU_SPEC },                 \
  { "asm_cpu",          ASM_CPU_SPEC },                 \
  { "startfile_default", STARTFILE_DEFAULT_SPEC },  \
  { "startfile_crtbegin", STARTFILE_CRTBEGIN_SPEC }
   
/* Keep this left justified, no white space is allowed between
   the arguments to the -Wc option */
#define ASM_CPU_SPEC "\
		  %{v:-V}\
		  %{g:\
%{mno-legend:-Wc,off}\
%{!mno-legend:-Wc,-fix-bb,-s\"%i\"\
%{traditional:,-lc}\
%{!traditional:,-lansi-c}\
%{mstandard:,-keep-std}\
%{mexternal-legend:,-external}\
%{mocs-frame-position:,-ocs}}}"

#define CPP_CPU_SPEC "\
                  %{!m88000:%{!m88100:%{m88110:-D__m88110__}}} \
		  %{!m88000:%{!m88110:%{m88100:-D__m88100__}}} \
		  %{!ansi:%{!traditional:-D__OPEN_NAMESPACE__}}"

#define STARTFILE_DEFAULT_SPEC "\
                        %{!shared:%{!symbolic:%{pg:gcrt0.o%s} \
			 %{!pg:%{p:/lib/mcrt0.o}%{!p:/lib/crt0.o}} \
			  %(startfile_crtbegin) \
			 %{svr4:%{ansi:/lib/values-Xc.o} \
			  %{!ansi:%{traditional:/lib/values-Xt.o} \
			   %{!traditional:/usr/lib/values-Xa.o}}}}}"

#define STARTFILE_CRTBEGIN_SPEC "\
			 %{msvr3:m88kdgux.ld%s bcscrtbegin.o%s} \
			 %{!msvr3:crtbegin.o%s}"

#undef	GPLUSPLUS_INCLUDE_DIR
#define GPLUSPLUS_INCLUDE_DIR "/usr/opt/g++/lib/g++-include"

/* Fast DG/UX version of profiler that does not require lots of
   registers to be stored.  */
#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "gcc.mcount", 0)

/* Output the legend info for mxdb when debugging except if standard
   debugging information only is explicitly requested.  */
#undef  ASM_FIRST_LINE
#define ASM_FIRST_LINE(FILE)						\
  do {									\
    if (TARGET_SVR4)							\
      {									\
	if (TARGET_88110)						\
	  fprintf (FILE, "%s\"%s\"\n", VERSION_ASM_OP, "04.00");	\
	else								\
	  fprintf (FILE, "%s\"%s\"\n", VERSION_ASM_OP, "03.00");	\
      }									\
    if (write_symbols != NO_DEBUG && !TARGET_NOLEGEND)			\
      {									\
	fprintf (FILE, ";legend_info -fix-bb -h\"gcc-%s\" -s\"%s\"",	\
		 version_string, main_input_filename);			\
	fputs (flag_traditional ? " -lc" : " -lansi-c", FILE);		\
	if (TARGET_STANDARD)						\
	  fputs (" -keep-std", FILE);					\
	if (TARGET_EXTERNAL_LEGEND)					\
	  fputs (" -external", FILE);					\
	if (TARGET_OCS_FRAME_POSITION)					\
	  fputs (" -ocs", FILE);					\
	fputc ('\n', FILE);						\
      }									\
  } while (0)

/* Override svr4.h.  */
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE

/* Override svr4.h and m88k.h except when compiling crtstuff.c.  These must
   be constant strings when compiling crtstuff.c.  Otherwise, respect the
   -mversion-STRING option used.  */
#undef INIT_SECTION_PREAMBLE
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#if defined (CRT_BEGIN) || defined (CRT_END) || defined (L__main)
/* routines to invoke global constructors and destructors are always COFF 
   to enable linking mixed COFF and ELF objects */
#define FINI_SECTION_ASM_OP ("\tsection  .fini,\"x\"")
#ifndef BCS
#define INIT_SECTION_PREAMBLE asm ("\taddu\tr31,r31,0x20")
#endif
#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP ("\tsection\t .init,\"x\"")
#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP ("\tsection\t .ctors,\"d\"")
#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP ("\tsection\t .dtors,\"d\"")
#undef OBJECT_FORMAT_ELF
#else
#undef        INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP (TARGET_SVR4                      \
                           ? "\tsection\t .init,\"xa\""         \
                           : "\tsection\t .init,\"x\"")
#undef        CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP (TARGET_SVR4                     \
                            ? "\tsection\t .ctors,\"aw\""       \
                            : "\tsection\t .ctors,\"d\"")
#undef        DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP (TARGET_SVR4                     \
                            ? "\tsection\t .dtors,\"aw\""       \
                            : "\tsection\t .dtors,\"d\"")
#endif /* crtstuff.c */

/* The lists of global object constructors and global destructors are always
   placed in the .ctors/.dtors sections.  This requires the use of a link
   script if the COFF linker is used, but otherwise COFF and ELF objects
   can be intermixed.  A COFF object will pad the section to 16 bytes with
   zeros; and ELF object will not contain padding.  We deal with this by
   putting a -1 marker at the begin and end of the list and ignoring zero
   entries.  */

/* Mark the end of the .ctors/.dtors sections with a -1.  */

#define CTOR_LIST_BEGIN			\
asm (CTORS_SECTION_ASM_OP);		\
func_ptr __CTOR_LIST__[1] = { (func_ptr) (-1) }

#define CTOR_LIST_END			\
asm (CTORS_SECTION_ASM_OP);		\
func_ptr __CTOR_END__[1] = { (func_ptr) (-1) }

#define DTOR_LIST_BEGIN			\
asm (DTORS_SECTION_ASM_OP);		\
func_ptr __DTOR_LIST__[1] = { (func_ptr) (-1) }

#define DTOR_LIST_END			\
asm (DTORS_SECTION_ASM_OP);		\
func_ptr __DTOR_END__[1] = { (func_ptr) (-1) }

/* Walk the list ignoring NULL entries till we hit the terminating -1.  */
#define DO_GLOBAL_CTORS_BODY				\
  do {							\
    int i;						\
    for (i=1;(int)(__CTOR_LIST__[i]) != -1; i++)	\
      if (((int *)__CTOR_LIST__)[i] != 0)		\
	__CTOR_LIST__[i] ();				\
  } while (0)					

/* Walk the list looking for the terminating -1 that marks the end.
   Go backward and ignore any NULL entries.  */
#define DO_GLOBAL_DTORS_BODY				\
  do {							\
    int i;						\
    for (i=1;(int)(__DTOR_LIST__[i]) != -1; i++);	\
    for (i-=1;(int)(__DTOR_LIST__[i]) != -1; i--)	\
      if (((int *)__DTOR_LIST__)[i] != 0)		\
	__DTOR_LIST__[i] ();				\
  } while (0)					

/* The maximum alignment which the object file format can support.
   page alignment would seem to be enough */
#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 0x1000

/* Must use data section for relocatable constants when pic.  */
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX,ALIGN)      \
{                                               \
  if (flag_pic && symbolic_operand ((RTX), (MODE))) \
    data_section ();                            \
  else                                          \
    const_section ();                           \
}
