/* Target definitions for GNU compiler for Intel 80x86 running DG/ux
   Copyright (C) 1993, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   Currently maintained by gcc@dg-rtp.dg.com.

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

/* for now, we are just like the sysv4 version with a
   few hacks
*/

#include "i386/sysv4.h"

#ifndef VERSION_INFO2
#define VERSION_INFO2   "$Revision: 1.8 $"
#endif

#ifndef VERSION_STRING
#define VERSION_STRING  version_string
#endif

/* Identify the compiler.  */
/* TARGET_VERSION used by toplev.c VERSION_STRING used by -midentify-revision */

#undef	TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (%s%s, %s)", \
				VERSION_INFO1, VERSION_INFO2, __DATE__)
#undef  VERSION_INFO1
#define VERSION_INFO1 "ix86 DG/ux, "

/* Augment TARGET_SWITCHES with the MXDB options.  */
#define MASK_STANDARD		0x40000000 /* Retain standard information */
#define MASK_NOLEGEND		0x20000000 /* Discard legend information */
#define MASK_EXTERNAL_LEGEND	0x10000000 /* Make external legends */
#define MASK_IDENTIFY_REVISION  0x08000000 /* Emit 'ident' to .s */
#define MASK_WARN_PASS_STRUCT   0x04000000 /* Warn when structures are passed */

#define TARGET_STANDARD		  (target_flags & MASK_STANDARD)
#define TARGET_NOLEGEND		  (target_flags & MASK_NOLEGEND)
#define TARGET_EXTERNAL_LEGEND	  (target_flags & MASK_EXTERNAL_LEGEND)
#define TARGET_IDENTIFY_REVISION  (target_flags & MASK_IDENTIFY_REVISION)
#define TARGET_WARN_PASS_STRUCT   (target_flags & MASK_WARN_PASS_STRUCT)

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    { "standard",			 MASK_STANDARD, "Retain standard MXDB information" },          \
    { "legend",				-MASK_NOLEGEND, "Retain legend information" },          \
    { "no-legend",			 MASK_NOLEGEND, "" },          \
    { "external-legend",		 MASK_EXTERNAL_LEGEND, "Generate external legend information" },   \
    { "identify-revision", 		 MASK_IDENTIFY_REVISION, "Emit identifying info in .s file" }, \
    { "warn-passed-structs", 		 MASK_WARN_PASS_STRUCT, "Warn when a function arg is a structure" },

#undef  DWARF_DEBUGGING_INFO
#define DWARF_DEBUGGING_INFO

/*
  allow -gstabs so that those who have gnu-as installed
  can debug c++ programs.
*/
#undef  DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG

/* Override svr[34].h.  */
#undef	ASM_FILE_START
#define ASM_FILE_START(FILE) \
  output_file_start (FILE, f_options, sizeof f_options / sizeof f_options[0], \
		     W_options, sizeof W_options / sizeof W_options[0])

/* ix86 abi specified type for wchar_t */

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD


/* Some machines may desire to change what optimizations are performed for
   various optimization levels.   This macro, if defined, is executed once
   just after the optimization level is determined and before the remainder
   of the command options have been parsed.  Values set in this macro are
   used as the default values for the other command line options.

   LEVEL is the optimization level specified; 2 if -O2 is specified,
   1 if -O is specified, and 0 if neither is specified.  */

/* This macro used to store 0 in flag_signed_bitfields.
   Not only is that misuse of this macro; the whole idea is wrong.

   The GNU C dialect makes bitfields signed by default,
   regardless of machine type.  Making any machine inconsistent in this
   regard is bad for portability.

   I chose to make bitfields signed by default because this is consistent
   with the way ordinary variables are handled: `int' equals `signed int'.
   If there is a good reason to prefer making bitfields unsigned by default,
   it cannot have anything to do with the choice of machine.
   If the reason is good enough, we should change the convention for all machines.

   -- rms, 20 July 1991.  */

/*
  this really should go into dgux-local.h 
*/

#undef	OPTIMIZATION_OPTIONS
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)		\
  do {							\
    extern int flag_signed_bitfields;			\
    flag_signed_bitfields = 0;				\
    optimization_options (LEVEL,SIZE);			\
  } while (0)


/* The normal location of the `ld' and `as' programs */

#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/bin/"

/* The normal location of the various *crt*.o files is the */

#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/lib/"

/* Macros to be automatically defined.  
   __CLASSIFY_TYPE__ is used in the <varargs.h> and <stdarg.h> header
   files with DG/UX revision 5.40 and later.  This allows GNU CC to
   operate without installing the header files.  */

#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ix86 -Dunix -DDGUX -D__CLASSIFY_TYPE__=2\
   -Asystem(unix) -Asystem(svr4)"

   /*
     If not -ansi, -traditional, or restricting include files to one
     specific source target, specify full DG/UX features.
   */
#undef	CPP_SPEC
#define	CPP_SPEC "%(cpp_cpu) %{!ansi:%{!traditional:-D__OPEN_NAMESPACE__}}"

/* Assembler support (legends for mxdb).  */
#undef	ASM_SPEC
#define ASM_SPEC "\
%{mno-legend:%{mstandard:-Wc,off}}\
%{g:%{!mno-legend:-Wc,-fix-bb,-s\"%i\"\
%{traditional:,-lc}%{!traditional:,-lansi-c}\
%{mstandard:,-keep-std}\
%{mexternal-legend:,-external}}}"

/* Override svr4.h.  */

/* hassey 3/12/94 keep svr4 ASM_FINAL_SPEC allows -pipe to work */

/* Linker and library spec's.
   -static, -shared, -symbolic, -h* and -z* access AT&T V.4 link options.
   -svr4 instructs gcc to place /usr/lib/values-X[cat].o on link the line.
   The absence of -msvr4 indicates linking done in a COFF environment and
   adds the link script to the link line.  In all environments, the first
   and last objects are crtbegin.o and crtend.o.
   When the -G link option is used (-shared and -symbolic) a final link is
   not being done.  */

#undef	LIB_SPEC
#define LIB_SPEC \
"%{!shared:%{!symbolic:-lc}}"

#undef	LINK_SPEC
#define LINK_SPEC "%{z*} %{h*} %{v:-V} \
		   %{static:-dn -Bstatic} \
		   %{shared:-G -dy} \
		   %{symbolic:-Bsymbolic -G -dy} \
		   %{pg:-L/usr/lib/libp}%{p:-L/usr/lib/libp}"

#ifdef CROSS_COMPILE

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{!symbolic:%{pg:gcrt1.o%s} 		\
			                      %{!pg:%{p:mcrt1.o%s} 	\
					      %{!p:crt1.o%s}}}} 	\
			 %{pg:gcrti.o%s}%{!pg:crti.o%s} 		\
			 crtbegin.o%s 					\
			 %{ansi:values-Xc.o%s} 				\
			 %{!ansi:%{traditional:values-Xt.o%s} 		\
			         %{!traditional:values-Xa.o%s}}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %{pg:gcrtn.o}%{!pg:crtn.o%s}"

#else

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{!symbolic:%{pg:gcrt1.o%s} 		\
			                      %{!pg:%{p:/lib/mcrt1.o%s}	\
					     %{!p:/lib/crt1.o%s}}}}    \
		       %{pg:gcrti.o%s}%{!pg:/lib/crti.o%s}	     \
			crtbegin.o%s 					\
			%{ansi:/lib/values-Xc.o%s} 			\
			%{!ansi:%{traditional:/lib/values-Xt.o%s} 	\
			        %{!traditional:/lib/values-Xa.o%s}}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %{pg:gcrtn.o}%{!pg:/lib/crtn.o}"

#endif /* CROSS_COMPILE */

/* The maximum alignment which the object file format can support.
   page alignment would seem to be enough */
#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 0x1000

/* Must use data section for relocatable constants when pic.  */
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX)            \
{                                               \
  if (flag_pic && symbolic_operand (RTX, VOIDmode)) \
    data_section ();                            \
  else                                          \
    const_section ();                           \
}

/* This supplements FUNCTION_ARG's definition in i386.h to check
   TARGET_WARN_PASS_STRUCT */

#undef  FUNCTION_ARG
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
((((MODE) == BLKmode && TARGET_WARN_PASS_STRUCT) ? \
    warning ("argument is a structure"),0 : 0), \
    (function_arg (&CUM, MODE, TYPE, NAMED)))

/* Add .align 1 to avoid .backalign bug in assembler */
#undef CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP    ".section\t.rodata\n\t.align 1"
