/* Definitions of target machine for GNU compiler.
   Motorola m88100 running DG/UX.
   Copyright (C) 1988, 1989, 1990, 1991 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)
   Enhanced by Michael Meissner (meissner@osf.org)
   Currently supported by Tom Wood (wood@dg-rtp.dg.com)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* You're not seeing double!  To transition to dwarf debugging, both are
   supported.  The option -msvr4 and -mversion-03.00 specify (for DG/UX)
   `real' elf.  With these combinations, -g means dwarf.  */
/* DWARF_DEBUGGING_INFO defined in svr4.h.  */
#define SDB_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE \
  (GET_VERSION_0300_SYNTAX ? DWARF_DEBUG : SDB_DEBUG)

#ifndef NO_BUGS
#define AS_BUG_IMMEDIATE_LABEL
/* The DG/UX 4.30 assembler doesn't accept the symbol `fcr63'.  */
#define AS_BUG_FLDCR
#endif

#include "svr4.h"
#include "m88k.h"

/* Augment TARGET_SWITCHES with the MXDB options.  */
#define MASK_STANDARD		0x40000000 /* Retain standard information */
#define MASK_LEGEND		0x20000000 /* Retain legend information */
#define MASK_EXTERNAL_LEGEND	0x10000000 /* Make external legends */

#define TARGET_STANDARD		  (target_flags & MASK_STANDARD)
#define TARGET_LEGEND		  (target_flags & MASK_LEGEND)
#define TARGET_EXTERNAL_LEGEND	  (target_flags & MASK_EXTERNAL_LEGEND)

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    { "standard",			 MASK_STANDARD }, \
    { "legend",				 MASK_LEGEND }, \
    { "external-legend",		 MASK_EXTERNAL_LEGEND }, \
    /* the following is used only in the *_SPEC's */ \
    { "keep-coff",			 0 },

/* Default switches */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV	 | \
			 MASK_OCS_DEBUG_INFO	 | \
			 MASK_OCS_FRAME_POSITION)
#undef	CPU_DEFAULT
#define CPU_DEFAULT MASK_88000

/* Macros to be automatically defined.  __svr4__ is our extension.
   __CLASSIFY_TYPE__ is used in the <varargs.h> and <stdarg.h> header
   files with DG/UX revision 5.40 and later.  This allows GNU CC to
   operate without installing the header files.  */

#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-Dm88000 -Dm88k -Dunix -DDGUX -D__CLASSIFY_TYPE__=2\
   -D__svr4__ -Asystem(unix) -Acpu(m88k) -Amachine(m88k)"

/* If -m88100 is in effect, add -Dm88100; similarly for -m88110.
   Here, the CPU_DEFAULT is assumed to be -m88000.  If not -ansi,
   -traditional, or restricting include files to one specific source
   target, specify full DG/UX features.  */
#undef	CPP_SPEC
#define	CPP_SPEC "%{!m88000:%{!m88100:%{m88110:-D__m88110__}}} \
		  %{!m88000:%{!m88110:%{m88100:-D__m88100__}}} \
		  %{!ansi:%{!traditional:-D__OPEN_NAMESPACE__}}"

/* Assembler support (-V, silicon filter, legends for mxdb).  */
#undef	ASM_SPEC
#define ASM_SPEC "\
%{V} %{v:%{!V:-V}} %{pipe: - %{msvr4:%{mversion-03.00:-KV3}}}\
%{!mlegend:%{mstandard:-Wc,off}}\
%{mlegend:-Wc,-fix-bb,-h\"gcc-2.2.14\",-s\"%i\"\
%{traditional:,-lc}%{!traditional:,-lansi-c}\
%{mstandard:,-keep-std}\
%{mkeep-coff:,-keep-coff}\
%{mexternal-legend:,-external}\
%{mocs-frame-position:,-ocs}}"

/* Override svr4.h.  */
#undef	ASM_FINAL_SPEC
#undef	STARTFILE_SPEC

/* Linker and library spec's.
   -static, -shared, -symbolic, -h* and -z* access AT&T V.4 link options.
   -svr4 instructs gcc to place /usr/lib/values-X[cat].o on link the line.
   When the -G link option is used (-shared and -symbolic) a final link is
   not being done.  */
#undef	LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"
#undef	LINK_SPEC
#define LINK_SPEC "%{z*} %{h*} %{V} %{v:%{!V:-V}} \
		   %{static:-dn -Bstatic} \
		   %{shared:-G -dy} \
		   %{symbolic:-Bsymbolic -G -dy} \
		   %{pg:-L/usr/lib/libp}%{p:-L/usr/lib/libp}"
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{!symbolic:%{pg:gcrt0.o%s} \
			 %{!pg:%{p:/lib/mcrt0.o}%{!p:/lib/crt0.o}} \
			 %{svr4:%{ansi:/lib/values-Xc.o} \
			  %{!ansi:%{traditional:/lib/values-Xt.o} \
			   %{!traditional:/usr/lib/values-Xa.o}}}}}"

#undef	GPLUSPLUS_INCLUDE_DIR
#define GPLUSPLUS_INCLUDE_DIR "/usr/opt/g++/lib/g++-include"

/* Fast DG/UX version of profiler that does not require lots of
   registers to be stored.  */
#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "gcc.mcount", 0)

/* DGUX V.4 isn't quite ELF--yet.  */
#undef  VERSION_0300_SYNTAX
#define VERSION_0300_SYNTAX (TARGET_SVR4 && m88k_version_0300)

/* Same, but used before OVERRIDE_OPTIONS has been processed.  */
#define GET_VERSION_0300_SYNTAX \
  (TARGET_SVR4 && m88k_version != 0 && strcmp (m88k_version, "03.00") >= 0)

/* Output the legend info for mxdb when debugging except if standard
   debugging information only is explicitly requested.  */
#undef  ASM_FIRST_LINE
#define ASM_FIRST_LINE(FILE)						\
  do {									\
    if (VERSION_0300_SYNTAX)						\
      fprintf (FILE, "\t%s\t \"03.00\"\n", VERSION_ASM_OP);		\
    if (write_symbols != NO_DEBUG					\
	&& ! (TARGET_STANDARD && ! TARGET_LEGEND))			\
      {									\
	fprintf (FILE, ";legend_info -fix-bb -h\"gcc-%s\" -s\"%s\"",	\
		 VERSION_STRING, main_input_filename);			\
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

/* Override svr4.h and m88k.h except when compiling crtstuff.c.  */
#if !defined (CRT_BEGIN) && !defined (CRT_END)
#if 0 /* The SVR4 init method doesn't yet work.  */
#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP (VERSION_0300_SYNTAX		\
			     ? "section\t .init,\"xa\""	\
			     : "section\t .init,\"x\"")
#endif
#undef	CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP (VERSION_0300_SYNTAX		\
			      ? "section\t .ctors,\"aw\""	\
			      : "section\t .ctors,\"d\"")
#undef	DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP (VERSION_0300_SYNTAX		\
			      ? "section\t .dtors,\"aw\""	\
			      : "section\t .dtors,\"d\"")
#endif /* crtstuff.c */
