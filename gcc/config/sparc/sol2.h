/* Definitions of target machine for GNU compiler, for SPARC running Solaris 2
   Copyright 1992 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@ncd.com) and
   David V. Henkel-Wallace (gumby@cygnus.com).

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

/* Supposedly the same as vanilla sparc svr4, except for the stuff below: */
#include "sparcv4.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Dsun -Dsparc -Dunix -D__svr4__ -Asystem(unix) -Acpu(sparc) -Amachine(sparc)"

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
   %{fpic:-K PIC} %{fPIC:-K PIC}"

/* However it appears that Solaris 2.0 uses the same reg numbering as
   the old BSD-style system did. */

#undef DBX_REGISTER_NUMBER
/* Same as sparc.h */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* We use stabs-in-elf for debugging, because that is what the native
   toolchain uses.  */
#define DBX_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define DBX_BLOCKS_FUNCTION_RELATIVE 1

/* "gcc2_compiled." must be a .stabs, not an ordinary symbol, or GDB won't
   see it.  Furthermore, since GDB reads the input piecemeal, starting
   with each N_SO, it's a lot easier if the gcc2 flag symbol is *after*
   the N_SO rather than before it.  So we emit an N_OPT stab here.  */

#define ASM_IDENTIFY_GCC(FILE)	/* Do nothing */

#define ASM_IDENTIFY_GCC_AFTER_SOURCE(FILE)	\
 fputs ("\t.stabs\t\"gcc2_compiled.\", 0x3c, 0, 0, 0\n", FILE)

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#if 0 /* These seems unnecessary; the ones in sparcv4.h look right.  */
#undef TEXT_SECTION_ASM_OP
#undef DATA_SECTION_ASM_OP
#undef BSS_SECTION_ASM_OP
#undef CONST_SECTION_ASM_OP
#undef INIT_SECTION_ASM_OP

#define TEXT_SECTION_ASM_OP	"\t.section\t\".text\""
#define DATA_SECTION_ASM_OP	"\t.section\t\".data\""
#define BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

#define CONST_SECTION_ASM_OP	"\t.section\t\".rodata\""
#define INIT_SECTION_ASM_OP	"\t.section\t\".init\""
#endif

#define CTORS_SECTION_ASM_OP	"\t.section\t\".ctors\",#alloc,#execinstr\n"
#define DTORS_SECTION_ASM_OP	"\t.section\t\".dtors\",#alloc,#execinstr\n"

/* The native assembler can't compute differences between symbols in different
   sections when generating pic code, so we must put jump tables in the
   text section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Must use data section for relocatable constants when pic.  */
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX)		\
{						\
  if (flag_pic && symbolic_operand (RTX))	\
    data_section ();				\
  else						\
    const_section ();				\
}

/* The Solaris 2 assembler uses .skip, not .zero, so put this back. */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common"

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABELREF
#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.L%s%d", PREFIX, NUM)

/* in Solaris 2.0, linenos are relative to the current fn. */
#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,.LM%d-%s\n.LM%d:\n",	\
	     line, sym_lineno, 				\
	     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl)),\
	     sym_lineno);				\
    sym_lineno += 1; }

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */
#define	DBX_FUNCTION_FIRST


/* We don't use the standard svr4 STARTFILE_SPEC because it's wrong for us.
   We don't use the standard LIB_SPEC only because we don't yet support c++ */

/* If we cannot find the GNU *crt*.o files in the STANDARD_STARTFILE_PREFIX
   directory, our fallback strategy must be to look for these files instead
   in the Sun C 2.0 directory.  */

#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/opt/SUNWspro/SC2.0/"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{pg:crt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} \
			  %{pg:gmon.o%s} \
			  %{pg:crti.o%s}%{!pg:crti.o%s} \
			  %{ansi:/usr/ccs/lib/values-Xc.o%s} \
			  %{!ansi: \
			   %{traditional:/usr/ccs/lib/values-Xt.o%s} \
			   %{!traditional:/usr/ccs/lib/values-Xa.o%s}}}} \
			  crtbegin.o%s"

#undef	LIB_SPEC
#define LIB_SPEC \
  "%{!shared:%{!symbolic:-lc}} \
  crtend.o%s \
  %{!shared:%{!symbolic:%{pg:crtn.o%s}%{!pg:crtn.o%s}}}"

/* This should be the same as in svr4.h, except with -R added.  */
#undef LINK_SPEC
#define LINK_SPEC "%{h*} %{V} %{v:%{!V:-V}} \
		   %{b} %{Wl,*:%*} \
		   %{static:-dn -Bstatic} \
		   %{shared:-G -dy} \
		   %{symbolic:-Bsymbolic -G -dy} \
		   %{G:-G} \
		   %{YP,*} \
		   %{R*} \
		   %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
		    %{!p:-Y P,/usr/ccs/lib:/usr/lib}} \
		   %{Qy:} %{!Qn:-Qy}"

/* This defines which switch letters take arguments.
   It is as in svr4.h but with -R added.  */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
  (   (CHAR) == 'D' \
   || (CHAR) == 'U' \
   || (CHAR) == 'o' \
   || (CHAR) == 'e' \
   || (CHAR) == 'u' \
   || (CHAR) == 'I' \
   || (CHAR) == 'm' \
   || (CHAR) == 'L' \
   || (CHAR) == 'R' \
   || (CHAR) == 'A' \
   || (CHAR) == 'h' \
   || (CHAR) == 'z')
