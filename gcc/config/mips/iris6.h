/* Definitions of target machine for GNU compiler.  Iris version 6.
   Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

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

/* Default to -mabi=n32 and -mips3.  */
#define MIPS_ISA_DEFAULT 3
#define MIPS_ABI_DEFAULT ABI_N32
#define MULTILIB_DEFAULTS { "mabi=n32" }

#include "mips/iris5.h"
#include "mips/abi64.h"

/* Irix 6 uses DWARF.  */
#define DWARF_DEBUGGING_INFO
#define DWARF_VERSION 2
#define MIPS_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 6.x"

/* The Irix 6.0.1 assembler doesn't like labels in the text section, so
   just avoid emitting them.  */
#define ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_LANGUAGE

/* Irix 5 stuff that we don't need for Irix 6.  */
/* ??? We do need this for the -mabi=32 switch though.  */
#undef ASM_OUTPUT_UNDEF_FUNCTION
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#undef ASM_DECLARE_FUNCTION_SIZE

/* Stuff we need for Irix 6 that isn't in Irix 5.  */

/* The SGI assembler doesn't like labels before the .ent, so we must output
   the .ent and function name here, which is the normal place for it.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)			\
  do {									\
    fputs ("\t.ent\t", STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs (":\n", STREAM);						\
  } while (0)

/* Likewise, the SGI assembler doesn't like labels after the .end, so we
   must output the .end here.  */
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL)			\
  do {									\
    fputs ("\t.end\t", STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
  } while (0)

/* Tell function_prologue in mips.c that we have already output the .ent/.end
   psuedo-ops.  */
#define FUNCTION_NAME_ALREADY_DECLARED

#undef SET_ASM_OP	/* Has no equivalent.  See ASM_OUTPUT_DEF below.  */

/* This is how to equate one symbol to another symbol.  The syntax used is
   `SYM1=SYM2'.  Note that this is different from the way equates are done
   with most svr4 assemblers, where the syntax is `.set SYM1,SYM2'.  */

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)

#define POPSECTION_ASM_OP	".popsection"

#define DEBUG_SECTION		".debug_info,0x7000001e,0,0,1"
#define LINE_SECTION		".debug_line,0x7000001e,0,0,1"
#define SFNAMES_SECTION		".debug_sfnames,0x7000001e,0,0,1"
#define SRCINFO_SECTION		".debug_srcinfo,0x7000001e,0,0,1"
#define MACINFO_SECTION		".debug_macinfo,0x7000001e,0,0,1"
#define PUBNAMES_SECTION	".debug_pubnames,0x7000001e,0,0,1"
#define ARANGES_SECTION		".debug_aranges,0x7000001e,0,0,1"
#define FRAME_SECTION		".debug_frame,0x7000001e,0x08000000,0,1"
#define ABBREV_SECTION		".debug_abbrev,0x7000001e,0,0,1"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef ASM_SPEC
#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GAS) != 0
/* GAS */
#define ASM_SPEC "\
%{mmips-as: \
	%{!.s:-nocpp} %{.s: %{cpp} %{nocpp}} \
	%{pipe: %e-pipe is not supported.} \
	%{K}} \
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} %{v} \
%{noasmopt:-O0} \
%{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}} \
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3} \
%{membedded-pic} \
%{mabi=32:-32}%{mabi=o32:-32}%{mabi=n32:-n32}%{mabi=64:-64}%{mabi=n64:-64} \
%{!mabi*:-n32}"

#else
/* not GAS */
/* Must pass -g0 to the assembler, otherwise it may overwrite our
   debug info with its own debug info. */
/* Must pass -show instead of -v.  */
/* Must pass -G 0 to the assembler, otherwise we may get warnings about
   GOT overflow.  */
#define ASM_SPEC "\
%{!mgas: \
	%{!.s:-nocpp} %{.s: %{cpp} %{nocpp}} \
	%{pipe: %e-pipe is not supported.} \
	%{K}} \
%{G*} %{EB} %{EL} %{v:-show} \
%{mips1} %{mips2} %{mips3} %{mips4} \
%{noasmopt:-O0} %{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}} \
-g0 -G 0 %{membedded-pic} \
%{mabi=32:-32}%{mabi=o32:-32}%{mabi=n32:-n32}%{mabi=64:-64}%{mabi=n64:-64} \
%{!mabi*:-n32}"

#endif

/* Stuff for constructors.  Start here.  */

/* The assembler now accepts .section pseudo-ops, but it does not allow
   one to change the section in the middle of a function.  crtstuff relies
   on this hack, and thus crtstuff won't work here.  So, we do init and
   fini sections exactly the same way as they are done for Irix 5, and
   we ifdef out the ASM_OUTPUT_{CON,DE}STRUCTOR macros below.  */

#define CONST_SECTION_ASM_OP_32	"\t.rdata"
#define CONST_SECTION_ASM_OP_64	".section\t.rodata"
#define CTORS_SECTION_ASM_OP	".section\t.ctors,1,2,0,4"
#define DTORS_SECTION_ASM_OP	".section\t.dtors,1,2,0,4"

/* This is the pseudo-op used to generate a 32-bit word of data with a
   specific value in some section.  This is the same for all known svr4
   assemblers.  */

#define INT_ASM_OP		".word"

/* A default list of other sections which we might be "in" at any given
   time.  For targets that use additional sections (e.g. .tdesc) you
   should override this definition in the target-specific file which
   includes this file.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_rdata, in_const, in_ctors, in_dtors

/* A default list of extra section function definitions.  For targets
   that use additional sections (e.g. .tdesc) you should override this
   definition in the target-specific file which includes this file.  */

/* ??? rdata_section is now same as svr4 const_section.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
void									\
rdata_section ()							\
{									\
  if (in_section != in_rdata)						\
    {									\
      if (mips_abi != ABI_32)						\
	fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP_64);	\
      else								\
	fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP_32);	\
      in_section = in_rdata;						\
    }									\
}									\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#if 0

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

#endif

/* Stuff for constructors.  End here.  */

/* ??? Perhaps just include svr4.h in this file?  */

/* ??? SGI assembler may core dump when compiling with -g.
   Sometimes as succeeds, but then we get a linker error. (cmds.c in 072.sc)
   Getting rid of .file solves both problems.  */
#undef ASM_OUTPUT_FILENAME
#define ASM_OUTPUT_FILENAME(STREAM, NUM_SOURCE_FILENAMES, NAME) \
do								\
  {								\
    fprintf (STREAM, "\t#.file\t%d ", NUM_SOURCE_FILENAMES);	\
    output_quoted_string (STREAM, NAME);			\
    fputs ("\n", STREAM);					\
  }								\
while (0)

/* ??? SGI assembler gives warning whenever .lcomm is used.  */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)	\
do								\
  {								\
    if (mips_abi != ABI_32)					\
      {								\
	fputs ("\t.section\t.bss\n", STREAM);			\
	ASM_DECLARE_OBJECT_NAME (STREAM, NAME, 0);		\
	ASM_OUTPUT_ALIGN (STREAM, floor_log2 (ALIGN / BITS_PER_UNIT));	\
	ASM_OUTPUT_SKIP (STREAM, SIZE);				\
	fprintf (STREAM, "\t%s\n", POPSECTION_ASM_OP);		\
      }								\
    else							\
      mips_declare_object (STREAM, NAME, "\n\t.lcomm\t", ",%u\n", (SIZE)); \
  }								\
while (0)

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX (mips_abi == ABI_32 ? "$" : ".")

/* Profiling is supported via libprof1.a not -lc_p as in Irix 3.  */
/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef STARTFILE_SPEC
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{mabi=32:%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}} \
   %{mabi=n32:%{pg:/usr/lib32/gcrt1.o%s}%{!pg:%{p:/usr/lib32/mcrt1.o%s /usr/lib32/libprof1.a%s}%{!p:/usr/lib32/crt1.o%s}}} \
   %{mabi=64:%{pg:/usr/lib64/gcrt1.o}%{!pg:%{p:/usr/lib64/mcrt1.o /usr/lib64/libprof1.a}%{!p:/usr/lib64/crt1.o}}} \
   %{!mabi=32:%{!mabi=n32:%{!mabi=64:%{pg:/usr/lib32/gcrt1.o%s}%{!pg:%{p:/usr/lib32/mcrt1.o%s /usr/lib32/libprof1.a%s}%{!p:/usr/lib32/crt1.o%s}}}}}"

#undef LIB_SPEC
#define LIB_SPEC "%{p:libprof1.a%s}%{pg:libprof1.a%s} -lc"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{mabi=32:crtn.o%s}%{mabi=n32:/usr/lib32/crtn.o%s}\
   %{mabi=64:/usr/lib64/crtn.o}\
   %{!mabi=32:%{!mabi=n32:%{!mabi=64:/usr/lib32/crtn.o%s}}}"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
/* ??? We use the -woff 84 option to disable the warning about linking
   with libraries that are unnecessary.  This message is currently more of
   a hassle than a benefit, because we get two warnings for libgcc.a everytime
   we link.  If we added the proper -dont_warn_unused/-warn_unused options
   around libgcc.a, then we can take out the -woff 84 option.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -call_shared -no_unresolved}}} \
%{rpath} \
-_SYSTYPE_SVR4 -woff 84 \
%{mabi=32: -32}%{mabi=n32: -n32}%{mabi=64: -64} \
%{!mabi=32:%{!mabi=n32:%{!mabi=64: -n32}}}"
