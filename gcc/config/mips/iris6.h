/* Definitions of target machine for GNU compiler.  Iris version 6.
   Copyright (C) 1994, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.

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

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ABICALLS|MASK_FLOAT64|MASK_64BIT)
#endif

#include "mips/iris5.h"
#include "mips/abi64.h"

/* Irix6 assembler does handle DWARF2 directives.  Override setting in
 irix5.h file.  */
#undef DWARF2_UNWIND_INFO

/* The Irix6 assembler will sometimes assign labels to the wrong
   section unless the labels are within .ent/.end blocks.  Therefore,
   we avoid creating such labels.  */
#define DWARF2_GENERATE_TEXT_SECTION_LABEL 0

/* For Irix 6, -mabi=64 implies TARGET_LONG64.  */
/* This is handled in override_options.  */

#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC "%{static: -mno-abicalls}"

/* We must pass -D_LONGLONG always, even when -ansi is used, because irix6
   system header files require it.  This is OK, because gcc never warns
   when long long is used in system header files.  Alternatively, we can
   add support for the SGI builtin type __long_long.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Dunix -Dmips -Dsgi -Dhost_mips -DMIPSEB -D_MIPSEB -DSYSTYPE_SVR4 \
  -D_LONGLONG -D_SVR4_SOURCE -D_MODERN_C -D__DSO__ \
  -Asystem(unix) -Asystem(svr4) -Acpu(mips) -Amachine(sgi)"

#undef SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mabi=32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=n32: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mabi=64: -D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
%{!mabi*: -D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}"

/* We must make -mips3 do what -mlong64 used to do.  */
/* ??? If no mipsX option given, but a mabi=X option is, then should set
   _MIPS_ISA based on the mabi=X option.  */
/* ??? If no mabi=X option give, but a mipsX option is, then should set
   _MIPS_SIM based on the mipsX option.  */
/* ??? Same for _MIPS_SZINT.  */
/* ??? Same for _MIPS_SZPTR.  */
/* ??? Same for __SIZE_TYPE and __PTRDIFF_TYPE.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
%{!ansi:-D__EXTENSIONS__ -D_SGI_SOURCE} \
%{mfp32: -D_MIPS_FPSET=16}%{!mfp32: -D_MIPS_FPSET=32} \
%{mips1: -D_MIPS_ISA=_MIPS_ISA_MIPS1} \
%{mips2: -D_MIPS_ISA=_MIPS_ISA_MIPS2} \
%{mips3: -D_MIPS_ISA=_MIPS_ISA_MIPS3} \
%{mips4: -D_MIPS_ISA=_MIPS_ISA_MIPS4} \
%{!mips*: -D_MIPS_ISA=_MIPS_ISA_MIPS3} \
%{mabi=32: -D_MIPS_SIM=_MIPS_SIM_ABI32}	\
%{mabi=n32: -D_ABIN32=2 -D_MIPS_SIM=_ABIN32} \
%{mabi=64: -D_ABI64=3 -D_MIPS_SIM=_ABI64} \
%{!mabi*: -D_ABIN32=2 -D_MIPS_SIM=_ABIN32} \
%{!mint64: -D_MIPS_SZINT=32}%{mint64: -D_MIPS_SZINT=64} \
%{mabi=32: -D_MIPS_SZLONG=32} \
%{mabi=n32: -D_MIPS_SZLONG=32} \
%{mabi=64: -D_MIPS_SZLONG=64} \
%{!mabi*: -D_MIPS_SZLONG=32} \
%{mabi=32: -D_MIPS_SZPTR=32} \
%{mabi=n32: -D_MIPS_SZPTR=32} \
%{mabi=64: -D_MIPS_SZPTR=64} \
%{!mabi*: -D_MIPS_SZPTR=32} \
%{!mips1:%{!mips2: -D_COMPILER_VERSION=601}}		\
%{!mips*: -U__mips -D__mips=3} \
%{mabi=32: -U__mips64} \
%{mabi=n32: -D__mips64} \
%{mabi=64: -D__mips64} \
%{!mabi*: -D__mips64}"

/* Irix 6 uses DWARF-2.  */
#define DWARF2_DEBUGGING_INFO
#define MIPS_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Force the generation of dwarf .debug_frame sections even if not
   compiling -g.  This guarantees that we can unwind the stack. */
#define DWARF2_FRAME_INFO 1
/* The size in bytes of a DWARF field indicating an offset or length
   relative to a debug info section, specified to be 4 bytes in the DWARF-2
   specification.  The SGI/MIPS ABI defines it to be the same as PTR_SIZE.  */
#define DWARF_OFFSET_SIZE PTR_SIZE

/* There is no GNU as port for Irix6 yet, so we set MD_EXEC_PREFIX so that
   gcc will automatically find SGI as instead of searching the user's path.
   The latter can fail when building a cross compiler if the user has . in
   the path before /usr/bin, since then gcc will find and try to use the link
   to the cross assembler which can't possibly work.  */

#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/bin/"

/* We have no need for MD_STARTFILE_PREFIX.  */
#undef MD_STARTFILE_PREFIX

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 6.x"

/* The Irix 6.0.1 assembler doesn't like labels in the text section, so
   just avoid emitting them.  */
#define ASM_IDENTIFY_GCC(x) ((void)0)
#define ASM_IDENTIFY_LANGUAGE(x) ((void)0)

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
   pseudo-ops.  */
#define FUNCTION_NAME_ALREADY_DECLARED

#undef SET_ASM_OP	/* Has no equivalent.  See ASM_OUTPUT_DEF below.  */

#if 0
/* This is *NOT* how to equate one symbol to another symbol.  The assembler
   '=' syntax just equates a name to a constant expression.
   See ASM_OUTPUT_WEAK_ALIAS.  */

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif

/* Define the strings used for the special svr4 .type and .size directives.  */

#define TYPE_ASM_OP	".type"
#define SIZE_ASM_OP	".size"

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_OUTPUT_WEAK_ALIAS(FILE,NAME,VALUE)	\
 do {						\
  ASM_GLOBALIZE_LABEL (FILE, NAME);		\
  fputs ("\t.weakext\t", FILE);			\
  assemble_name (FILE, NAME);			\
  if (VALUE)					\
    {						\
      fputc (' ', FILE);			\
      assemble_name (FILE, VALUE);		\
    }						\
  fputc ('\n', FILE);				\
 } while (0)

#define ASM_WEAKEN_LABEL(FILE,NAME) ASM_OUTPUT_WEAK_ALIAS(FILE,NAME,0)

#define POPSECTION_ASM_OP	".popsection"

#define DEBUG_INFO_SECTION	".debug_info,0x7000001e,0,0,1"
#define DEBUG_LINE_SECTION	".debug_line,0x7000001e,0,0,1"
#define SFNAMES_SECTION		".debug_sfnames,0x7000001e,0,0,1"
#define SRCINFO_SECTION		".debug_srcinfo,0x7000001e,0,0,1"
#define MACINFO_SECTION		".debug_macinfo,0x7000001e,0,0,1"
#define PUBNAMES_SECTION	".debug_pubnames,0x7000001e,0,0,1"
#define ARANGES_SECTION		".debug_aranges,0x7000001e,0,0,1"
#define FRAME_SECTION		".debug_frame,0x7000001e,0x08000000,0,1"
#define ABBREV_SECTION		".debug_abbrev,0x7000001e,0,0,1"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
/* If no mips[3,4] option given, give the appropriate default for mabi=X */
#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "%{!mabi*:-n32} %{!mips*: %{!mabi*:-mips3} %{mabi=n32:-mips3} %{mabi=64:-mips4}}"

/* Must pass -g0 to the assembler, otherwise it may overwrite our
   debug info with its own debug info. */
/* Must pass -show instead of -v.  */
/* Must pass -G 0 to the assembler, otherwise we may get warnings about
   GOT overflow.  */
/* ??? We pass -w to disable all assembler warnings.  The `label should be
   inside .ent/.end block' warning that we get for DWARF II debug info labels
   is particularly annoying.  */
#undef SUBTARGET_MIPS_AS_ASM_SPEC
#define SUBTARGET_MIPS_AS_ASM_SPEC "%{v:-show} -G 0 -w"

#undef SUBTARGET_ASM_DEBUGGING_SPEC
#define SUBTARGET_ASM_DEBUGGING_SPEC "-g0"

/* The MIPS assembler occasionally misoptimizes.  Since GCC should be
   doing scheduling anyhow, just turn off optimization in the assembler.  */
#undef SUBTARGET_ASM_OPTIMIZING_SPEC
#define SUBTARGET_ASM_OPTIMIZING_SPEC "-O0"

/* Stuff for constructors.  Start here.  */

/* The assembler now accepts .section pseudo-ops, but it does not allow
   one to change the section in the middle of a function, so we can't use
   the INIT_SECTION_ASM_OP code in crtstuff.  But we can build up the ctor
   and dtor lists this way, so we use -init and -fini to invoke the
   do_global_* functions instead of running collect2.  */

#define BSS_SECTION_ASM_OP	".section\t.bss"
#define CONST_SECTION_ASM_OP_32	"\t.rdata"
#define CONST_SECTION_ASM_OP_64	".section\t.rodata"

/* The IRIX 6 assembler .section directive takes four additional args:
   section type, flags, entry size, and alignment.  The alignment of the
   .ctors and .dtors sections needs to be the same as the size of a pointer
   so that the linker doesn't add padding between elements.  */
#if defined (CRT_BEGIN) || defined (CRT_END)

/* If we are included from crtstuff.c, these need to be plain strings.
   _MIPS_SZPTR is defined in SUBTARGET_CPP_SPEC above.  */
#if _MIPS_SZPTR == 64
#define CTORS_SECTION_ASM_OP ".section\t.ctors,1,2,0,8"
#define DTORS_SECTION_ASM_OP ".section\t.dtors,1,2,0,8"
#else /* _MIPS_SZPTR != 64 */
#define CTORS_SECTION_ASM_OP ".section\t.ctors,1,2,0,4"
#define DTORS_SECTION_ASM_OP ".section\t.dtors,1,2,0,4"
#endif /* _MIPS_SZPTR == 64 */

#else /* ! (defined (CRT_BEGIN) || defined (CRT_END)) */

/* If we are included from varasm.c, these need to depend on -mabi.  */
#define CTORS_SECTION_ASM_OP \
  (Pmode == DImode ? ".section\t.ctors,1,2,0,8" : ".section\t.ctors,1,2,0,4")
#define DTORS_SECTION_ASM_OP \
  (Pmode == DImode ? ".section\t.dtors,1,2,0,8" : ".section\t.dtors,1,2,0,4")
#endif /* defined (CRT_BEGIN) || defined (CRT_END) */

/* dwarf2out will handle padding this data properly.  We definitely don't
   want it 8-byte aligned on n32.  */
#define EH_FRAME_SECTION_ASM_OP ".section\t.eh_frame,1,2,0,1"

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
      if (mips_abi != ABI_32 && mips_abi != ABI_O64)			\
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

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ",						\
	     (Pmode == DImode) ? ".dword" : ".word");			\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ",						\
	     (Pmode == DImode) ? ".dword" : ".word");			\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  */

#define ASM_OUTPUT_SECTION_NAME(F, DECL, NAME, RELOC)			\
do {									\
  extern FILE *asm_out_text_file;					\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)			\
    fprintf (asm_out_text_file, "\t.section %s,1,6,4,4\n", (NAME));	\
  else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))		\
    fprintf (F, "\t.section %s,1,2,0,8\n", (NAME));			\
  else									\
    fprintf (F, "\t.section %s,1,3,0,8\n", (NAME));			\
} while (0)

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
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)		   \
do									   \
  {									   \
    if (mips_abi != ABI_32 && mips_abi != ABI_O64)			   \
      {									   \
	fprintf (STREAM, "%s\n", BSS_SECTION_ASM_OP);			   \
	mips_declare_object (STREAM, NAME, "", ":\n", 0);		   \
	ASM_OUTPUT_ALIGN (STREAM, floor_log2 (ALIGN / BITS_PER_UNIT));	   \
	ASM_OUTPUT_SKIP (STREAM, SIZE);					   \
	fprintf (STREAM, "\t%s\n", POPSECTION_ASM_OP);			   \
      }									   \
    else								   \
      mips_declare_object (STREAM, NAME, "\n\t.lcomm\t", ",%u\n", (SIZE)); \
  }									   \
while (0)

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* Write the extra assembler code needed to declare an object properly.  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)			\
do									\
 {									\
   size_directive_output = 0;						\
   if (!flag_inhibit_size_directive && DECL_SIZE (DECL))	\
     {									\
       size_directive_output = 1;					\
       fprintf (STREAM, "\t%s\t ", SIZE_ASM_OP);			\
       assemble_name (STREAM, NAME);					\
       fprintf (STREAM, ",%d\n", int_size_in_bytes (TREE_TYPE (DECL)));	\
     }									\
   mips_declare_object (STREAM, NAME, "", ":\n", 0);			\
 }									\
while (0)

/* Define the `__builtin_va_list' type for the ABI.  On Irix6, this
   type is `char *'.  */
#undef BUILD_VA_LIST_TYPE
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = build_pointer_type (char_type_node)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);			 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);			 \
	 assemble_name (FILE, name);					 \
	 fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL))); \
       }								 \
   } while (0)

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX ((mips_abi == ABI_32 || mips_abi == ABI_O64) \
			    ? "$" : ".")

/* Profiling is supported via libprof1.a not -lc_p as in Irix 3.  */
/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!static:%{!shared: \
     %{mabi=32:%{pg:gcrt1.o%s} \
       %{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}} \
     %{mabi=n32: \
       %{mips4:%{pg:/usr/lib32/mips4/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/mcrt1.o%s /usr/lib32/mips4/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/mcrt1.o%s /usr/lib32/mips3/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/crt1.o%s}}}} \
     %{mabi=64: \
       %{mips4:%{pg:/usr/lib64/mips4/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips4/mcrt1.o /usr/lib64/mips4/libprof1.a} \
           %{!p:/usr/lib64/mips4/crt1.o}}} \
       %{!mips4:%{pg:/usr/lib64/mips3/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips3/mcrt1.o /usr/lib64/mips3/libprof1.a} \
           %{!p:/usr/lib64/mips3/crt1.o}}}} \
     %{!mabi*: \
       %{mips4:%{pg:/usr/lib32/mips4/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/mcrt1.o%s /usr/lib32/mips4/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/mcrt1.o%s /usr/lib32/mips3/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/crt1.o%s}}}}}} \
   %{static: \
     %{mabi=32:%{pg:/usr/lib/nonshared/gcrt1.o%s} \
       %{!pg:%{p:/usr/lib/nonshared/mcrt1.o%s /usr/lib/nonshared/libprof1.a%s} \
         %{!p:/usr/lib/nonshared/crt1.o%s}}} \
     %{mabi=n32: \
       %{mips4:%{pg:/usr/lib32/mips4/nonshared/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/nonshared/mcrt1.o%s \
             /usr/lib32/mips4/nonshared/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/nonshared/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/nonshared/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/nonshared/mcrt1.o%s \
             /usr/lib32/mips3/nonshared/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/nonshared/crt1.o%s}}}} \
     %{mabi=64: \
       %{mips4:%{pg:/usr/lib64/mips4/nonshared/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips4/nonshared/mcrt1.o \
             /usr/lib64/mips4/nonshared/libprof1.a} \
           %{!p:/usr/lib64/mips4/nonshared/crt1.o}}} \
       %{!mips4:%{pg:/usr/lib64/mips3/nonshared/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips3/nonshared/mcrt1.o \
             /usr/lib64/mips3/nonshared/libprof1.a} \
           %{!p:/usr/lib64/mips3/nonshared/crt1.o}}}} \
     %{!mabi*: \
       %{mips4:%{pg:/usr/lib32/mips4/nonshared/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/nonshared/mcrt1.o%s \
             /usr/lib32/mips4/nonshared/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/nonshared/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/nonshared/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/nonshared/mcrt1.o%s \
           /usr/lib32/mips3/nonshared/libprof1.a%s} \
         %{!pg:%{p:/usr/lib32/mips3/nonshared/mcrt1.o%s \
             /usr/lib32/mips3/nonshared/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/nonshared/crt1.o%s}}}}}} \
   crtbegin.o%s"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{mabi=n32: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{mabi=64: %{mips4:-L/usr/lib64/mips4} %{!mips4:-L/usr/lib64/mips3} \
     -L/usr/lib64} \
   %{!mabi*: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{!shared: \
     -dont_warn_unused %{p:libprof1.a%s}%{pg:libprof1.a%s} -lc -warn_unused}"

/* Avoid getting two warnings for libgcc.a everytime we link.  */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC "-dont_warn_unused -lgcc -warn_unused"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend.o%s \
   %{!shared: \
     %{mabi=32:crtn.o%s}\
     %{mabi=n32:%{mips4:/usr/lib32/mips4/crtn.o%s}\
       %{!mips4:/usr/lib32/mips3/crtn.o%s}}\
     %{mabi=64:%{mips4:/usr/lib64/mips4/crtn.o%s}\
       %{!mips4:/usr/lib64/mips3/crtn.o%s}}\
     %{!mabi*:%{mips4:/usr/lib32/mips4/crtn.o%s}\
       %{!mips4:/usr/lib32/mips3/crtn.o%s}}}"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} %{w} \
%{static: -non_shared} \
%{!static: \
  %{!shared: %{!non_shared: %{!call_shared: -call_shared -no_unresolved}}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_dtors,__EH_FRAME_BEGIN__,__frame_dummy} \
-_SYSTYPE_SVR4 -woff 131 \
%{mabi=32: -32}%{mabi=n32: -n32}%{mabi=64: -64}%{!mabi*: -n32}"
