/* Definitions of target machine for GNU compiler.
   Intel 386 (OSF/1 with OSF/rose) version.
   Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

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

#include "halfpic.h"
#include "i386/gstabs.h"

/* Get perform_* macros to build libgcc.a.  */
#include "i386/perform.h"

#define OSF_OS

#undef  WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR)					\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR) || !strcmp (STR, "pic-names"))

/* This defines which switch letters take arguments.  On svr4, most of
   the normal cases (defined in gcc.c) apply, and we also have -h* and
   -z* options (for the linker).  */

#define SWITCH_TAKES_ARG(CHAR) \
  (   (CHAR) == 'D' \
   || (CHAR) == 'U' \
   || (CHAR) == 'o' \
   || (CHAR) == 'e' \
   || (CHAR) == 'T' \
   || (CHAR) == 'u' \
   || (CHAR) == 'I' \
   || (CHAR) == 'm' \
   || (CHAR) == 'L' \
   || (CHAR) == 'A' \
   || (CHAR) == 'h' \
   || (CHAR) == 'z')

#define MASK_HALF_PIC     	010000000000	/* Mask for half-pic code */
#define MASK_HALF_PIC_DEBUG     004000000000	/* Debug flag */
#define MASK_ELF		002000000000	/* ELF not rose */
#define MASK_NO_IDENT		001000000000	/* suppress .ident */
#define MASK_NO_UNDERSCORES	000400000000	/* suppress leading _ */
#define MASK_LARGE_ALIGN	000200000000	/* align to >word boundaries */
#define MASK_NO_MCOUNT		000100000000	/* profiling uses mcount_ptr */

#define TARGET_HALF_PIC		(target_flags & MASK_HALF_PIC)
#define TARGET_DEBUG		(target_flags & MASK_HALF_PIC_DEBUG)
#define HALF_PIC_DEBUG		TARGET_DEBUG
#define TARGET_ELF		(target_flags & MASK_ELF)
#define TARGET_ROSE		((target_flags & MASK_ELF) == 0)
#define TARGET_IDENT		((target_flags & MASK_NO_IDENT) == 0)
#define TARGET_UNDERSCORES	((target_flags & MASK_NO_UNDERSCORES) == 0)
#define TARGET_LARGE_ALIGN	(target_flags & MASK_LARGE_ALIGN)
#define TARGET_MCOUNT		((target_flags & MASK_NO_MCOUNT) == 0)

#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
     { "half-pic",		 MASK_HALF_PIC},			\
     { "no-half-pic",		-MASK_HALF_PIC},			\
     { "debug-half-pic",	 MASK_HALF_PIC_DEBUG},			\
     { "debugb",		 MASK_HALF_PIC_DEBUG},			\
     { "elf",			 MASK_ELF},				\
     { "rose",			-MASK_ELF},				\
     { "ident",			-MASK_NO_IDENT},			\
     { "no-ident",		 MASK_NO_IDENT},			\
     { "underscores",		-MASK_NO_UNDERSCORES},			\
     { "no-underscores",	 MASK_NO_UNDERSCORES},			\
     { "large-align",		 MASK_LARGE_ALIGN},			\
     { "no-large-align",	-MASK_LARGE_ALIGN},			\
     { "mcount",		-MASK_NO_MCOUNT},			\
     { "mcount-ptr",		 MASK_NO_MCOUNT},			\
     { "no-mcount",		 MASK_NO_MCOUNT},

/* OSF/rose uses stabs, not dwarf.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#ifndef DWARF_DEBUGGING_INFO
#define DWARF_DEBUGGING_INFO	/* enable dwarf debugging for testing */
#endif

/* Handle #pragma weak and #pragma pack.  */

#define HANDLE_SYSV_PRAGMA
#define SUPPORTS_WEAK	TARGET_ELF

/* Change default predefines.  */
#undef	CPP_PREDEFINES
#define CPP_PREDEFINES "-DOSF -DOSF1 -Dunix -Di386 -Asystem(unix) -Asystem(xpg4) -Acpu(i386) -Amachine(i386)"

#undef  CPP_SPEC
#define CPP_SPEC "\
%{!melf: -D__ROSE__ %{!pic-none: -D__SHARED__}} \
%{melf: -D__ELF__ %{fpic: -D__SHARED__}} \
%{mno-underscores: -D__NO_UNDERSCORES__} \
%{melf: %{!munderscores: -D__NO_UNDERSCORES__}} \
%{.S:	%{!ansi:%{!traditional:%{!traditional-cpp:%{!ftraditional: -traditional}}}}} \
%{.S:	-D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

/* Turn on -pic-extern by default for OSF/rose, -fpic for ELF.  */
#undef  CC1_SPEC
#define CC1_SPEC "\
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{!melf: %{!mrose: -mrose }} \
%{melf: %{!munderscores: %{!mno-underscores: -mno-underscores }} \
	%{!mmcount: %{!mno-mcount: %{!mmcount-ptr: -mmcount-ptr }}}} \
%{!melf: %{!munderscores: %{!mno-underscores: -munderscores }} \
	 %{!mmcount: %{!mno-mcount: %{!mmcount-ptr: -mmcount }}} \
	 %{pic-extern: -mhalf-pic } %{pic-lib: -mhalf-pic } \
	 %{!pic-extern: %{!pic-lib: %{pic-none: -mno-half-pic} %{!pic-none: -mhalf-pic}}} \
	 %{pic-calls: } %{pic-names*: }}"

#undef	ASM_SPEC
#define ASM_SPEC       "%{v*: -v}"

#undef  LINK_SPEC
#define LINK_SPEC      "%{v*: -v} \
%{!melf:	%{!noshrlib: %{pic-none: -noshrlib} %{!pic-none: -warn_nopic}} \
		%{nostdlib} %{noshrlib} %{glue}} \
%{melf:		%{dy} %{dn} %{glue: } \
		%{h*} %{z*} \
		%{static:-dn -Bstatic} \
		%{shared:-G -dy} \
		%{symbolic:-Bsymbolic -G -dy} \
		%{G:-G} \
		%{!dy: %{!dn: %{!static: %{!shared: %{!symbolic: \
			%{noshrlib: -dn } %{pic-none: -dn } \
			%{!noshrlib: %{!pic-none: -dy}}}}}}}}"

#undef  LIB_SPEC
#define LIB_SPEC "-lc"

#undef  LIBG_SPEC
#define LIBG_SPEC ""

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#undef TARGET_VERSION_INTERNAL
#undef TARGET_VERSION

#define I386_VERSION " 80386, OSF/rose objects"

#define TARGET_VERSION_INTERNAL(STREAM) fputs (I386_VERSION, STREAM)
#define TARGET_VERSION TARGET_VERSION_INTERNAL (stderr)

#undef  MD_EXEC_PREFIX
#define MD_EXEC_PREFIX		"/usr/ccs/gcc/"

#undef  MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"

/* Specify size_t, ptrdiff_t, and wchar_t types.  */
#undef	SIZE_TYPE
#undef	PTRDIFF_TYPE
#undef	WCHAR_TYPE
#undef	WCHAR_TYPE_SIZE

#define SIZE_TYPE	"long unsigned int"
#define PTRDIFF_TYPE	"int"
#define WCHAR_TYPE	"unsigned int"
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Define this macro if the system header files support C++ as well
   as C.  This macro inhibits the usual method of using system header
   files in C++, which is to pretend that the file's contents are
   enclosed in `extern "C" {...}'. */
#define NO_IMPLICIT_EXTERN_C

/* Turn off long double being 96 bits.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.

   We override it here to allow for the new profiling code to go before
   the prologue and the old mcount code to go after the prologue (and
   after %ebx has been set up for ELF shared library support).  */

#define OSF_PROFILE_BEFORE_PROLOGUE					\
  (!TARGET_MCOUNT							\
   && !current_function_needs_context					\
   && (!flag_pic							\
       || !frame_pointer_needed						\
       || (!current_function_uses_pic_offset_table			\
	   && !current_function_uses_const_pool)))

#undef	FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE, SIZE)					\
do									\
  {									\
    char *prefix = (TARGET_UNDERSCORES) ? "_" : "";			\
    char *lprefix = LPREFIX;						\
    int labelno = profile_label_no;					\
									\
    if (profile_flag && OSF_PROFILE_BEFORE_PROLOGUE)			\
      {									\
	if (!flag_pic && !HALF_PIC_P ())				\
	  {								\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall *%s_mcount_ptr\n", prefix);		\
	  }								\
									\
	else if (HALF_PIC_P ())						\
	  {								\
	    rtx symref;							\
									\
	    HALF_PIC_EXTERNAL ("_mcount_ptr");				\
	    symref = HALF_PIC_PTR (gen_rtx (SYMBOL_REF, Pmode,		\
					    "_mcount_ptr"));		\
									\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tmovl %s%s,%%eax\n", prefix,		\
		     XSTR (symref, 0));					\
	    fprintf (FILE, "\tcall *(%%eax)\n");			\
	  }								\
									\
	else								\
	  {								\
	    static int call_no = 0;					\
									\
	    fprintf (FILE, "\tcall %sPc%d\n", lprefix, call_no);	\
	    fprintf (FILE, "%sPc%d:\tpopl %%eax\n", lprefix, call_no);	\
	    fprintf (FILE, "\taddl $_GLOBAL_OFFSET_TABLE_+[.-%sPc%d],%%eax\n", \
		     lprefix, call_no++);				\
	    fprintf (FILE, "\tleal %sP%d@GOTOFF(%%eax),%%edx\n",	\
		     lprefix, labelno);					\
	    fprintf (FILE, "\tmovl %s_mcount_ptr@GOT(%%eax),%%eax\n",	\
		     prefix);						\
	    fprintf (FILE, "\tcall *(%%eax)\n");			\
	  }								\
      }									\
									\
    function_prologue (FILE, SIZE);					\
  }									\
while (0)

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results. */

#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
do									\
  {									\
    if (!OSF_PROFILE_BEFORE_PROLOGUE)					\
      {									\
	char *prefix = (TARGET_UNDERSCORES) ? "_" : "";			\
	char *lprefix = LPREFIX;					\
	int labelno = LABELNO;						\
									\
	/* Note that OSF/rose blew it in terms of calling mcount,	\
	   since OSF/rose prepends a leading underscore, but mcount's	\
	   doesn't.  At present, we keep this kludge for ELF as well	\
	   to allow old kernels to build profiling.  */			\
									\
	if (flag_pic							\
	    && !current_function_uses_pic_offset_table			\
	    && !current_function_uses_const_pool)			\
	  abort ();							\
									\
	if (TARGET_MCOUNT && flag_pic)					\
	  {								\
	    fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%edx\n",	\
		     lprefix, labelno);					\
	    fprintf (FILE, "\tcall *%smcount@GOT(%%ebx)\n", prefix);	\
	  }								\
									\
	else if (TARGET_MCOUNT && HALF_PIC_P ())			\
	  {								\
	    rtx symdef;							\
									\
	    HALF_PIC_EXTERNAL ("mcount");				\
	    symdef = HALF_PIC_PTR (gen_rtx (SYMBOL_REF, Pmode, "mcount")); \
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall *%s%s\n", prefix, XSTR (symdef, 0));	\
	  }								\
									\
	else if (TARGET_MCOUNT)						\
	  {								\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall %smcount\n", prefix);		\
	  }								\
									\
	else if (flag_pic && frame_pointer_needed)			\
	  {								\
	    fprintf (FILE, "\tmovl 4(%%ebp),%%ecx\n");			\
	    fprintf (FILE, "\tpushl %%ecx\n");				\
	    fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%edx\n",	\
		     lprefix, labelno);					\
	    fprintf (FILE, "\tmovl _mcount_ptr@GOT(%%ebx),%%eax\n");	\
	    fprintf (FILE, "\tcall *(%%eax)\n");			\
	    fprintf (FILE, "\tpopl %%eax\n");				\
	  }								\
									\
	else if (frame_pointer_needed)					\
	  {								\
	    fprintf (FILE, "\tmovl 4(%%ebp),%%ecx\n");			\
	    fprintf (FILE, "\tpushl %%ecx\n");				\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall *_mcount_ptr\n");			\
	    fprintf (FILE, "\tpopl %%eax\n");				\
	  }								\
									\
	else								\
	  abort ();							\
      }									\
  }									\
while (0)

/* A C function or functions which are needed in the library to
   support block profiling.  When support goes into libc, undo
   the #if 0.  */

#if 0
#undef	BLOCK_PROFILING_CODE
#define	BLOCK_PROFILING_CODE
#endif

/* Prefix for internally generated assembler labels.  If we aren't using
   underscores, we are using prefix `.'s to identify labels that should
   be ignored, as in `i386/gas.h' --karl@cs.umb.edu  */
#undef	LPREFIX
#define	LPREFIX ((TARGET_UNDERSCORES) ? "L" : ".L")

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef	ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)			\
    sprintf ((BUF), "*%s%s%d", (TARGET_UNDERSCORES) ? "" : ".",		\
	     (PREFIX), (NUMBER))

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef	ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
  fprintf (FILE, "%s%s%d:\n", (TARGET_UNDERSCORES) ? "" : ".",		\
	   PREFIX, NUM)

/* This is how to output a reference to a user-level label named NAME.  */

#undef	ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)					\
  fprintf (FILE, "%s%s", (TARGET_UNDERSCORES) ? "_" : "", NAME)

/* This is how to output an element of a case-vector that is relative.
   This is only used for PIC code.  See comments by the `casesi' insn in
   i386.md for an explanation of the expression this outputs. */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t.long _GLOBAL_OFFSET_TABLE_+[.-%s%d]\n", LPREFIX, VALUE)

/* Output a definition */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
do									\
{									\
    fprintf ((FILE), "\t%s\t", SET_ASM_OP);				\
    assemble_name (FILE, LABEL1);					\
    fprintf (FILE, ",");						\
    assemble_name (FILE, LABEL2);					\
    fprintf (FILE, "\n");						\
    }									\
while (0)

/* A C expression to output text to align the location counter in the
   way that is desirable at a point in the code that is reached only
   by jumping.

   This macro need not be defined if you don't want any special
   alignment to be done at such a time.  Most machine descriptions do
   not currently define the macro.  */

#undef	ASM_OUTPUT_ALIGN_CODE
#define ASM_OUTPUT_ALIGN_CODE(STREAM)					\
  fprintf (STREAM, "\t.align\t%d\n",					\
	   (!TARGET_LARGE_ALIGN && i386_align_jumps > 2) ? 2 : i386_align_jumps)

/* A C expression to output text to align the location counter in the
   way that is desirable at the beginning of a loop.

   This macro need not be defined if you don't want any special
   alignment to be done at such a time.  Most machine descriptions do
   not currently define the macro.  */

#undef	ASM_OUTPUT_LOOP_ALIGN
#define ASM_OUTPUT_LOOP_ALIGN(STREAM) \
  fprintf (STREAM, "\t.align\t%d\n", i386_align_loops)

/* A C statement to output to the stdio stream STREAM an assembler
   command to advance the location counter to a multiple of 2 to the
   POWER bytes.  POWER will be a C expression of type `int'.  */

#undef	ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(STREAM, POWER)					\
  fprintf (STREAM, "\t.align\t%d\n",					\
	   (!TARGET_LARGE_ALIGN && (POWER) > 2) ? 2 : (POWER))

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  On most machines, this can be defined as
   `CONSTANT_P (X)', but a few machines are more restrictive in
   which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are
   not explicitly known, such as `symbol_ref', `label_ref', and
   `high' expressions and `const' arithmetic expressions, in
   addition to `const_int' and `const_double' expressions.  */

#define CONSTANT_ADDRESS_P_ORIG(X)					\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

#undef	CONSTANT_ADDRESS_P
#define CONSTANT_ADDRESS_P(X)                                           \
  ((CONSTANT_ADDRESS_P_ORIG (X)) && (!HALF_PIC_P () || !HALF_PIC_ADDRESS_P (X)))

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#undef	LEGITIMATE_CONSTANT_P
#define LEGITIMATE_CONSTANT_P(X)					\
  (!HALF_PIC_P ()							\
   || GET_CODE (X) == CONST_DOUBLE					\
   || GET_CODE (X) == CONST_INT						\
   || !HALF_PIC_ADDRESS_P (X))

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.  */

#undef  SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS					\
{									\
  /*									\
  if (TARGET_ELF && TARGET_HALF_PIC)					\
    {									\
      target_flags &= ~MASK_HALF_PIC;					\
      flag_pic = 1;							\
    }									\
  */									\
									\
  if (TARGET_ROSE && flag_pic)						\
    {									\
      target_flags |= MASK_HALF_PIC;					\
      flag_pic = 0;							\
    }									\
									\
  if (TARGET_HALF_PIC)							\
    half_pic_init ();							\
}

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL has been created and stored in `DECL_RTL (DECL)'.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   The best way to modify the name string is by adding text to the
   beginning, with suitable punctuation to prevent any ambiguity.
   Allocate the new name in `saveable_obstack'.  You will have to
   modify `ASM_OUTPUT_LABELREF' to remove and decode the added text
   and output the name accordingly.

   You can also check the information stored in the `symbol_ref' in
   the definition of `GO_IF_LEGITIMATE_ADDRESS' or
   `PRINT_OPERAND_ADDRESS'. */

#undef	ENCODE_SECTION_INFO
#define ENCODE_SECTION_INFO(DECL)					\
do									\
  {									\
   if (HALF_PIC_P ())							\
      HALF_PIC_ENCODE (DECL);						\
									\
   else if (flag_pic)							\
     {									\
       rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		  ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));		\
       SYMBOL_REF_FLAG (XEXP (rtl, 0))					\
	 = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'			\
	    || ! TREE_PUBLIC (DECL));					\
      }									\
  }									\
while (0)


/* On most machines, read-only variables, constants, and jump tables
   are placed in the text section.  If this is not the case on your
   machine, this macro should be defined to be the name of a function
   (either `data_section' or a function defined in `EXTRA_SECTIONS')
   that switches to the section to be used for read-only items.

   If these items should be placed in the text section, this macro
   should not be defined.  */

#if 0
#undef	READONLY_DATA_SECTION
#define READONLY_DATA_SECTION()						\
do									\
  {									\
    if (TARGET_ELF)							\
      {									\
	if (in_section != in_rodata)					\
	  {								\
	    fprintf (asm_out_file, "\t.section \"rodata\"\n");		\
	    in_section = in_rodata;					\
	  }								\
      }									\
    else								\
      text_section ();							\
  }									\
while (0)
#endif

/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro on a
   system with no other sections (that GCC needs to use).  */

#undef	EXTRA_SECTIONS
#define	EXTRA_SECTIONS in_rodata, in_data1

/* Given a decl node or constant node, choose the section to output it in
   and select that section.  */

#undef	SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, RTX)					\
do									\
  {									\
    if (MODE == Pmode && HALF_PIC_P () && HALF_PIC_ADDRESS_P (RTX))	\
      data_section ();							\
    else								\
      readonly_data_section ();						\
  }									\
while (0)

#undef	SELECT_SECTION
#define SELECT_SECTION(DECL, RELOC)					\
{									\
  if (RELOC && HALF_PIC_P ())						\
    data_section ();							\
									\
  else if (TREE_CODE (DECL) == STRING_CST)				\
    {									\
      if (flag_writable_strings)					\
	data_section ();						\
      else								\
	readonly_data_section ();					\
    }									\
									\
  else if (TREE_CODE (DECL) != VAR_DECL)				\
    readonly_data_section ();						\
									\
  else if (!TREE_READONLY (DECL) || TREE_SIDE_EFFECTS (DECL)		\
	   || !DECL_INITIAL (DECL)					\
	   || (DECL_INITIAL (DECL) != error_mark_node			\
	       && !TREE_CONSTANT (DECL_INITIAL (DECL))))		\
    data_section ();							\
									\
  else									\
    readonly_data_section ();						\
}


/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#define TYPE_ASM_OP	".type"
#define SIZE_ASM_OP	".size"
#define SET_ASM_OP	".set"

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define TYPE_OPERAND_FMT	"@%s"

/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the name NAME of an
   initialized variable which is being defined.  This macro must
   output the label definition (perhaps using `ASM_OUTPUT_LABEL').
   The argument DECL is the `VAR_DECL' tree node representing the
   variable.

   If this macro is not defined, then the variable name is defined
   in the usual manner as a label (by means of `ASM_OUTPUT_LABEL').  */

#undef	ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)			     \
do									     \
 {									     \
   ASM_OUTPUT_LABEL(STREAM,NAME);					     \
   HALF_PIC_DECLARE (NAME);						     \
   if (TARGET_ELF)							     \
     {									     \
       fprintf (STREAM, "\t%s\t ", TYPE_ASM_OP);			     \
       assemble_name (STREAM, NAME);					     \
       putc (',', STREAM);						     \
       fprintf (STREAM, TYPE_OPERAND_FMT, "object");			     \
       putc ('\n', STREAM);						     \
       size_directive_output = 0;					     \
       if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		     \
	 {								     \
           size_directive_output = 1;					     \
	   fprintf (STREAM, "\t%s\t ", SIZE_ASM_OP);			     \
	   assemble_name (STREAM, NAME);				     \
	   fprintf (STREAM, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL))); \
	 }								     \
     }									     \
 }									     \
while (0)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);			 \
     if (TARGET_ELF							 \
	 && !flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);			 \
	 assemble_name (FILE, name);					 \
	 fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (DECL))); \
       }								 \
   } while (0)

/* This is how to declare a function name. */

#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)			\
do									\
 {									\
   ASM_OUTPUT_LABEL(STREAM,NAME);					\
   HALF_PIC_DECLARE (NAME);						\
   if (TARGET_ELF)							\
     {									\
       fprintf (STREAM, "\t%s\t ", TYPE_ASM_OP);			\
       assemble_name (STREAM, NAME);					\
       putc (',', STREAM);						\
       fprintf (STREAM, TYPE_OPERAND_FMT, "function");			\
       putc ('\n', STREAM);						\
       ASM_DECLARE_RESULT (STREAM, DECL_RESULT (DECL));			\
     }									\
 }									\
while (0)

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

/* This is how to declare the size of a function.  */

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
do									\
  {									\
    if (TARGET_ELF && !flag_inhibit_size_directive)			\
      {									\
        char label[256];						\
	static int labelno;						\
	labelno++;							\
	ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",");						\
	assemble_name (FILE, label);					\
        fprintf (FILE, "-");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }									\
  }									\
while (0)

/* Attach a special .ident directive to the end of the file to identify
   the version of GCC which compiled this code.  The format of the
   .ident string is patterned after the ones produced by native svr4
   C compilers.  */

#define IDENT_ASM_OP ".ident"

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* This says what to print at the end of the assembly file */
#define ASM_FILE_END(STREAM)						\
do									\
  {									\
    if (HALF_PIC_P ())							\
      HALF_PIC_FINISH (STREAM);						\
									\
    if (TARGET_IDENT)							\
      {									\
	char *fstart = main_input_filename;				\
	char *fname;							\
									\
	if (!fstart)							\
	  fstart = "<no file>";						\
									\
	fname = fstart + strlen (fstart) - 1;				\
	while (fname > fstart && *fname != '/')				\
	  fname--;							\
									\
	if (*fname == '/')						\
	  fname++;							\
									\
	fprintf ((STREAM), "\t%s\t\"GCC: (GNU) %s %s -O%d",		\
		 IDENT_ASM_OP, version_string, fname, optimize);	\
									\
	if (write_symbols == PREFERRED_DEBUGGING_TYPE)			\
	  fprintf ((STREAM), " -g%d", (int)debug_info_level);		\
									\
	else if (write_symbols == DBX_DEBUG)				\
	  fprintf ((STREAM), " -gstabs%d", (int)debug_info_level);	\
									\
	else if (write_symbols == DWARF_DEBUG)				\
	  fprintf ((STREAM), " -gdwarf%d", (int)debug_info_level);	\
									\
	else if (write_symbols != NO_DEBUG)				\
	  fprintf ((STREAM), " -g??%d", (int)debug_info_level);		\
									\
	if (flag_omit_frame_pointer)					\
	  fprintf ((STREAM), " -fomit-frame-pointer");			\
									\
	if (flag_strength_reduce)					\
	  fprintf ((STREAM), " -fstrength-reduce");			\
									\
	if (flag_unroll_loops)						\
	  fprintf ((STREAM), " -funroll-loops");			\
									\
	if (flag_schedule_insns)					\
	  fprintf ((STREAM), " -fschedule-insns");			\
									\
	if (flag_schedule_insns_after_reload)				\
	  fprintf ((STREAM), " -fschedule-insns2");			\
									\
	if (flag_force_mem)						\
	  fprintf ((STREAM), " -fforce-mem");				\
									\
	if (flag_force_addr)						\
	  fprintf ((STREAM), " -fforce-addr");				\
									\
	if (flag_inline_functions)					\
	  fprintf ((STREAM), " -finline-functions");			\
									\
	if (flag_caller_saves)						\
	  fprintf ((STREAM), " -fcaller-saves");			\
									\
	if (flag_pic)							\
	  fprintf ((STREAM), (flag_pic > 1) ? " -fPIC" : " -fpic");	\
									\
	if (flag_inhibit_size_directive)				\
	  fprintf ((STREAM), " -finhibit-size-directive");		\
									\
	if (flag_gnu_linker)						\
	  fprintf ((STREAM), " -fgnu-linker");				\
									\
	if (profile_flag)						\
	  fprintf ((STREAM), " -p");					\
									\
	if (profile_block_flag)						\
	  fprintf ((STREAM), " -a");					\
									\
	if (TARGET_IEEE_FP)						\
	  fprintf ((STREAM), " -mieee-fp");				\
									\
	if (TARGET_HALF_PIC)						\
	  fprintf ((STREAM), " -mhalf-pic");				\
									\
	if (!TARGET_MOVE)						\
	  fprintf ((STREAM), " -mno-move");				\
									\
	if (TARGET_386)							\
	  fprintf ((STREAM), " -m386");					\
									\
	else if (TARGET_486)						\
	  fprintf ((STREAM), " -m486");					\
									\
	else								\
	  fprintf ((STREAM), " -munknown-machine");			\
									\
	fprintf ((STREAM), (TARGET_ELF) ? " -melf\"\n" : " -mrose\"\n"); \
      }									\
  }									\
while (0)

/* Tell collect that the object format is OSF/rose.  */
#define OBJECT_FORMAT_ROSE

/* Tell collect where the appropriate binaries are.  */
#define REAL_NM_FILE_NAME	"/usr/ccs/gcc/bfd-nm"
#define REAL_STRIP_FILE_NAME	"/usr/ccs/bin/strip"

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Define this macro meaning that gcc should find the library 'libgcc.a'
   by hand, rather than passing the argument '-lgcc' to tell the linker
   to do the search */
#define LINK_LIBGCC_SPECIAL

/* A C statement to output assembler commands which will identify the object
  file as having been compile with GNU CC. We don't need or want this for
  OSF1. GDB doesn't need it and kdb doesn't like it */
#define ASM_IDENTIFY_GCC(FILE)

/* Identify the front-end which produced this file.  To keep symbol
   space down, and not confuse kdb, only do this if the language is
   not C.  */

#define ASM_IDENTIFY_LANGUAGE(STREAM)					\
{									\
  if (strcmp (lang_identify (), "c") != 0)				\
    output_lang_identify (STREAM);					\
}

/* Generate calls to memcpy, etc., not bcopy, etc. */
#define TARGET_MEM_FUNCTIONS

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Map i386 registers to the numbers dwarf expects.  Of course this is different
   from what stabs expects.  */

#define DWARF_DBX_REGISTER_NUMBER(n) \
((n) == 0 ? 0 \
 : (n) == 1 ? 2 \
 : (n) == 2 ? 1 \
 : (n) == 3 ? 3 \
 : (n) == 4 ? 6 \
 : (n) == 5 ? 7 \
 : (n) == 6 ? 5 \
 : (n) == 7 ? 4 \
 : ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG) ? (n)+3 \
 : (-1))

/* Now what stabs expects in the register.  */
#define STABS_DBX_REGISTER_NUMBER(n) \
((n) == 0 ? 0 : \
 (n) == 1 ? 2 : \
 (n) == 2 ? 1 : \
 (n) == 3 ? 3 : \
 (n) == 4 ? 6 : \
 (n) == 5 ? 7 : \
 (n) == 6 ? 4 : \
 (n) == 7 ? 5 : \
 (n) + 4)

#undef	DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) ((write_symbols == DWARF_DEBUG)		\
				? DWARF_DBX_REGISTER_NUMBER(n)		\
				: STABS_DBX_REGISTER_NUMBER(n))
