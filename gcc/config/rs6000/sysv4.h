/* Target definitions for GNU compiler for PowerPC running System V.4
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

/* eABI local switches -- put here rather than eabi.h, so the switches
   can be tested in macros.  */

#define	MASK_NO_BITFIELD_TYPE	0x40000000	/* Set PCC_BITFIELD_TYPE_MATTERS to 0 */
#define	MASK_STRICT_ALIGN	0x20000000	/* Set STRICT_ALIGNMENT to 1.  */
#define MASK_RELOCATABLE	0x10000000	/* GOT pointers are PC relative */
#define	MASK_UNUSED		0x08000000	/* UNUSED, was no-traceback */
#define MASK_LITTLE_ENDIAN	0x04000000	/* target is little endian */
#define MASK_CALLS_1		0x02000000	/* First ABI bit (AIX, AIXDESC) */
#define MASK_PROTOTYPE		0x01000000	/* Only prototyped fcns pass variable args */
#define	MASK_CALLS_2		0x00800000	/* Second ABI bit (NT) */

#define	MASK_CALLS		(MASK_CALLS_1 | MASK_CALLS_2)
#define	MASK_CALLS_V4		0
#define	MASK_CALLS_AIX		MASK_CALLS_1
#define	MASK_CALLS_NT		MASK_CALLS_2
#define	MASK_CALLS_AIXDESC	MASK_CALLS

#define	TARGET_NO_BITFIELD_TYPE	(target_flags & MASK_NO_BITFIELD_TYPE)
#define TARGET_STRICT_ALIGN	(target_flags & MASK_STRICT_ALIGN)
#define TARGET_RELOCATABLE	(target_flags & MASK_RELOCATABLE)
#define TARGET_LITTLE_ENDIAN	(target_flags & MASK_LITTLE_ENDIAN)
#define	TARGET_PROTOTYPE	(target_flags & MASK_PROTOTYPE)
#define	TARGET_TOC		((target_flags & (MASK_64BIT		\
						 | MASK_RELOCATABLE	\
						 | MASK_MINIMAL_TOC))	\
				 || DEFAULT_ABI == ABI_AIX		\
				 || DEFAULT_ABI == ABI_NT)

#define	TARGET_BITFIELD_TYPE	(! TARGET_NO_BITFIELD_TYPE)
#define TARGET_BIG_ENDIAN	(! TARGET_LITTLE_ENDIAN)
#define	TARGET_NO_PROTOTYPE	(! TARGET_PROTOTYPE)
#define	TARGET_NO_TOC		(! TARGET_TOC)

#define TARGET_AIX_CALLS	(target_flags & MASK_CALLS_1)	/* either -mcall-aix or -mcall-aixdesc */
#define TARGET_V4_CALLS		((target_flags & MASK_CALLS) == MASK_CALLS_V4)
#define TARGET_NT_CALLS		((target_flags & MASK_CALLS) == MASK_CALLS_NT)
#define TARGET_AIXDESC_CALLS	((target_flags & MASK_CALLS) == MASK_CALLS_AIXDESC)

/* Pseudo target to indicate whether the object format is ELF
   (to get around not having conditional compilation in the md file)  */
#define	TARGET_ELF		1

/* Note, V.4 no longer uses a normal TOC, so make -mfull-toc, be just
   the same as -mminimal-toc.  */
#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
  { "bit-align",	-MASK_NO_BITFIELD_TYPE },			\
  { "no-bit-align",	 MASK_NO_BITFIELD_TYPE },			\
  { "strict-align",	 MASK_STRICT_ALIGN },				\
  { "no-strict-align",	-MASK_STRICT_ALIGN },				\
  { "relocatable",	 MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC }, \
  { "no-relocatable",	-MASK_RELOCATABLE },				\
  { "relocatable-lib",	 MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC }, \
  { "no-relocatable-lib", -MASK_RELOCATABLE },				\
  { "little-endian",	 MASK_LITTLE_ENDIAN },				\
  { "little",		 MASK_LITTLE_ENDIAN },				\
  { "big-endian",	-MASK_LITTLE_ENDIAN },				\
  { "big",		-MASK_LITTLE_ENDIAN },				\
  { "no-toc",		 0 },						\
  { "toc",		 MASK_MINIMAL_TOC },				\
  { "full-toc",		 MASK_MINIMAL_TOC },				\
  { "call-aix",		 MASK_CALLS_AIX },				\
  { "call-aix",		-MASK_CALLS_NT },				\
  { "call-aixdesc",	 MASK_CALLS_AIXDESC },				\
  { "call-aixdesc",	-MASK_LITTLE_ENDIAN },				\
  { "call-sysv",	-MASK_CALLS },					\
  { "call-nt",		 MASK_CALLS_NT | MASK_LITTLE_ENDIAN },		\
  { "call-nt",		-MASK_CALLS_AIX },				\
  { "prototype",	 MASK_PROTOTYPE },				\
  { "no-prototype",	-MASK_PROTOTYPE },				\
  { "no-traceback",	 0 },						\
  { "sim",		 0 },						\
  { "mvme",		 0 },						\
  { "emb",		 0 },						\

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   The macro SUBTARGET_OVERRIDE_OPTIONS is provided for subtargets, to
   get control.  */

#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (TARGET_RELOCATABLE && !TARGET_MINIMAL_TOC)			\
    {									\
      target_flags |= MASK_MINIMAL_TOC;					\
      error ("-mrelocatable and -mno-minimal-toc are incompatible.");	\
    }									\
									\
  if (TARGET_RELOCATABLE && TARGET_AIXDESC_CALLS)			\
    {									\
      target_flags &= ~MASK_RELOCATABLE;				\
      error ("-mrelocatable and -mcall-aixdesc are incompatible.");	\
    }									\
									\
  if (TARGET_RELOCATABLE && TARGET_NT_CALLS)				\
    {									\
      target_flags &= ~MASK_MINIMAL_TOC;				\
      error ("-mrelocatable and -mcall-nt are incompatible.");		\
    }									\
									\
  if (TARGET_AIXDESC_CALLS && TARGET_LITTLE_ENDIAN)			\
    {									\
      target_flags &= ~MASK_LITTLE_ENDIAN;				\
      error ("-mcall-aixdesc must be big endian");			\
    }									\
									\
  if (TARGET_NT_CALLS && TARGET_BIG_ENDIAN)				\
    {									\
      target_flags |= MASK_LITTLE_ENDIAN;				\
      error ("-mcall-nt must be little endian");			\
    }									\
									\
  rs6000_current_abi = ((TARGET_AIXDESC_CALLS) ? ABI_AIX :		\
			(TARGET_NT_CALLS)      ? ABI_NT :		\
			(TARGET_AIX_CALLS)     ? ABI_AIX_NODESC :	\
						 ABI_V4);		\
} while (0)

/* Default ABI to compile code for */
#define DEFAULT_ABI rs6000_current_abi

#include "rs6000/powerpc.h"

/* System V.4 uses register 13 as a pointer to the small data area,
   so it is not available to the normal user.  */

#undef	FIXED_R13
#define FIXED_R13 1

/* System V.4 passes the first 8 floating arguments in registers,
   instead of the first 13 like AIX does.  */
#undef	FP_ARG_MAX_REG
#define	FP_ARG_AIX_MAX_REG	45
#define	FP_ARG_V4_MAX_REG	40
#define	FP_ARG_MAX_REG ((TARGET_AIX_CALLS) ? FP_ARG_AIX_MAX_REG : FP_ARG_V4_MAX_REG)

/* Size of the V.4 varargs area if needed */
#undef	RS6000_VARARGS_AREA
#define RS6000_VARARGS_AREA ((rs6000_sysv_varargs_p) ? RS6000_VARARGS_SIZE : 0)

/* Override default big endianism */
#undef  BYTES_BIG_ENDIAN
#undef  WORDS_BIG_ENDIAN
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifndef _LITTLE_ENDIAN
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Size of the outgoing register save area */
#undef	RS6000_REG_SAVE
#define RS6000_REG_SAVE (TARGET_AIX_CALLS ? (TARGET_64BIT ? 64 : 32) : 0)

/* Size of the fixed area on the stack.  For AIX, use the standard 6 word
   area, otherwise use 2 words to store back chain & LR.  */
#undef	RS6000_SAVE_AREA
#define RS6000_SAVE_AREA \
  ((TARGET_AIX_CALLS ? 24 : 8) << (TARGET_64BIT ? 1 : 0))

/* Define cutoff for using external functions to save floating point.
   Currently on V.4, always use inline stores */
#undef	FP_SAVE_INLINE
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 64)

/* Don't generate XCOFF debugging information.  */

#undef XCOFF_DEBUGGING_INFO

/* Don't use the COFF object file format.  */

#undef OBJECT_FORMAT_COFF

/* Don't bother to output .extern pseudo-ops.  They are not needed by
   ELF assemblers.  */

#undef ASM_OUTPUT_EXTERNAL

/* Undefine some things which are defined by the generic svr4.h.  */

#undef ASM_FILE_END
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#undef READONLY_DATA_SECTION
#undef SELECT_SECTION
#undef ASM_DECLARE_FUNCTION_NAME
#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR

/* Use the regular svr4 definitions.  */

#include "svr4.h"

/* Prefix and suffix to use to saving floating point */
#undef	SAVE_FP_PREFIX
#undef	SAVE_FP_SUFFIX
#define	SAVE_FP_PREFIX "_savefpr_"
#define SAVE_FP_SUFFIX "_l"

/* Prefix and suffix to use to restoring floating point */
#undef	RESTORE_FP_PREFIX
#undef	RESTORE_FP_SUFFIX
#define	RESTORE_FP_PREFIX "_restfpr_"
#define RESTORE_FP_SUFFIX "_l"

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#undef	PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Type used for wchar_t, as a string used in a declaration.  */
#undef	WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

/* Width of wchar_t in bits.  */
#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Align stack to 16 byte boundaries */
#undef	STACK_BOUNDARY
#define	STACK_BOUNDARY	128

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#undef  BIGGEST_FIELD_ALIGNMENT
#define BIGGEST_ALIGNMENT 128

/* Use ELF style section commands.  */

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP	"\t.section\t\".text\""

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP	"\t.section\t\".data\""

/* Besides the usual ELF sections, we need a toc section.  */
#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_toc

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  TOC_SECTION_FUNCTION

#define TOC_SECTION_FUNCTION						\
void									\
toc_section ()								\
{									\
  static int toc_initialized = 0;					\
									\
  if (in_section != in_toc)						\
    {									\
      in_section = in_toc;						\
      if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_NT)		\
	  && TARGET_MINIMAL_TOC						\
	  && !TARGET_RELOCATABLE)					\
	{								\
	  if (! toc_initialized)					\
	    {								\
	      toc_initialized = 1;					\
	      fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);	\
	      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LCTOC", 0);	\
	      fprintf (asm_out_file, "\t.tc ");				\
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1[TC],"); \
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1"); \
	      fprintf (asm_out_file, "\n");				\
									\
	      fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP); \
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1"); \
	      fprintf (asm_out_file, " = .+32768\n");			\
	    }								\
	  else								\
	    fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);	\
	}								\
      else if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_NT)	\
	       && !TARGET_RELOCATABLE)					\
	fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);		\
      else								\
	{								\
	  fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);	\
	  if (! toc_initialized)					\
	    {								\
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1"); \
	      fprintf (asm_out_file, " = .+32768\n");			\
	      toc_initialized = 1;					\
	    }								\
	}								\
    }									\
}

#define TOC_SECTION_ASM_OP "\t.section\t\".got\",\"aw\""
#define MINIMAL_TOC_SECTION_ASM_OP "\t.section\t\".got1\",\"aw\""

/* Use the TOC section for TOC entries.  */

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, X)		\
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X))	\
    toc_section ();				\
  else						\
    const_section ();				\
}

/* Return non-zero if this entry is to be written into the constant pool
   in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF or a CONST
   containing one of them.  If -mfp-in-toc (the default), we also do
   this for floating-point constants.  We actually can only do this
   if the FP formats of the target and host machines are the same, but
   we can't check that since not every file that uses
   GO_IF_LEGITIMATE_ADDRESS_P includes real.h.

   Unlike AIX, we don't key off of -mmininal-toc, but instead do not
   allow floating point constants in the TOC if -mrelocatable.  */

#undef	ASM_OUTPUT_SPECIAL_POOL_ENTRY_P
#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X)				\
  (TARGET_TOC								\
   && (GET_CODE (X) == SYMBOL_REF					\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
       || GET_CODE (X) == LABEL_REF					\
       || (!TARGET_NO_FP_IN_TOC						\
	   && !TARGET_RELOCATABLE					\
	   && GET_CODE (X) == CONST_DOUBLE				\
	   && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT		\
	   && BITS_PER_WORD == HOST_BITS_PER_INT)))

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

extern int rs6000_pic_labelno;

#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    char *orig_name;							\
    char *init_ptr = (TARGET_64BIT) ? ".quad" : ".long";		\
    STRIP_NAME_ENCODING (orig_name, NAME);				\
									\
    if (TARGET_RELOCATABLE && get_pool_size () != 0)			\
      {									\
	char buf[256], *buf_ptr;					\
									\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "LCL", rs6000_pic_labelno);	\
									\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);			\
	STRIP_NAME_ENCODING (buf_ptr, buf);				\
	fprintf (FILE, "\t%s %s-", init_ptr, buf_ptr);			\
									\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);	\
	fprintf (FILE, "%s\n", buf_ptr);				\
      }									\
									\
    fprintf (FILE, "\t%s\t %s,", TYPE_ASM_OP, orig_name);		\
    fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', FILE);							\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
									\
    if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_NT)		\
      {									\
	char *desc_name = orig_name;					\
									\
	while (*desc_name == '.')					\
	  desc_name++;							\
									\
	if (TREE_PUBLIC (DECL))						\
	  fprintf (FILE, "\t.globl %s\n", desc_name);			\
									\
	fprintf (FILE, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);		\
	fprintf (FILE, "%s:\n", desc_name);				\
	fprintf (FILE, "\t%s %s\n", init_ptr, orig_name);		\
	fprintf (FILE, "\t%s _GLOBAL_OFFSET_TABLE_\n", init_ptr);	\
	if (DEFAULT_ABI == ABI_AIX)					\
	  fprintf (FILE, "\t%s 0\n", init_ptr);				\
	fprintf (FILE, "\t.previous\n");				\
      }									\
    fprintf (FILE, "%s:\n", orig_name);					\
  } while (0)

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* svr4.h overrides ASM_OUTPUT_INTERNAL_LABEL.  */

#undef ASM_OUTPUT_INTERNAL_LABEL_PREFIX
#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, ".%s", PREFIX)

/* Pass -mppc to the assembler, since that is what powerpc.h currently
   implies.  */
#undef ASM_SPEC
#define ASM_SPEC "-u %(asm_cpu) \
%{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} \
%{mrelocatable} %{mrelocatable-lib} %{memb} \
%{mlittle} %{mlittle-endian} %{mbig} %{mbig-endian}"

/* Output .file and comments listing what options there are */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
do {									\
  ASM_OUTPUT_OPTIONS (FILE);						\
  output_file_directive ((FILE), main_input_filename);			\
} while (0)


/* This is how to output an assembler line defining an `int' constant.
   For -mrelocatable, we mark all addresses that need to be fixed up
   in the .fixup section.  */
#undef	ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(FILE,VALUE)					\
do {									\
  static int recurse = 0;						\
  if (TARGET_RELOCATABLE						\
      && in_section != in_toc						\
      && in_section != in_text						\
      && in_section != in_ctors						\
      && in_section != in_dtors						\
      && !recurse							\
      && GET_CODE (VALUE) != CONST_INT					\
      && GET_CODE (VALUE) != CONST_DOUBLE				\
      && CONSTANT_P (VALUE))						\
    {									\
      static int labelno = 0;						\
      char buf[256], *p;						\
									\
      recurse = 1;							\
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", labelno++);		\
      STRIP_NAME_ENCODING (p, buf);					\
      fprintf (FILE, "%s:\n", p);					\
      fprintf (FILE, "\t.long (");					\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, ")@fixup\n");					\
      fprintf (FILE, "\t.section\t\".fixup\",\"aw\"\n");		\
      ASM_OUTPUT_ALIGN (FILE, 2);					\
      fprintf (FILE, "\t.long\t%s\n", p);				\
      fprintf (FILE, "\t.previous\n");					\
      recurse = 0;							\
    }									\
  /* Remove initial .'s to turn a -mcall-aixdesc or -mcall-nt function	\
     address into the address of the descriptor, not the function	\
     itself.  */							\
  else if (GET_CODE (VALUE) == SYMBOL_REF				\
	   && XSTR (VALUE, 0)[0] == '.'					\
	   && (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_NT))	\
    {									\
      char *name = XSTR (VALUE, 0);					\
      while (*name == '.')						\
	name++;								\
									\
      fprintf (FILE, "\t.long %s\n", name);				\
    }									\
  else									\
    {									\
      fprintf (FILE, "\t.long ");					\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, "\n");						\
    }									\
} while (0)

/* This is the end of what might become sysv4.h.  */

/* Allow stabs and dwarf, prefer dwarf.  */
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
#define	DBX_DEBUGGING_INFO
#define	DWARF_DEBUGGING_INFO

/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  For real AIX and NT calling sequences, we also replace the
   function name with the real name (1 or 2 leading .'s), rather than
   the function descriptor name.  This saves a lot of overriding code
   to readd the prefixes.  */

#undef	ENCODE_SECTION_INFO
#define ENCODE_SECTION_INFO(DECL)					\
  do {									\
    if (TREE_CODE (DECL) == FUNCTION_DECL)				\
      {									\
	rtx sym_ref = XEXP (DECL_RTL (DECL), 0);			\
	if (TREE_ASM_WRITTEN (DECL) || ! TREE_PUBLIC (DECL))		\
	  SYMBOL_REF_FLAG (sym_ref) = 1;				\
									\
	if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_NT)		\
	  {								\
	    char *prefix = (DEFAULT_ABI == ABI_AIX) ? "." : "..";	\
	    char *str = permalloc (strlen (prefix) + 1			\
				   + strlen (XSTR (sym_ref, 0)));	\
	    strcpy (str, prefix);					\
	    strcat (str, XSTR (sym_ref, 0));				\
	    XSTR (sym_ref, 0) = str;					\
	  }								\
      }									\
  } while (0)

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  Discard
   a leading * */
#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME) \
  (VAR) = ((SYMBOL_NAME) + ((SYMBOL_NAME)[0] == '*'))

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */

#define	DBX_FUNCTION_FIRST

/* This is the end of what might become sysv4dbx.h.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC System V.4)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-DPPC -Dunix -D__svr4__ -Asystem(unix) -Asystem(svr4) -Acpu(powerpc) -Amachine(powerpc)"

/* Don't put -Y P,<path> for cross compilers */
#undef LINK_SPEC
#ifdef CROSS_COMPILE
#define LINK_SPEC "\
%{h*} %{V} %{v:%{!V:-V}} \
%{b} %{Wl,*:%*} \
%{static:-dn -Bstatic} \
%{shared:-G -dy -z text %{!h*:%{o*:-h %*}}} \
%{symbolic:-Bsymbolic -G -dy -z text %{!h*:%{o*:-h %*}}} \
%{G:-G} \
%{YP,*} \
%{Qy:} %{!Qn:-Qy} \
%{mlittle: -oformat elf32-powerpcle } %{mlittle-endian: -oformat elf32-powerpcle } \
%{mbig: -oformat elf32-powerpc } %{mbig-endian: -oformat elf32-powerpc }"
#else

#define LINK_SPEC "\
%{h*} %{V} %{v:%{!V:-V}} \
%{b} %{Wl,*:%*} \
%{static:-dn -Bstatic} \
%{shared:-G -dy -z text %{!h*:%{o*:-h %*}}} \
%{symbolic:-Bsymbolic -G -dy -z text %{!h*:%{o*:-h %*}}} \
%{G:-G} \
%{YP,*} \
%{!nostdlib: %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
	     %{!p:-Y P,/usr/ccs/lib:/usr/lib}}} \
%{Qy:} %{!Qn:-Qy} \
%{mlittle: -oformat elf32-powerpcle } %{mlittle-endian: -oformat elf32-powerpcle } \
%{mbig: -oformat elf32-powerpc } %{mbig-endian: -oformat elf32-powerpc }"
#endif /* CROSS_COMPILE */

#undef	CPP_SYSV_SPEC
#define CPP_SYSV_SPEC \
"%{mrelocatable: -D_RELOCATABLE} \
%{mcall-sysv: -D_CALL_SYSV} %{mcall-nt: -D_CALL_NT} \
%{mcall-aix: -D_CALL_AIX} %{mcall-aixdesc: -D_CALL_AIX -D_CALL_AIXDESC} \
%{!mcall-sysv: %{!mcall-aix: %{!mcall-aixdesc: %{!mcall-nt: %(cpp_sysv_default) }}}} \
%{msoft-float: -D_SOFT_FLOAT} %{mcpu=403: -D_SOFT_FLOAT}"

#undef	CPP_SYSV_DEFAULT_SPEC
#define	CPP_SYSV_DEFAULT_SPEC "-D_CALL_SYSV"

#undef	CPP_ENDIAN_SPEC
#define	CPP_ENDIAN_SPEC \
"%{mlittle: -D_LITTLE_ENDIAN -Amachine(littleendian)} \
%{mlittle-endian: -D_LITTLE_ENDIAN -Amachine(littleendian)} \
%{!mlittle: %{!mlittle-endian: -D_BIG_ENDIAN -Amachine(bigendian)}}"

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE} %(cpp_sysv) %(cpp_endian) %(cpp_cpu)"

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mbig", "mbig-endian", "mcall-sysv" }
