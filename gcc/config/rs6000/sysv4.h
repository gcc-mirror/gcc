/* Target definitions for GNU compiler for PowerPC running System V.4
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

/* Small data support types */
enum rs6000_sdata_type {
  SDATA_NONE,			/* no small data support */
  SDATA_DATA,			/* just put data in .sbss/.sdata, don't use relocs */
  SDATA_SYSV,			/* Use r13 to point to .sdata/.sbss */
  SDATA_EABI			/* Use r13 like above, r2 points to .sdata2/.sbss2 */
};

extern enum rs6000_sdata_type rs6000_sdata;

/* V.4/eabi switches */
#define	MASK_NO_BITFIELD_TYPE	0x40000000	/* Set PCC_BITFIELD_TYPE_MATTERS to 0 */
#define	MASK_STRICT_ALIGN	0x20000000	/* Set STRICT_ALIGNMENT to 1.  */
#define MASK_RELOCATABLE	0x10000000	/* GOT pointers are PC relative */
#define	MASK_EABI		0x08000000	/* Adhere to eabi, not System V spec */
#define MASK_LITTLE_ENDIAN	0x04000000	/* target is little endian */
#define MASK_REGNAMES		0x02000000	/* use alternate register names.  */
#define MASK_PROTOTYPE		0x01000000	/* Only prototyped fcns pass variable args */

#define	TARGET_NO_BITFIELD_TYPE	(target_flags & MASK_NO_BITFIELD_TYPE)
#define TARGET_STRICT_ALIGN	(target_flags & MASK_STRICT_ALIGN)
#define TARGET_RELOCATABLE	(target_flags & MASK_RELOCATABLE)
#define TARGET_EABI		(target_flags & MASK_EABI)
#define TARGET_LITTLE_ENDIAN	(target_flags & MASK_LITTLE_ENDIAN)
#define TARGET_REGNAMES		(target_flags & MASK_REGNAMES)
#define	TARGET_PROTOTYPE	(target_flags & MASK_PROTOTYPE)
#define	TARGET_TOC		((target_flags & MASK_64BIT)		\
				 || ((target_flags & (MASK_RELOCATABLE	\
						      | MASK_MINIMAL_TOC)) \
				     && flag_pic > 1)			\
				 || DEFAULT_ABI == ABI_AIX		\
				 || DEFAULT_ABI == ABI_NT)

#define	TARGET_BITFIELD_TYPE	(! TARGET_NO_BITFIELD_TYPE)
#define TARGET_BIG_ENDIAN	(! TARGET_LITTLE_ENDIAN)
#define	TARGET_NO_PROTOTYPE	(! TARGET_PROTOTYPE)
#define	TARGET_NO_TOC		(! TARGET_TOC)
#define TARGET_NO_EABI		(! TARGET_EABI)

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
  { "prototype",	 MASK_PROTOTYPE },				\
  { "no-prototype",	-MASK_PROTOTYPE },				\
  { "no-traceback",	 0 },						\
  { "eabi",		 MASK_EABI },					\
  { "no-eabi",		-MASK_EABI },					\
  { "regnames",		  MASK_REGNAMES },				\
  { "no-regnames",	 -MASK_REGNAMES },				\
  { "sdata",		 0 },						\
  { "no-sdata",		 0 },						\
  { "sim",		 0 },						\
  { "ads",		 0 },						\
  { "yellowknife",	 0 },						\
  { "mvme",		 0 },						\
  { "emb",		 0 },						\
  { "solaris-cclib",	 0 },						\
  { "shlib",		 0 },						\
  EXTRA_SUBTARGET_SWITCHES                                              \
  { "newlib",		 0 },

/* This is meant to be redefined in the host dependent files */
#define EXTRA_SUBTARGET_SWITCHES

/* Default ABI to use */
#define RS6000_ABI_NAME "sysv"

/* Strings provided by SUBTARGET_OPTIONS */
extern const char *rs6000_abi_name;
extern const char *rs6000_sdata_name;

#define SUBTARGET_OPTIONS						\
  { "call-",  &rs6000_abi_name},					\
  { "sdata=", &rs6000_sdata_name}

/* Max # of bytes for variables to automatically be put into the .sdata
   or .sdata2 sections.  */
extern int g_switch_value;		/* value of the -G xx switch */
extern int g_switch_set;		/* whether -G xx was passed.  */

#ifndef SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE 8
#endif

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   The macro SUBTARGET_OVERRIDE_OPTIONS is provided for subtargets, to
   get control.  */

#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (!g_switch_set)							\
    g_switch_value = SDATA_DEFAULT_SIZE;				\
									\
  if (!strcmp (rs6000_abi_name, "sysv"))				\
    rs6000_current_abi = ABI_V4;					\
  else if (!strcmp (rs6000_abi_name, "sysv-noeabi"))			\
    {									\
      rs6000_current_abi = ABI_V4;					\
      target_flags &= ~ MASK_EABI;					\
    }									\
  else if (!strcmp (rs6000_abi_name, "sysv-eabi")			\
	   || !strcmp (rs6000_abi_name, "eabi"))			\
    {									\
      rs6000_current_abi = ABI_V4;					\
      target_flags |= MASK_EABI;					\
    }									\
  else if (!strcmp (rs6000_abi_name, "aix"))				\
    {									\
      rs6000_current_abi = ABI_AIX_NODESC;				\
      target_flags |= MASK_EABI;					\
    }									\
  else if (!strcmp (rs6000_abi_name, "aixdesc"))			\
    rs6000_current_abi = ABI_AIX;					\
  else if (!strcmp (rs6000_abi_name, "nt"))				\
    rs6000_current_abi = ABI_NT;					\
  else if (!strcmp (rs6000_abi_name, "linux"))				\
    rs6000_current_abi = ABI_V4;					\
  else if (!strcmp (rs6000_abi_name, "solaris"))			\
    rs6000_current_abi = ABI_SOLARIS;					\
  else									\
    {									\
      rs6000_current_abi = ABI_V4;					\
      error ("Bad value for -mcall-%s", rs6000_abi_name);		\
    }									\
									\
  if (rs6000_sdata_name)						\
    {									\
      if (!strcmp (rs6000_sdata_name, "none"))				\
	rs6000_sdata = SDATA_NONE;					\
      else if (!strcmp (rs6000_sdata_name, "data"))			\
	rs6000_sdata = SDATA_DATA;					\
      else if (!strcmp (rs6000_sdata_name, "default"))			\
	rs6000_sdata = (TARGET_EABI) ? SDATA_EABI : SDATA_SYSV;		\
      else if (!strcmp (rs6000_sdata_name, "sysv"))			\
	rs6000_sdata = SDATA_SYSV;					\
      else if (!strcmp (rs6000_sdata_name, "eabi"))			\
	rs6000_sdata = SDATA_EABI;					\
      else								\
	error ("Bad value for -msdata=%s", rs6000_sdata_name);		\
    }									\
  else if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)		\
    {									\
      rs6000_sdata = SDATA_DATA;					\
      rs6000_sdata_name = "data";					\
    }									\
  else									\
    {									\
      rs6000_sdata = SDATA_NONE;					\
      rs6000_sdata_name = "none";					\
    }									\
									\
  if (TARGET_RELOCATABLE &&						\
      (rs6000_sdata == SDATA_EABI || rs6000_sdata == SDATA_SYSV))	\
    {									\
      rs6000_sdata = SDATA_DATA;					\
      error ("-mrelocatable and -msdata=%s are incompatible.",		\
	     rs6000_sdata_name);					\
    }									\
									\
  else if (flag_pic &&							\
	   (rs6000_sdata == SDATA_EABI || rs6000_sdata == SDATA_SYSV))	\
    {									\
      rs6000_sdata = SDATA_DATA;					\
      error ("-f%s and -msdata=%s are incompatible.",			\
	     (flag_pic > 1) ? "PIC" : "pic",				\
	     rs6000_sdata_name);					\
    }									\
									\
  if (rs6000_sdata != SDATA_NONE && DEFAULT_ABI != ABI_V4		\
      && DEFAULT_ABI != ABI_SOLARIS)					\
    {									\
      rs6000_sdata = SDATA_NONE;					\
      error ("-msdata=%s and -mcall-%s are incompatible.",		\
	     rs6000_sdata_name, rs6000_abi_name);			\
    }									\
									\
  if (TARGET_RELOCATABLE && !TARGET_MINIMAL_TOC)			\
    {									\
      target_flags |= MASK_MINIMAL_TOC;					\
      error ("-mrelocatable and -mno-minimal-toc are incompatible.");	\
    }									\
									\
  if (TARGET_RELOCATABLE &&						\
      (rs6000_current_abi == ABI_AIX || rs6000_current_abi == ABI_NT))	\
    {									\
      target_flags &= ~MASK_RELOCATABLE;				\
      error ("-mrelocatable and -mcall-%s are incompatible.",		\
	     rs6000_abi_name);						\
    }									\
									\
  if (flag_pic > 1 &&							\
      (rs6000_current_abi == ABI_AIX || rs6000_current_abi == ABI_NT))	\
    {									\
      flag_pic = 0;							\
      error ("-fPIC and -mcall-%s are incompatible.",			\
	     rs6000_abi_name);						\
    }									\
									\
  if (rs6000_current_abi == ABI_AIX && TARGET_LITTLE_ENDIAN)		\
    {									\
      target_flags &= ~MASK_LITTLE_ENDIAN;				\
      error ("-mcall-aixdesc must be big endian");			\
    }									\
									\
  if (rs6000_current_abi == ABI_NT && TARGET_BIG_ENDIAN)		\
    {									\
      target_flags |= MASK_LITTLE_ENDIAN;				\
      error ("-mcall-nt must be little endian");			\
    }									\
									\
  /* Treat -fPIC the same as -mrelocatable */				\
  if (flag_pic > 1)							\
    target_flags |= MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC; \
									\
  else if (TARGET_RELOCATABLE)						\
    flag_pic = 2;							\
									\
} while (0)

/* Default ABI to compile code for */
#define DEFAULT_ABI rs6000_current_abi

#define CPP_DEFAULT_SPEC "-D_ARCH_PPC"

#define ASM_DEFAULT_SPEC "-mppc"

#include "rs6000/rs6000.h"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC601

/* System V.4 uses register 13 as a pointer to the small data area,
   so it is not available to the normal user.  */

#undef	FIXED_R13
#define FIXED_R13 1

/* System V.4 passes the first 8 floating arguments in registers,
   instead of the first 13 like AIX does.  */
#undef	FP_ARG_MAX_REG
#define	FP_ARG_MAX_REG ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_AIX_NODESC) \
			? FP_ARG_AIX_MAX_REG : FP_ARG_V4_MAX_REG)

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
#if !defined(_LITTLE_ENDIAN) && !defined(__sun__)
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Size of the outgoing register save area */
#undef	RS6000_REG_SAVE
#define RS6000_REG_SAVE ((DEFAULT_ABI == ABI_AIX			\
			  || DEFAULT_ABI == ABI_AIX_NODESC)		\
			 ? (TARGET_64BIT ? 64 : 32)			\
			 : 0)

/* Size of the fixed area on the stack.  For AIX, use the standard 6 word
   area, otherwise use 2 words to store back chain & LR.  */
#undef	RS6000_SAVE_AREA
#define RS6000_SAVE_AREA \
  (((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_AIX_NODESC) ? 24 : 8) << (TARGET_64BIT ? 1 : 0))

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

/* Put jump tables in read-only memory, rather than in .text.  */
#undef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 0

/* Disable AIX-ism that disables turning -B into -L if the argument specifies a
   relative file name.  This breaks setting GCC_EXEC_PREFIX to D:\path under
   Windows.  */
#undef RELATIVE_PREFIX_NOT_LINKDIR

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
#define WCHAR_TYPE "long int"

/* Width of wchar_t in bits.  */
#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Make int foo : 8 not cause structures to be aligned to an int boundary */

#undef	PCC_BITFIELD_TYPE_MATTERS
#define	PCC_BITFIELD_TYPE_MATTERS (TARGET_BITFIELD_TYPE)

/* Define this macro to be the value 1 if instructions will fail to
   work if given data not on the nominal alignment.  If instructions
   will merely go slower in that case, define this macro as 0.  */
#undef	STRICT_ALIGNMENT
#define	STRICT_ALIGNMENT (TARGET_STRICT_ALIGN)

/* Alignment in bits of the stack boundary.  Note, in order to allow building
   one set of libraries with -mno-eabi instead of eabi libraries and non-eabi
   versions, just use 64 as the stack boundary.  */
#undef	STACK_BOUNDARY
#define	STACK_BOUNDARY	64

/* Real stack boundary as mandated by the appropriate ABI */
#define ABI_STACK_BOUNDARY ((TARGET_EABI) ? 64 : 128)

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT ((TARGET_EABI) ? 64 : 128)

#undef  BIGGEST_FIELD_ALIGNMENT
#undef  ADJUST_FIELD_ALIGN
#undef  ROUND_TYPE_ALIGN

/* Use ELF style section commands.  */

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP	"\t.section\t\".text\""

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP	"\t.section\t\".data\""

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP "\t.section\t\".init\",\"ax\""

#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP "\t.section\t\".fini\",\"ax\""

#define TOC_SECTION_ASM_OP "\t.section\t\".got\",\"aw\""

/* Put PC relative got entries in .got2 */
#define MINIMAL_TOC_SECTION_ASM_OP \
  ((TARGET_RELOCATABLE || flag_pic) ? "\t.section\t\".got2\",\"aw\"" : "\t.section\t\".got1\",\"aw\"")

/* Put relocatable data in .data, not .rodata so initialized pointers can be updated */
#undef	CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP \
  ((TARGET_RELOCATABLE || flag_pic) ? "\t.section\t\".data\"\t# .rodata" : "\t.section\t\".rodata\"")


#define SDATA_SECTION_ASM_OP "\t.section\t\".sdata\",\"aw\""
#define SDATA2_SECTION_ASM_OP "\t.section\t\".sdata2\",\"a\""
#define SBSS_SECTION_ASM_OP \
  ((DEFAULT_ABI == ABI_SOLARIS) ? "\t.section\t\".sbss\",\"aw\"" : "\t.section\t\".sbss\",\"aw\",@nobits")


/* Besides the usual ELF sections, we need a toc section.  */
#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_toc, in_sdata, in_sdata2, in_sbss, in_init, in_fini

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  TOC_SECTION_FUNCTION							\
  SDATA_SECTION_FUNCTION						\
  SDATA2_SECTION_FUNCTION						\
  SBSS_SECTION_FUNCTION							\
  INIT_SECTION_FUNCTION							\
  FINI_SECTION_FUNCTION

extern void toc_section (), sdata_section (), sdata2_section ();
extern void sbss_section ();

#define TOC_SECTION_FUNCTION						\
void									\
toc_section ()								\
{									\
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

#define SDATA_SECTION_FUNCTION						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      in_section = in_sdata;						\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
    }									\
}

#define SDATA2_SECTION_FUNCTION						\
void									\
sdata2_section ()							\
{									\
  if (in_section != in_sdata2)						\
    {									\
      in_section = in_sdata2;						\
      fprintf (asm_out_file, "%s\n", SDATA2_SECTION_ASM_OP);		\
    }									\
}

#define SBSS_SECTION_FUNCTION						\
void									\
sbss_section ()								\
{									\
  if (in_section != in_sbss)						\
    {									\
      in_section = in_sbss;						\
      fprintf (asm_out_file, "%s\n", SBSS_SECTION_ASM_OP);		\
    }									\
}

#define INIT_SECTION_FUNCTION						\
void									\
init_section ()								\
{									\
  if (in_section != in_init)						\
    {									\
      in_section = in_init;						\
      fprintf (asm_out_file, "%s\n", INIT_SECTION_ASM_OP);		\
    }									\
}

#define FINI_SECTION_FUNCTION						\
void									\
fini_section ()								\
{									\
  if (in_section != in_fini)						\
    {									\
      in_section = in_fini;						\
      fprintf (asm_out_file, "%s\n", FINI_SECTION_ASM_OP);		\
    }									\
}

/* A C statement or statements to switch to the appropriate section
   for output of RTX in mode MODE.  You can assume that RTX is some
   kind of constant in RTL.  The argument MODE is redundant except in
   the case of a `const_int' rtx.  Select the section by calling
   `text_section' or one of the alternatives for other sections.

   Do not define this macro if you put all constants in the read-only
   data section.  */

extern void rs6000_select_rtx_section (), rs6000_select_section ();

#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, X) rs6000_select_rtx_section (MODE, X)

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

#undef SELECT_SECTION
#define SELECT_SECTION(DECL,RELOC) rs6000_select_section (DECL, RELOC)

/* Return non-zero if this entry is to be written into the constant pool
   in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF or a CONST
   containing one of them.  If -mfp-in-toc (the default), we also do
   this for floating-point constants.  We actually can only do this
   if the FP formats of the target and host machines are the same, but
   we can't check that since not every file that uses
   GO_IF_LEGITIMATE_ADDRESS_P includes real.h.

   Unlike AIX, we don't key off of -mminimal-toc, but instead do not
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
    if (TARGET_RELOCATABLE && (get_pool_size () != 0 || profile_flag))	\
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

/* This is how to allocate empty space in some section.  Use .space
   instead of .zero because the Solaris PowerPC assembler doesn't
   like it, and gas accepts either syntax.  */

#undef	SKIP_ASM_OP
#define SKIP_ASM_OP	".space"

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#ifndef LOCAL_ASM_OP
#define LOCAL_ASM_OP	".local"
#endif

#ifndef LCOMM_ASM_OP
#define LCOMM_ASM_OP	".lcomm"
#endif

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  if (rs6000_sdata != SDATA_NONE && (SIZE) > 0				\
      && (SIZE) <= g_switch_value)					\
    {									\
      sbss_section ();							\
      ASM_OUTPUT_ALIGN (FILE, exact_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
      ASM_OUTPUT_SKIP (FILE, SIZE);					\
      if (!flag_inhibit_size_directive && (SIZE) > 0)			\
	{								\
	  fprintf (FILE, "\t%s\t ", SIZE_ASM_OP);			\
	  assemble_name (FILE, NAME);					\
	  fprintf (FILE, ",%d\n",  SIZE);				\
	}								\
    }									\
  else									\
    {									\
      fprintf (FILE, "\t%s\t", LCOMM_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
    }									\
} while (0)

/* Describe how to emit uninitialized external linkage items  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
do {									\
  ASM_GLOBALIZE_LABEL (FILE, NAME);					\
  ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);			\
} while (0)

/* Switch  Recognition by gcc.c.  Add -G xx support */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)						\
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o'			\
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u'			\
   || (CHAR) == 'I' || (CHAR) == 'm' || (CHAR) == 'x'			\
   || (CHAR) == 'L' || (CHAR) == 'A' || (CHAR) == 'V'			\
   || (CHAR) == 'B' || (CHAR) == 'b' || (CHAR) == 'G')

/* Output .file.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
do {									\
  output_file_directive ((FILE), main_input_filename);			\
  rs6000_file_start (FILE, TARGET_CPU_DEFAULT);				\
} while (0)


/* This is how to output an assembler line defining an `int' constant.
   For -mrelocatable, we mark all addresses that need to be fixed up
   in the .fixup section.  */
#undef	ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(FILE,VALUE)					\
do {									\
  static int recurse = 0;						\
  if ((TARGET_RELOCATABLE || flag_pic)					\
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
      fprintf (FILE, "\t.section\t\".fixup\",\"aw\"\n");			\
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

/* Allow stabs and dwarf, for now, make stabs the default debugging type,
   not dwarf since G++ doesn't support dwarf. */
#undef	PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

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
#define ENCODE_SECTION_INFO(DECL) rs6000_encode_section_info (DECL)

extern void rs6000_encode_section_info ();

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  Discard
   a leading * or @. */
#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME)				\
do {									\
  char *_name = SYMBOL_NAME;						\
  while (*_name == '*' || *_name == '@')				\
    _name++;								\
  (VAR) = _name;							\
} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
do {									\
  char *_name = NAME;							\
  while (*_name == '*' || *_name == '@')				\
    _name++;								\
  fputs (_name, FILE);							\
} while (0)

/*
 * Switch into a generic section.
 *
 * We make the section read-only and executable for a function decl,
 * read-only for a const data decl, and writable for a non-const data decl.
 *
 * If the section has already been defined, we must not
 * emit the attributes here. The SVR4 assembler does not
 * recognize section redefinitions.
 * If DECL is NULL, no attributes are emitted.
 *
 * Note, Solaris as doesn't like @nobits, and gas can handle .sbss without
 * needing @nobits.
 */

#undef	ASM_OUTPUT_SECTION_NAME
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC)		\
do {									\
  static struct section_info						\
    {									\
      struct section_info *next;				        \
      char *name;						        \
      enum sect_enum {SECT_RW, SECT_RO, SECT_EXEC} type;		\
    } *sections;							\
  struct section_info *s;						\
  char *mode;								\
  enum sect_enum type;							\
									\
  for (s = sections; s; s = s->next)					\
    if (!strcmp (NAME, s->name))					\
      break;								\
									\
  if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)			\
    type = SECT_EXEC, mode = "ax";					\
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC) && !TARGET_RELOCATABLE && !flag_pic) \
    type = SECT_RO, mode = "a";						\
  else									\
    type = SECT_RW, mode = "aw";					\
									\
  if (s == 0)								\
    {									\
      s = (struct section_info *) xmalloc (sizeof (struct section_info));  \
      s->name = xmalloc ((strlen (NAME) + 1) * sizeof (*NAME));		\
      strcpy (s->name, NAME);						\
      s->type = type;							\
      s->next = sections;						\
      sections = s;							\
      fprintf (FILE, "\t.section\t\"%s\",\"%s\"\n", NAME, mode);	\
    }									\
  else									\
    {									\
      if (DECL && s->type != type)					\
	error_with_decl (DECL, "%s causes a section type conflict");	\
									\
      fprintf (FILE, "\t.section\t\"%s\"\n", NAME);			\
    }									\
} while (0)

#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    if (DEFAULT_ABI != ABI_SOLARIS)					\
      {									\
	ctors_section ();						\
	fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
	assemble_name (FILE, NAME);					\
      }									\
    else								\
      {									\
	init_section ();						\
	fputs ("\tbl ", FILE);						\
	assemble_name (FILE, NAME);					\
      }									\
    fputs ("\n", FILE);							\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    if (DEFAULT_ABI != ABI_SOLARIS)					\
      {									\
	dtors_section ();						\
	fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
	assemble_name (FILE, NAME);					\
      }									\
    else								\
      {									\
	fini_section ();						\
	fputs ("\tbl ", FILE);						\
	assemble_name (FILE, NAME);					\
      }									\
    fputs ("\n", FILE);							\
  } while (0)

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */

#define	DBX_FUNCTION_FIRST

/* This is the end of what might become sysv4dbx.h.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC System V.4)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-DPPC -Dunix -D__svr4__ -Asystem(unix) -Asystem(svr4) -Acpu(powerpc) -Amachine(powerpc)"

/* Pass various options to the assembler */
#undef ASM_SPEC
#define ASM_SPEC "%(asm_cpu) \
%{.s: %{mregnames} %{mno-regnames}} %{.S: %{mregnames} %{mno-regnames}} \
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} \
%{mrelocatable} %{mrelocatable-lib} %{fpic:-K PIC} %{fPIC:-K PIC} \
%{memb} %{!memb: %{msdata: -memb} %{msdata=eabi: -memb}} \
%{mlittle} %{mlittle-endian} %{mbig} %{mbig-endian} \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-solaris: -mlittle -msolaris} \
    %{mcall-linux: -mbig} }}}}"

#ifndef CC1_ENDIAN_BIG_SPEC
#define CC1_ENDIAN_BIG_SPEC ""
#endif

#ifndef CC1_ENDIAN_LITTLE_SPEC
#define CC1_ENDIAN_LITTLE_SPEC "\
%{!mstrict-align: %{!mno-strict-align: \
	-mstrict-align \
}}"
#endif

#ifndef CC1_ENDIAN_DEFAULT_SPEC
#define CC1_ENDIAN_DEFAULT_SPEC "%(cc1_endian_big_spec)"
#endif

#undef CC1_SPEC
/* Pass -G xxx to the compiler and set correct endian mode */
#define CC1_SPEC "%{G*} \
%{mlittle: %(cc1_endian_little)} %{!mlittle: %{mlittle-endian: %(cc1_endian_little)}} \
%{mbig: %(cc1_endian_big)} %{!mbig: %{mbig-endian: %(cc1_endian_big)}} \
    %{mcall-nt: -mlittle %{cc1_endian_little} } \
    %{mcall-aixdesc: -mbig %{cc1_endian_big} } \
    %{mcall-solaris: -mlittle %{cc1_endian_little} } \
    %{mcall-linux: -mbig %{cc1_endian_big} } \
    %{!mcall-nt: %{!mcall-aixdesc: %{!mcall-solaris: %{!mcall-linux: \
	    %(cc1_endian_default) \
    }}}} \
%{mcall-solaris: -mregnames } \
%{mno-sdata: -msdata=none } \
%{meabi: %{!mcall-*: -mcall-sysv }} \
%{!meabi: %{!mno-eabi: \
    %{mrelocatable: -meabi } \
    %{mcall-solaris: -mno-eabi } \
    %{mcall-linux: -mno-eabi }}} \
%{msdata: -msdata=default} \
%{mno-sdata: -msdata=none} \
%{profile: -p}"

/* Don't put -Y P,<path> for cross compilers */
#undef LINK_PATH_SPEC
#ifndef CROSS_COMPILE
#define LINK_PATH_SPEC "\
%{!R*:%{L*:-R %*}} \
%{!nostdlib: %{!YP,*: \
    %{compat-bsd: \
	%{p:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
	%{!p:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}} \
	%{!R*: %{!L*: -R /usr/ucblib}} \
    %{!compat-bsd: \
	%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
	%{!p:-Y P,/usr/ccs/lib:/usr/lib}}}}"

#else
#define LINK_PATH_SPEC ""
#endif

/* Default starting address if specified */
#ifndef LINK_START_SPEC
#define LINK_START_SPEC "\
%{mads: %(link_start_ads) } \
%{myellowknife: %(link_start_yellowknife) } \
%{mmvme: %(link_start_mvme) } \
%{msim: %(link_start_sim) } \
%{mcall-linux: %(link_start_linux) } \
%{mcall-solaris: %(link_start_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(link_start_default) }}}}}}"
#endif

#ifndef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC ""
#endif

#undef LINK_SPEC
#define LINK_SPEC "\
%{h*} %{v:-V} %{G*} \
%{Wl,*:%*} %{YP,*} %{R*} \
%{Qy:} %{!Qn:-Qy} \
%(link_shlib) \
%{!Ttext*: %(link_start) } \
%(link_target) \
%(link_os)"

/* For now, turn off shared libraries by default.  */
#ifndef SHARED_LIB_SUPPORT
#define NO_SHARED_LIB_SUPPORT
#endif

#undef  LINK_SHLIB_SPEC
#ifndef NO_SHARED_LIB_SUPPORT
/* Shared libraries are default.  */
#define LINK_SHLIB_SPEC "\
%{!static: %(link_path) %{!R*:%{L*:-R %*}}} \
%{mshlib: } \
%{static:-dn -Bstatic} \
%{shared:-G -dy -z text} \
%{symbolic:-Bsymbolic -G -dy -z text}"

#else
/* Shared libraries are not default.  */
#define LINK_SHLIB_SPEC "\
%{mshlib: %(link_path) } \
%{!mshlib: %{!shared: %{!symbolic: -dn -Bstatic}}} \
%{static: } \
%{shared:-G -dy -z text %(link_path) } \
%{symbolic:-Bsymbolic -G -dy -z text %(link_path) }"
#endif

/* Override the default target of the linker.  */
#undef	LINK_TARGET_SPEC
#define	LINK_TARGET_SPEC "\
%{mlittle: -oformat elf32-powerpcle } %{mlittle-endian: -oformat elf32-powerpcle } \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: %{mcall-solaris: -oformat elf32-powerpcle}}}}}"

/* Any specific OS flags */
#ifndef LINK_OS_SPEC
#define LINK_OS_SPEC "\
%{mads: %(link_os_ads) } \
%{myellowknife: %(link_os_yellowknife) } \
%{mmvme: %(link_os_mvme) } \
%{msim: %(link_os_sim) } \
%{mcall-linux: %(link_os_linux) } \
%{mcall-solaris: %(link_os_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(link_os_default) }}}}}}"
#endif

#ifndef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC ""
#endif

#undef	CPP_SYSV_SPEC
#define CPP_SYSV_SPEC \
"%{mrelocatable*: -D_RELOCATABLE} \
%{fpic: -D__PIC__=1 -D__pic__=1} \
%{!fpic: %{fPIC: -D__PIC__=2 -D__pic__=2}} \
%{mcall-sysv: -D_CALL_SYSV} %{mcall-nt: -D_CALL_NT} \
%{mcall-aix: -D_CALL_AIX} %{mcall-aixdesc: -D_CALL_AIX -D_CALL_AIXDESC} \
%{!mcall-sysv: %{!mcall-aix: %{!mcall-aixdesc: %{!mcall-nt: %(cpp_sysv_default) }}}} \
%{msoft-float: -D_SOFT_FLOAT} %{mcpu=403: -D_SOFT_FLOAT}"

#undef	CPP_SYSV_DEFAULT_SPEC
#define	CPP_SYSV_DEFAULT_SPEC "-D_CALL_SYSV"

#ifndef CPP_ENDIAN_BIG_SPEC
#define CPP_ENDIAN_BIG_SPEC "-D_BIG_ENDIAN -D__BIG_ENDIAN__ -Amachine(bigendian)"
#endif

#ifndef CPP_ENDIAN_LITTLE_SPEC
#define CPP_ENDIAN_LITTLE_SPEC "-D_LITTLE_ENDIAN -D__LITTLE_ENDIAN__ -Amachine(littleendian)"
#endif

#ifndef CPP_ENDIAN_SOLARIS_SPEC
#define CPP_ENDIAN_SOLARIS_SPEC "-D__LITTLE_ENDIAN__ -Amachine(littleendian)"
#endif

/* For solaris, don't define _LITTLE_ENDIAN, it conflicts with a header file.  */
#undef	CPP_ENDIAN_SPEC
#define	CPP_ENDIAN_SPEC \
"%{mlittle: %(cpp_endian_little) } \
%{mlittle-endian: %(cpp_endian_little) } \
%{mbig: %(cpp_endian_big) } \
%{mbig-endian: %(cpp_endian_big) } \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-solaris: %(cpp_endian_solaris) } \
    %{mcall-nt: %(cpp_endian_little) } \
    %{mcall-linux: %(cpp_endian_big) } \
    %{mcall-aixdesc:  %(cpp_endian_big) } \
    %{!mcall-solaris: %{!mcall-linux: %{!mcall-nt: %{!mcall-aixdesc: %(cpp_endian_default) }}}}}}}}"

#undef	CPP_ENDIAN_DEFAULT_SPEC
#define	CPP_ENDIAN_DEFAULT_SPEC "%(cpp_endian_big)"

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE} %(cpp_sysv) %(cpp_endian) %(cpp_cpu) \
%{mads: %(cpp_os_ads) } \
%{myellowknife: %(cpp_os_yellowknife) } \
%{mmvme: %(cpp_os_mvme) } \
%{msim: %(cpp_os_sim) } \
%{mcall-linux: %(cpp_os_linux) } \
%{mcall-solaris: %(cpp_os_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(cpp_os_default) }}}}}}"

#ifndef CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC ""
#endif

#undef  STARTFILE_SPEC
#define	STARTFILE_SPEC "\
%{mads: %(startfile_ads) } \
%{myellowknife: %(startfile_yellowknife) } \
%{mmvme: %(startfile_mvme) } \
%{msim: %(startfile_sim) } \
%{mcall-linux: %(startfile_linux) } \
%{mcall-solaris: %(startfile_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(startfile_default) }}}}}}"

#undef	STARTFILE_DEFAULT_SPEC
#define	STARTFILE_DEFAULT_SPEC ""

#undef	LIB_SPEC
#define	LIB_SPEC "\
%{mads: %(lib_ads) } \
%{myellowknife: %(lib_yellowknife) } \
%{mmvme: %(lib_mvme) } \
%{msim: %(lib_sim) } \
%{mcall-linux: %(lib_linux) } \
%{mcall-solaris: %(lib_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(lib_default) }}}}}}"

#undef	LIBGCC_SPEC
#define	LIBGCC_SPEC "libgcc.a%s"

#ifndef LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC ""
#endif

#undef	ENDFILE_SPEC
#define	ENDFILE_SPEC "\
%{mads: ecrtn.o%s} \
%{myellowknife: ecrtn.o%s} \
%{mmvme: ecrtn.o%s} \
%{msim: ecrtn.o%s} \
%{mcall-linux: %(endfile_linux) } \
%{mcall-solaris: scrtn.o%s} \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(endfile_default) }}}}}}"

#undef	ENDFILE_DEFAULT_SPEC
#define	ENDFILE_DEFAULT_SPEC ""

/* Motorola ADS support.  */
#ifndef	LIB_ADS_SPEC
#define LIB_ADS_SPEC "--start-group -lads -lc --end-group"
#endif

#ifndef	STARTFILE_ADS_SPEC
#define	STARTFILE_ADS_SPEC "ecrti.o%s crt0.o%s"
#endif

#ifndef	ENDFILE_ADS_SPEC
#define	ENDFILE_ADS_SPEC "ecrtn.o%s"
#endif

#ifndef LINK_START_ADS_SPEC
#define LINK_START_ADS_SPEC "-T ads.ld%s"
#endif

#ifndef LINK_OS_ADS_SPEC
#define LINK_OS_ADS_SPEC ""
#endif

#ifndef CPP_OS_ADS_SPEC
#define CPP_OS_ADS_SPEC ""
#endif

/* Motorola Yellowknife support.  */
#ifndef	LIB_YELLOWKNIFE_SPEC
#define LIB_YELLOWKNIFE_SPEC "--start-group -lyk -lc --end-group"
#endif

#ifndef	STARTFILE_YELLOWKNIFE_SPEC
#define	STARTFILE_YELLOWKNIFE_SPEC "ecrti.o%s crt0.o%s"
#endif

#ifndef	ENDFILE_YELLOWKNIFE_SPEC
#define	ENDFILE_YELLOWKNIFE_SPEC "ecrtn.o%s"
#endif

#ifndef LINK_START_YELLOWKNIFE_SPEC
#define LINK_START_YELLOWKNIFE_SPEC "-T yellowknife.ld%s"
#endif

#ifndef LINK_OS_YELLOWKNIFE_SPEC
#define LINK_OS_YELLOWKNIFE_SPEC ""
#endif

#ifndef CPP_OS_YELLOWKNIFE_SPEC
#define CPP_OS_YELLOWKNIFE_SPEC ""
#endif

/* Motorola MVME support.  */
#ifndef	LIB_MVME_SPEC
#define LIB_MVME_SPEC "--start-group -lmvme -lc --end-group"
#endif

#ifndef	STARTFILE_MVME_SPEC
#define	STARTFILE_MVME_SPEC "ecrti.o%s crt0.o%s"
#endif

#ifndef	ENDFILE_MVME_SPEC
#define	ENDFILE_MVME_SPEC "ecrtn.o%s"
#endif

#ifndef LINK_START_MVME_SPEC
#define LINK_START_MVME_SPEC "%{!Wl,-T*: %{!T*: -Ttext 0x40000}}"
#endif

#ifndef LINK_OS_MVME_SPEC
#define LINK_OS_MVME_SPEC ""
#endif

#ifndef CPP_OS_MVME_SPEC
#define CPP_OS_MVME_SPEC ""
#endif

/* PowerPC simulator based on netbsd system calls support.  */
#ifndef	LIB_SIM_SPEC
#define LIB_SIM_SPEC "--start-group -lsim -lc --end-group"
#endif

#ifndef	STARTFILE_SIM_SPEC
#define	STARTFILE_SIM_SPEC "ecrti.o%s sim-crt0.o%s"
#endif

#ifndef	ENDFILE_SIM_SPEC
#define	ENDFILE_SIM_SPEC "ecrtn.o%s"
#endif

#ifndef LINK_START_SIM_SPEC
#define LINK_START_SIM_SPEC "-Ttext 0x10000074"
#endif

#ifndef LINK_OS_SIM_SPEC
#define LINK_OS_SIM_SPEC ""
#endif

#ifndef CPP_OS_SIM_SPEC
#define CPP_OS_SIM_SPEC ""
#endif

/* GNU/Linux support.  */
#ifndef	LIB_LINUX_SPEC
#ifdef USE_GNULIBC_1
#define LIB_LINUX_SPEC "%{mnewlib: --start-group -llinux -lc --end-group } \
%{!mnewlib: -lc }"
#else
#define LIB_LINUX_SPEC "%{mnewlib: --start-group -llinux -lc --end-group } \
%{!mnewlib: %{shared:-lc} %{!shared: %{pthread:-lpthread } \
%{profile:-lc_p} %{!profile:-lc}}}"
#endif
#endif

#ifndef	STARTFILE_LINUX_SPEC
#define	STARTFILE_LINUX_SPEC "\
%{!shared: %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}} \
%{mnewlib: ecrti.o%s} \
%{!mnewlib: crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}}"
#endif

#ifndef	ENDFILE_LINUX_SPEC
#define	ENDFILE_LINUX_SPEC "\
%{mnewlib: ecrtn.o%s} \
%{!mnewlib: %{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s}"
#endif

#ifndef LINK_START_LINUX_SPEC
#define LINK_START_LINUX_SPEC "-Ttext 0x400074"
#endif

#ifndef LINK_OS_LINUX_SPEC
#define LINK_OS_LINUX_SPEC ""
#endif

#ifndef CPP_OS_LINUX_SPEC
#ifdef USE_GNULIBC_1
#define CPP_OS_LINUX_SPEC "-D__unix__ -D__linux__		\
%{!undef:							\
  %{!ansi:							\
    %{!std=*:-Dunix -D__unix -Dlinux -D__linux}			\
    %{std=gnu*:-Dunix -D__unix -Dlinux -D__linux}}}		\
-Asystem(unix) -Asystem(posix)"
#else
#define CPP_OS_LINUX_SPEC "-D__unix__ -D__linux__		\
%{!undef:							\
  %{!ansi:							\
    %{!std=*:-Dunix -D__unix -Dlinux -D__linux}			\
    %{std=gnu*:-Dunix -D__unix -Dlinux -D__linux}}}		\
-Asystem(unix) -Asystem(posix) %{pthread:-D_REENTRANT}"
#endif
#endif

/* Solaris support.  */
/* For Solaris, Gcc automatically adds in one of the files
   /usr/ccs/lib/values-Xc.o, /usr/ccs/lib/values-Xa.o, or
   /usr/ccs/lib/values-Xt.o for each final link step (depending upon the other
   gcc options selected, such as -traditional and -ansi).  These files each
   contain one (initialized) copy of a special variable called `_lib_version'.
   Each one of these files has `_lib_version' initialized to a different (enum)
   value.  The SVR4 library routines query the value of `_lib_version' at run
   to decide how they should behave.  Specifically, they decide (based upon the
   value of `_lib_version') if they will act in a strictly ANSI conforming
   manner or not.  */

#ifndef	LIB_SOLARIS_SPEC
#define LIB_SOLARIS_SPEC "\
%{mnewlib: --start-group -lsolaris -lc --end-group } \
%{!mnewlib: \
    %{ansi:values-Xc.o%s} \
    %{!ansi: \
	%{traditional:values-Xt.o%s} \
	%{!traditional:values-Xa.o%s}} \
	%{compat-bsd:-lucb -lsocket -lnsl -lelf -laio} \
    %{solaris-cclib: /opt/SUNWspro/SC4.0/lib/libabi.a} \
    %{!shared: %{!symbolic: -lc }}}"
#endif

#ifndef	STARTFILE_SOLARIS_SPEC
#define	STARTFILE_SOLARIS_SPEC "\
%{!msolaris-cclib: scrti.o%s scrt0.o%s} \
%{msolaris-cclib: /opt/SUNWspro/SC4.0/lib/crti.o%s /opt/SUNWspro/SC4.0/lib/crt1.o%s}"
#endif

#ifndef	ENDFILE_SOLARIS_SPEC
#define	ENDFILE_SOLARIS_SPEC "\
%{!msolaris-cclib: scrtn.o%s} \
%{msolaris-cclib: /opt/SUNWspro/SC4.0/lib/crtn.o%s}"
#endif

#ifndef LINK_START_SOLARIS_SPEC
#ifdef CROSS_COMPILER
#define LINK_START_SOLARIS_SPEC "-Ttext 0x2000074"
#else
#define LINK_START_SOLARIS_SPEC ""
#endif
#endif

#ifndef LINK_OS_SOLARIS_SPEC
#define LINK_OS_SOLARIS_SPEC ""
#endif

#ifndef CPP_OS_SOLARIS_SPEC
#define CPP_OS_SOLARIS_SPEC "-D__ppc -D__sun__=1 -D__unix__ -D__svr4__  -D__SVR4__ \
%{!undef:%{!ansi:%{!std=*:-Dsun=1 -Dunix -DSVR4 -D__EXTENSIONS__} \
               %{std=gnu*:-Dsun=1 -Dunix -DSVR4 -D__EXTENSIONS__}}} \
-Amachine(prep)"
#endif

/* Define any extra SPECS that the compiler needs to generate.  */
#undef	SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "lib_ads",			LIB_ADS_SPEC },				\
  { "lib_yellowknife",		LIB_YELLOWKNIFE_SPEC },			\
  { "lib_mvme",			LIB_MVME_SPEC },			\
  { "lib_sim",			LIB_SIM_SPEC },				\
  { "lib_linux",		LIB_LINUX_SPEC },			\
  { "lib_solaris",		LIB_SOLARIS_SPEC },			\
  { "lib_default",		LIB_DEFAULT_SPEC },			\
  { "startfile_ads",		STARTFILE_ADS_SPEC },			\
  { "startfile_yellowknife",	STARTFILE_YELLOWKNIFE_SPEC },		\
  { "startfile_mvme",		STARTFILE_MVME_SPEC },			\
  { "startfile_sim",		STARTFILE_SIM_SPEC },			\
  { "startfile_linux",		STARTFILE_LINUX_SPEC },			\
  { "startfile_solaris",	STARTFILE_SOLARIS_SPEC },		\
  { "startfile_default",	STARTFILE_DEFAULT_SPEC },		\
  { "endfile_ads",		ENDFILE_ADS_SPEC },			\
  { "endfile_yellowknife",	ENDFILE_YELLOWKNIFE_SPEC },		\
  { "endfile_mvme",		ENDFILE_MVME_SPEC },			\
  { "endfile_sim",		ENDFILE_SIM_SPEC },			\
  { "endfile_linux",		ENDFILE_LINUX_SPEC },			\
  { "endfile_solaris",		ENDFILE_SOLARIS_SPEC },			\
  { "endfile_default",		ENDFILE_DEFAULT_SPEC },			\
  { "link_path",		LINK_PATH_SPEC },			\
  { "link_shlib",		LINK_SHLIB_SPEC },			\
  { "link_target",		LINK_TARGET_SPEC },			\
  { "link_start",		LINK_START_SPEC },			\
  { "link_start_ads",		LINK_START_ADS_SPEC },			\
  { "link_start_yellowknife",	LINK_START_YELLOWKNIFE_SPEC },		\
  { "link_start_mvme",		LINK_START_MVME_SPEC },			\
  { "link_start_sim",		LINK_START_SIM_SPEC },			\
  { "link_start_linux",		LINK_START_LINUX_SPEC },		\
  { "link_start_solaris",	LINK_START_SOLARIS_SPEC },		\
  { "link_start_default",	LINK_START_DEFAULT_SPEC },		\
  { "link_os",			LINK_OS_SPEC },				\
  { "link_os_ads",		LINK_OS_ADS_SPEC },			\
  { "link_os_yellowknife",	LINK_OS_YELLOWKNIFE_SPEC },		\
  { "link_os_mvme",		LINK_OS_MVME_SPEC },			\
  { "link_os_sim",		LINK_OS_SIM_SPEC },			\
  { "link_os_linux",		LINK_OS_LINUX_SPEC },			\
  { "link_os_solaris",		LINK_OS_SOLARIS_SPEC },			\
  { "link_os_default",		LINK_OS_DEFAULT_SPEC },			\
  { "cc1_endian_big",		CC1_ENDIAN_BIG_SPEC },			\
  { "cc1_endian_little",	CC1_ENDIAN_LITTLE_SPEC },		\
  { "cc1_endian_default",	CC1_ENDIAN_DEFAULT_SPEC },		\
  { "cpp_endian_big",		CPP_ENDIAN_BIG_SPEC },			\
  { "cpp_endian_little",	CPP_ENDIAN_LITTLE_SPEC },		\
  { "cpp_endian_solaris",	CPP_ENDIAN_SOLARIS_SPEC },		\
  { "cpp_os_ads",		CPP_OS_ADS_SPEC },			\
  { "cpp_os_yellowknife",	CPP_OS_YELLOWKNIFE_SPEC },		\
  { "cpp_os_mvme",		CPP_OS_MVME_SPEC },			\
  { "cpp_os_sim",		CPP_OS_SIM_SPEC },			\
  { "cpp_os_linux",		CPP_OS_LINUX_SPEC },			\
  { "cpp_os_solaris",		CPP_OS_SOLARIS_SPEC },			\
  { "cpp_os_default",		CPP_OS_DEFAULT_SPEC },

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mbig", "mcall-sysv" }

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */
#define PROFILE_BEFORE_PROLOGUE 1

/* Function name to call to do profiling.  */
#undef	RS6000_MCOUNT
#define RS6000_MCOUNT "_mcount"
