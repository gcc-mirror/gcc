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

#include "rs6000/rs6000.h"

/* Use the regular svr4 definitions.  */

#include "svr4.h"

/* Yes!  We are ELF.  */
#define	TARGET_OBJECT_FORMAT OBJECT_ELF

/* Default ABI to compile code for.  */
#define DEFAULT_ABI rs6000_current_abi

/* Default ABI to use.  */
#define RS6000_ABI_NAME "sysv"

/* Override rs6000.h definition.  */
#undef	ASM_DEFAULT_SPEC
#define	ASM_DEFAULT_SPEC "-mppc"

/* Override rs6000.h definition.  */
#undef	CPP_DEFAULT_SPEC
#define	CPP_DEFAULT_SPEC "-D_ARCH_PPC"

/* Small data support types.  */
enum rs6000_sdata_type {
  SDATA_NONE,			/* No small data support.  */
  SDATA_DATA,			/* Just put data in .sbss/.sdata, don't use relocs.  */
  SDATA_SYSV,			/* Use r13 to point to .sdata/.sbss.  */
  SDATA_EABI			/* Use r13 like above, r2 points to .sdata2/.sbss2.  */
};

extern enum rs6000_sdata_type rs6000_sdata;

/* V.4/eabi switches.  */
#define	MASK_NO_BITFIELD_TYPE	0x40000000	/* Set PCC_BITFIELD_TYPE_MATTERS to 0.  */
#define	MASK_STRICT_ALIGN	0x20000000	/* Set STRICT_ALIGNMENT to 1.  */
#define	MASK_RELOCATABLE	0x10000000	/* GOT pointers are PC relative.  */
#define	MASK_EABI		0x08000000	/* Adhere to eabi, not System V spec.  */
#define	MASK_LITTLE_ENDIAN	0x04000000	/* Target is little endian.  */
#define	MASK_REGNAMES		0x02000000	/* Use alternate register names.  */
#define	MASK_PROTOTYPE		0x01000000	/* Only prototyped fcns pass variable args.  */

#define	TARGET_NO_BITFIELD_TYPE	(target_flags & MASK_NO_BITFIELD_TYPE)
#define	TARGET_STRICT_ALIGN	(target_flags & MASK_STRICT_ALIGN)
#define	TARGET_RELOCATABLE	(target_flags & MASK_RELOCATABLE)
#define	TARGET_EABI		(target_flags & MASK_EABI)
#define	TARGET_LITTLE_ENDIAN	(target_flags & MASK_LITTLE_ENDIAN)
#define	TARGET_REGNAMES		(target_flags & MASK_REGNAMES)
#define	TARGET_PROTOTYPE	(target_flags & MASK_PROTOTYPE)
#define	TARGET_TOC		((target_flags & MASK_64BIT)		\
				 || ((target_flags & (MASK_RELOCATABLE	\
						      | MASK_MINIMAL_TOC)) \
				     && flag_pic > 1)			\
				 || DEFAULT_ABI == ABI_AIX)

#define	TARGET_BITFIELD_TYPE	(! TARGET_NO_BITFIELD_TYPE)
#define	TARGET_BIG_ENDIAN	(! TARGET_LITTLE_ENDIAN)
#define	TARGET_NO_PROTOTYPE	(! TARGET_PROTOTYPE)
#define	TARGET_NO_TOC		(! TARGET_TOC)
#define	TARGET_NO_EABI		(! TARGET_EABI)

/* Strings provided by SUBTARGET_OPTIONS */
extern const char *rs6000_abi_name;
extern const char *rs6000_sdata_name;

/* Override rs6000.h definition.  */
#undef	SUBTARGET_OPTIONS
#define	SUBTARGET_OPTIONS						\
  { "call-",  &rs6000_abi_name, "Select ABI calling convention." },			\
  { "sdata=", &rs6000_sdata_name, "Select method for sdata handling." }

/* Max # of bytes for variables to automatically be put into the .sdata
   or .sdata2 sections.  */
extern int g_switch_value;		/* Value of the -G xx switch.  */
extern int g_switch_set;		/* Whether -G xx was passed.  */

#define SDATA_DEFAULT_SIZE 8

/* Note, V.4 no longer uses a normal TOC, so make -mfull-toc, be just
   the same as -mminimal-toc.  */
/* Override rs6000.h definition.  */
#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
  { "bit-align",	-MASK_NO_BITFIELD_TYPE, "Align to the base type of the bitfield." },\
  { "no-bit-align",	 MASK_NO_BITFIELD_TYPE, "Don't align to the base type of the bitfield." },\
  { "strict-align",	 MASK_STRICT_ALIGN, "Don't assume that unaligned accesses are handled by the system" },\
  { "no-strict-align",	-MASK_STRICT_ALIGN, "Assume that unaligned accesses are handled by the system" },\
  { "relocatable",	 MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC, "Produce code relocatable at runtime." },\
  { "no-relocatable",	-MASK_RELOCATABLE, "Don't produce code relocatable at runtime." },\
  { "relocatable-lib",	 MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC, "Produce code relocatable at runtime." },\
  { "no-relocatable-lib", -MASK_RELOCATABLE, "Don't produce code relocatable at runtime." },\
  { "little-endian",	 MASK_LITTLE_ENDIAN, "Produce little endian code." },	\
  { "little",		 MASK_LITTLE_ENDIAN, "Produce little endian code." },	\
  { "big-endian",	-MASK_LITTLE_ENDIAN, "Produce big endian code." },	\
  { "big",		-MASK_LITTLE_ENDIAN, "Produce big endian code." },	\
  { "no-toc",		 0, "no description yet" },				\
  { "toc",		 MASK_MINIMAL_TOC, "no description yet" },		\
  { "full-toc",		 MASK_MINIMAL_TOC, "no description yet" },		\
  { "prototype",	 MASK_PROTOTYPE, "no description yet" },		\
  { "no-prototype",	-MASK_PROTOTYPE, "no description yet" },		\
  { "no-traceback",	 0, "no description yet" },				\
  { "eabi",		 MASK_EABI, "Use EABI." },				\
  { "no-eabi",		-MASK_EABI, "Don't use EABI." },			\
  { "regnames",		  MASK_REGNAMES, "Use alternate register names." },	\
  { "no-regnames",	 -MASK_REGNAMES, "Don't use alternate register names." },\
  { "sdata",		 0, "no description yet" },				\
  { "no-sdata",		 0, "no description yet" },				\
  { "sim",		 0, "Link with libsim.a, libc.a and sim-crt0.o." },	\
  { "ads",		 0, "Link with libads.a, libc.a and crt0.o." },		\
  { "yellowknife",	 0, "Link with libyk.a, libc.a and crt0.o." },		\
  { "mvme",		 0, "Link with libmvme.a, libc.a and crt0.o." },	\
  { "emb",		 0, "Set the PPC_EMB bit in the ELF flags header" },	\
  { "vxworks",		 0, "no description yet" },				\
  { "solaris-cclib",	 0, "no description yet" },				\
  { "shlib",		 0, "no description yet" },				\
  EXTRA_SUBTARGET_SWITCHES							\
  { "newlib",		 0, "no description yet" },

/* This is meant to be redefined in the host dependent files.  */
#define EXTRA_SUBTARGET_SWITCHES

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
  if (TARGET_RELOCATABLE && rs6000_current_abi == ABI_AIX)		\
    {									\
      target_flags &= ~MASK_RELOCATABLE;				\
      error ("-mrelocatable and -mcall-%s are incompatible.",		\
	     rs6000_abi_name);						\
    }									\
									\
  if (flag_pic > 1 && rs6000_current_abi == ABI_AIX)			\
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
  /* Treat -fPIC the same as -mrelocatable.  */				\
  if (flag_pic > 1)							\
    target_flags |= MASK_RELOCATABLE | MASK_MINIMAL_TOC | MASK_NO_FP_IN_TOC; \
									\
  else if (TARGET_RELOCATABLE)						\
    flag_pic = 2;							\
									\
} while (0)


/* Override rs6000.h definition.  */
#undef	TARGET_DEFAULT
#define	TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

/* Override rs6000.h definition.  */
#undef	PROCESSOR_DEFAULT
#define	PROCESSOR_DEFAULT PROCESSOR_PPC750

/* System V.4 uses register 13 as a pointer to the small data area,
   so it is not available to the normal user.  */

#define FIXED_R13 1

/* Size of the V.4 varargs area if needed.  */
/* Override rs6000.h definition.  */
#undef	RS6000_VARARGS_AREA
#define RS6000_VARARGS_AREA ((rs6000_sysv_varargs_p) ? RS6000_VARARGS_SIZE : 0)

/* Override default big endianism definitions in rs6000.h.  */
#undef	BYTES_BIG_ENDIAN
#undef	WORDS_BIG_ENDIAN
#define	BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)
#define	WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#if !defined(_LITTLE_ENDIAN) && !defined(__sun__)
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Define cutoff for using external functions to save floating point.
   Currently on V.4, always use inline stores.  */
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 64)

/* Put jump tables in read-only memory, rather than in .text.  */
#define JUMP_TABLES_IN_TEXT_SECTION 0

/* Prefix and suffix to use to saving floating point.  */
#define	SAVE_FP_PREFIX "_savefpr_"
#define SAVE_FP_SUFFIX "_l"

/* Prefix and suffix to use to restoring floating point.  */
#define	RESTORE_FP_PREFIX "_restfpr_"
#define RESTORE_FP_SUFFIX "_l"

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#define PTRDIFF_TYPE "int"

/* Type used for wchar_t, as a string used in a declaration.  */
/* Override svr4.h definition.  */
#undef	WCHAR_TYPE
#define WCHAR_TYPE "long int"

/* Width of wchar_t in bits.  */
/* Override svr4.h definition.  */
#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Make int foo : 8 not cause structures to be aligned to an int boundary.  */
/* Override elfos.h definition.  */
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

/* Real stack boundary as mandated by the appropriate ABI.  */
#define ABI_STACK_BOUNDARY ((TARGET_EABI) ? 64 : 128)

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT ((TARGET_EABI) ? 64 : 128)

#undef  BIGGEST_FIELD_ALIGNMENT
#undef  ADJUST_FIELD_ALIGN
#undef  ROUND_TYPE_ALIGN

/* Use ELF style section commands.  */

#define	TEXT_SECTION_ASM_OP	"\t.section\t\".text\""

#define	DATA_SECTION_ASM_OP	"\t.section\t\".data\""

#define	BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

/* Override elfos.h definition.  */
#undef	INIT_SECTION_ASM_OP
#define	INIT_SECTION_ASM_OP "\t.section\t\".init\",\"ax\""

/* Override elfos.h definition.  */
#undef	FINI_SECTION_ASM_OP
#define	FINI_SECTION_ASM_OP "\t.section\t\".fini\",\"ax\""

#define	TOC_SECTION_ASM_OP "\t.section\t\".got\",\"aw\""

/* Put PC relative got entries in .got2.  */
#define	MINIMAL_TOC_SECTION_ASM_OP \
  ((TARGET_RELOCATABLE || flag_pic) ? "\t.section\t\".got2\",\"aw\"" : "\t.section\t\".got1\",\"aw\"")

/* Put relocatable data in .data, not .rodata so initialized pointers can be updated.  */
/* Override elfos.h definition.  */
#undef	CONST_SECTION_ASM_OP
#define	CONST_SECTION_ASM_OP \
  ((TARGET_RELOCATABLE || flag_pic) ? "\t.section\t\".data\"\t# .rodata" : "\t.section\t\".rodata\"")


#define	SDATA_SECTION_ASM_OP "\t.section\t\".sdata\",\"aw\""
#define	SDATA2_SECTION_ASM_OP "\t.section\t\".sdata2\",\"a\""
#define	SBSS_SECTION_ASM_OP \
  ((DEFAULT_ABI == ABI_SOLARIS) ? "\t.section\t\".sbss\",\"aw\"" : "\t.section\t\".sbss\",\"aw\",@nobits")


/* Besides the usual ELF sections, we need a toc section.  */
/* Override elfos.h definition.  */
#undef	EXTRA_SECTIONS
#define	EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_toc, in_sdata, in_sdata2, in_sbss, in_init, in_fini

/* Override elfos.h definition.  */
#undef	EXTRA_SECTION_FUNCTIONS
#define	EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  TOC_SECTION_FUNCTION							\
  SDATA_SECTION_FUNCTION						\
  SDATA2_SECTION_FUNCTION						\
  SBSS_SECTION_FUNCTION							\
  INIT_SECTION_FUNCTION							\
  FINI_SECTION_FUNCTION

#define	TOC_SECTION_FUNCTION						\
void									\
toc_section ()								\
{									\
  if (in_section != in_toc)						\
    {									\
      in_section = in_toc;						\
      if (DEFAULT_ABI == ABI_AIX					\
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
      else if (DEFAULT_ABI == ABI_AIX && !TARGET_RELOCATABLE)		\
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

#define	SDATA_SECTION_FUNCTION						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      in_section = in_sdata;						\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
    }									\
}

#define	SDATA2_SECTION_FUNCTION						\
void									\
sdata2_section ()							\
{									\
  if (in_section != in_sdata2)						\
    {									\
      in_section = in_sdata2;						\
      fprintf (asm_out_file, "%s\n", SDATA2_SECTION_ASM_OP);		\
    }									\
}

#define	SBSS_SECTION_FUNCTION						\
void									\
sbss_section ()								\
{									\
  if (in_section != in_sbss)						\
    {									\
      in_section = in_sbss;						\
      fprintf (asm_out_file, "%s\n", SBSS_SECTION_ASM_OP);		\
    }									\
}

#define	INIT_SECTION_FUNCTION						\
void									\
init_section ()								\
{									\
  if (in_section != in_init)						\
    {									\
      in_section = in_init;						\
      fprintf (asm_out_file, "%s\n", INIT_SECTION_ASM_OP);		\
    }									\
}

#define	FINI_SECTION_FUNCTION						\
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

/* Override elfos.h definition.  */
#undef	SELECT_RTX_SECTION
#define	SELECT_RTX_SECTION(MODE, X) rs6000_select_rtx_section (MODE, X)

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

/* Override elfos.h definition.  */
#undef	SELECT_SECTION
#define	SELECT_SECTION(DECL,RELOC) rs6000_select_section (DECL, RELOC)

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
#define	ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X)				\
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

/* Override elfos.h definition.  */
#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    const char *init_ptr = (TARGET_64BIT) ? ".quad" : ".long";		\
									\
    if (TARGET_RELOCATABLE && (get_pool_size () != 0 || profile_flag)	\
	&& uses_TOC())							\
      {									\
	char buf[256];							\
	const char *buf_ptr;						\
									\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "LCL", rs6000_pic_labelno);	\
									\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);			\
	fprintf (FILE, "\t%s ", init_ptr);				\
	assemble_name (FILE, buf);					\
	putc ('-', FILE);						\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);	\
	assemble_name (FILE, buf);					\
	putc ('\n', FILE);						\
      }									\
									\
    fprintf (FILE, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', FILE);							\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
									\
    if (DEFAULT_ABI == ABI_AIX)						\
      {									\
	const char *desc_name, *orig_name;				\
									\
        STRIP_NAME_ENCODING (orig_name, NAME);				\
        desc_name = orig_name;						\
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
    ASM_OUTPUT_LABEL (FILE, NAME);					\
  } while (0)

/* A C compound statement that outputs the assembler code for a thunk function,
    used to implement C++ virtual function calls with multiple inheritance.  The
    thunk acts as a wrapper around a virtual function, adjusting the implicit
    object parameter before handing control off to the real function.

    First, emit code to add the integer DELTA to the location that contains the
    incoming first argument.  Assume that this argument contains a pointer, and
    is the one used to pass the this' pointer in C++.  This is the incoming
    argument *before* the function prologue, e.g. %o0' on a sparc.  The
    addition must preserve the values of all other incoming arguments.

    After the addition, emit code to jump to FUNCTION, which is a
    FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
    the return address.  Hence returning from FUNCTION will return to whoever
    called the current thunk'.

    The effect must be as if FUNCTION had been called directly with the adjusted
    first argument.  This macro is responsible for emitting all of the code for
    a thunk function; FUNCTION_PROLOGUE' and FUNCTION_EPILOGUE' are not
    invoked.

    The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
    extracted from it.)  It might possibly be useful on some targets, but
    probably not.

    If you do not define this macro, the target-independent code in the C++
    frontend will generate a less efficient heavyweight thunk that calls
    FUNCTION instead of jumping to it.  The generic approach does not support
    varargs.  */

#define	ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION) \
  output_mi_thunk (FILE, THUNK_FNDECL, DELTA, FUNCTION)

/* How to renumber registers for dbx and gdb.  */

#define	DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* The USER_LABEL_PREFIX stuff is affected by the -fleading-underscore
   flag.  The LOCAL_LABEL_PREFIX variable is used by dbxelf.h.  */

#define	LOCAL_LABEL_PREFIX "."
#define	USER_LABEL_PREFIX ""

/* svr4.h overrides ASM_OUTPUT_INTERNAL_LABEL.  */

#define	ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  asm_fprintf (FILE, "%L%s", PREFIX)

#define	ASM_OUTPUT_LABEL(FILE,NAME)	\
  (assemble_name (FILE, NAME), fputs (":\n", FILE))

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define	ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.globl ", FILE);	\
       assemble_name (FILE, NAME); putc ('\n', FILE);} while (0)

/* This is how to allocate empty space in some section.  Use .space
   instead of .zero because the Solaris PowerPC assembler doesn't
   like it, and gas accepts either syntax.  */

/* Override elfos.h definition.  */
#undef	SKIP_ASM_OP
#define SKIP_ASM_OP	".space"

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#define	LOCAL_ASM_OP	".local"

#define	LCOMM_ASM_OP	".lcomm"

/* Override elfos.h definition.  */
#undef	ASM_OUTPUT_ALIGNED_LOCAL
#define	ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
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

/* Describe how to emit uninitialized external linkage items.  */
#define	ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
do {									\
  ASM_GLOBALIZE_LABEL (FILE, NAME);					\
  ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);			\
} while (0)

/* Switch  Recognition by gcc.c.  Add -G xx support.  */

/* Override svr4.h definition.  */
#undef	SWITCH_TAKES_ARG
#define	SWITCH_TAKES_ARG(CHAR)						\
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o'			\
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u'			\
   || (CHAR) == 'I' || (CHAR) == 'm' || (CHAR) == 'x'			\
   || (CHAR) == 'L' || (CHAR) == 'A' || (CHAR) == 'V'			\
   || (CHAR) == 'B' || (CHAR) == 'b' || (CHAR) == 'G')

/* Output .file.  */
/* Override elfos.h definition.  */
#undef	ASM_FILE_START
#define	ASM_FILE_START(FILE)						\
do {									\
  output_file_directive ((FILE), main_input_filename);			\
  rs6000_file_start (FILE, TARGET_CPU_DEFAULT);				\
} while (0)


extern int fixuplabelno;

/* This is how to output an assembler line defining an `int' constant.
   For -mrelocatable, we mark all addresses that need to be fixed up
   in the .fixup section.  */
/* Override rs6000.h definition.  */
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
      char buf[256];							\
									\
      recurse = 1;							\
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", fixuplabelno);		\
      fixuplabelno++;							\
      ASM_OUTPUT_LABEL (FILE, buf);					\
      fprintf (FILE, "\t.long (");					\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, ")@fixup\n");					\
      fprintf (FILE, "\t.section\t\".fixup\",\"aw\"\n");		\
      ASM_OUTPUT_ALIGN (FILE, 2);					\
      fprintf (FILE, "\t.long\t");					\
      assemble_name (FILE, buf);					\
      fprintf (FILE, "\n\t.previous\n");				\
      recurse = 0;							\
    }									\
  /* Remove initial .'s to turn a -mcall-aixdesc function		\
     address into the address of the descriptor, not the function	\
     itself.  */							\
  else if (GET_CODE (VALUE) == SYMBOL_REF				\
	   && XSTR (VALUE, 0)[0] == '.'					\
	   && DEFAULT_ABI == ABI_AIX)					\
    {									\
      const char *name = XSTR (VALUE, 0);				\
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

/* This is how to output an assembler line defining an address 
   constant for the dwarf call unwinding information.
   For -mrelocatable, we mark all addresses that need to be fixed up
   in the .fixup section.  */

#define	ASM_OUTPUT_DWARF_ADDR(FILE,LABEL)				\
do {									\
  if (TARGET_RELOCATABLE)						\
    {									\
      char buf[256];							\
      const char *p;							\
									\
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", fixuplabelno);		\
      fixuplabelno++;							\
      ASM_OUTPUT_LABEL (FILE, buf);					\
      fprintf (FILE, "\t.%dbyte\t", POINTER_SIZE / BITS_PER_UNIT);	\
      assemble_name (FILE, LABEL);					\
      fprintf (FILE, "\n");						\
      fprintf (FILE, "\t.section \".fixup\",\"aw\"\n");			\
      ASM_OUTPUT_ALIGN (FILE, 2);					\
      fprintf (FILE, "\t.long\t");					\
      assemble_name (FILE, buf);					\
      fprintf (FILE, "\n\t.previous\n");				\
    }									\
  else									\
    {									\
      fprintf (FILE, "\t.%dbyte\t", POINTER_SIZE / BITS_PER_UNIT);	\
      assemble_name (FILE, LABEL);					\
    }									\
} while (0)

/* This is the end of what might become sysv4.h.  */

/* Allow stabs and dwarf, for now, make stabs the default debugging type,
   not dwarf since G++ doesn't support dwarf.  */
#undef	PREFERRED_DEBUGGING_TYPE
#define	PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define	DBX_DEBUGGING_INFO

/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  For real AIX calling sequences, we also replace the
   function name with the real name (1 or 2 leading .'s), rather than
   the function descriptor name.  This saves a lot of overriding code
   to read the prefixes.  */

#undef	ENCODE_SECTION_INFO
#define	ENCODE_SECTION_INFO(DECL) rs6000_encode_section_info (DECL)

/* The ELF version doesn't encode [DS] or whatever at the end of symbols.  */

#define	RS6000_OUTPUT_BASENAME(FILE, NAME)	\
    assemble_name (FILE, NAME)

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  Discard
   a leading * or @.  */
#define	STRIP_NAME_ENCODING(VAR,SYMBOL_NAME)				\
do {									\
  const char *_name = SYMBOL_NAME;					\
  while (*_name == '*' || *_name == '@')				\
    _name++;								\
  (VAR) = _name;							\
} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

/* Override elfos.h definition.  */
#undef	ASM_OUTPUT_LABELREF
#define	ASM_OUTPUT_LABELREF(FILE,NAME)		\
do {						\
  const char *_name = NAME;			\
  if (*_name == '@')				\
    _name++;					\
 						\
  if (*_name == '*')				\
    fprintf (FILE, "%s", _name + 1);		\
  else						\
    asm_fprintf (FILE, "%U%s", _name);		\
} while (0)

/* Switch into a generic section.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.

   If the section has already been defined, we must not
   emit the attributes here. The SVR4 assembler does not
   recognize section redefinitions.
   If DECL is NULL, no attributes are emitted.

   Note, Solaris as doesn't like @nobits, and gas can handle .sbss without
   needing @nobits.  */

/* Override elfos.h definition.  */
#undef	ASM_OUTPUT_SECTION_NAME
#define	ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC)		\
do {									\
  static struct section_info						\
    {									\
      struct section_info *next;				        \
      char *name;						        \
      enum sect_enum {SECT_RW, SECT_RO, SECT_EXEC} type;		\
    } *sections;							\
  struct section_info *s;						\
  const char *mode;							\
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

/* Override elfos.h definition.  */
#undef	ASM_OUTPUT_CONSTRUCTOR
#define	ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
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
/* Override elfos.h definition.  */
#undef	ASM_OUTPUT_DESTRUCTOR
#define	ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
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

/* Override rs6000.h definition.  */
#undef	TARGET_VERSION
#define	TARGET_VERSION fprintf (stderr, " (PowerPC System V.4)");

#define	CPP_PREDEFINES \
  "-DPPC -Dunix -D__svr4__ -Asystem(unix) -Asystem(svr4) -Acpu(powerpc) -Amachine(powerpc)"

/* Pass various options to the assembler.  */
/* Override svr4.h definition.  */
#undef	ASM_SPEC
#define	ASM_SPEC "%(asm_cpu) \
%{.s: %{mregnames} %{mno-regnames}} %{.S: %{mregnames} %{mno-regnames}} \
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} \
%{mrelocatable} %{mrelocatable-lib} %{fpic:-K PIC} %{fPIC:-K PIC} \
%{memb} %{!memb: %{msdata: -memb} %{msdata=eabi: -memb}} \
%{mlittle} %{mlittle-endian} %{mbig} %{mbig-endian} \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-solaris: -mlittle -msolaris} \
    %{mcall-linux: -mbig} }}}}"

#define	CC1_ENDIAN_BIG_SPEC ""

#define	CC1_ENDIAN_LITTLE_SPEC "\
%{!mstrict-align: %{!mno-strict-align: \
	-mstrict-align \
}}"

#define	CC1_ENDIAN_DEFAULT_SPEC "%(cc1_endian_big_spec)"

/* Pass -G xxx to the compiler and set correct endian mode.  */
#define	CC1_SPEC "%{G*} \
%{mlittle: %(cc1_endian_little)} %{!mlittle: %{mlittle-endian: %(cc1_endian_little)}} \
%{mbig: %(cc1_endian_big)} %{!mbig: %{mbig-endian: %(cc1_endian_big)}} \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-aixdesc: -mbig %(cc1_endian_big) } \
    %{mcall-solaris: -mlittle %(cc1_endian_little) } \
    %{mcall-linux: -mbig %(cc1_endian_big) } \
    %{!mcall-aixdesc: %{!mcall-solaris: %{!mcall-linux: \
	    %(cc1_endian_default) \
    }}} \
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

/* Don't put -Y P,<path> for cross compilers.  */
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

/* Default starting address if specified.  */
#define LINK_START_SPEC "\
%{mads: %(link_start_ads) } \
%{myellowknife: %(link_start_yellowknife) } \
%{mmvme: %(link_start_mvme) } \
%{msim: %(link_start_sim) } \
%{mcall-linux: %(link_start_linux) } \
%{mcall-solaris: %(link_start_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(link_start_default) }}}}}}"

#define LINK_START_DEFAULT_SPEC ""

/* Override svr4.h definition.  */
#undef	LINK_SPEC
#define	LINK_SPEC "\
%{h*} %{v:-V} %{G*} \
%{Wl,*:%*} %{YP,*} %{R*} \
%{Qy:} %{!Qn:-Qy} \
%(link_shlib) \
%{!Wl,-T*: %{!T*: %(link_start) }} \
%(link_target) \
%(link_os)"

/* For now, turn off shared libraries by default.  */
#ifndef SHARED_LIB_SUPPORT
#define NO_SHARED_LIB_SUPPORT
#endif

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
#define	LINK_TARGET_SPEC "\
%{mlittle: -oformat elf32-powerpcle } %{mlittle-endian: -oformat elf32-powerpcle } \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-solaris: -oformat elf32-powerpcle} \
  }}}}"

/* Any specific OS flags.  */
#define LINK_OS_SPEC "\
%{mads: %(link_os_ads) } \
%{myellowknife: %(link_os_yellowknife) } \
%{mmvme: %(link_os_mvme) } \
%{msim: %(link_os_sim) } \
%{mcall-linux: %(link_os_linux) } \
%{mcall-solaris: %(link_os_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(link_os_default) }}}}}}"

#define LINK_OS_DEFAULT_SPEC ""

#define CPP_SYSV_SPEC \
"%{mrelocatable*: -D_RELOCATABLE} \
%{fpic: -D__PIC__=1 -D__pic__=1} \
%{!fpic: %{fPIC: -D__PIC__=2 -D__pic__=2}} \
%{mcall-sysv: -D_CALL_SYSV} \
%{mcall-aix: -D_CALL_AIX} %{mcall-aixdesc: -D_CALL_AIX -D_CALL_AIXDESC} \
%{!mcall-sysv: %{!mcall-aix: %{!mcall-aixdesc: %(cpp_sysv_default) }}} \
%{msoft-float: -D_SOFT_FLOAT} \
%{!msoft-float: %{!mhard-float: \
    %{mcpu=401: -D_SOFT_FLOAT} \
    %{mcpu=403: -D_SOFT_FLOAT} \
    %{mcpu=ec603e: -D_SOFT_FLOAT} \
    %{mcpu=801: -D_SOFT_FLOAT} \
    %{mcpu=821: -D_SOFT_FLOAT} \
    %{mcpu=823: -D_SOFT_FLOAT} \
    %{mcpu=860: -D_SOFT_FLOAT} \
    %{!mcpu*: %(cpp_float_default) }}}"

/* Whether floating point is disabled by default.  */
#define	CPP_FLOAT_DEFAULT_SPEC ""

#define	CPP_SYSV_DEFAULT_SPEC "-D_CALL_SYSV"

#define CPP_ENDIAN_BIG_SPEC "-D_BIG_ENDIAN -D__BIG_ENDIAN__ -Amachine(bigendian)"

#define CPP_ENDIAN_LITTLE_SPEC "-D_LITTLE_ENDIAN -D__LITTLE_ENDIAN__ -Amachine(littleendian)"

#define CPP_ENDIAN_SOLARIS_SPEC "-D__LITTLE_ENDIAN__ -Amachine(littleendian)"

/* For solaris, don't define _LITTLE_ENDIAN, it conflicts with a header file.  */
#define	CPP_ENDIAN_SPEC \
"%{mlittle: %(cpp_endian_little) } \
%{mlittle-endian: %(cpp_endian_little) } \
%{mbig: %(cpp_endian_big) } \
%{mbig-endian: %(cpp_endian_big) } \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-solaris: %(cpp_endian_solaris) } \
    %{mcall-linux: %(cpp_endian_big) } \
    %{mcall-aixdesc:  %(cpp_endian_big) } \
    %{!mcall-solaris: %{!mcall-linux: %{!mcall-aixdesc: %(cpp_endian_default) }}}}}}}"

#define	CPP_ENDIAN_DEFAULT_SPEC "%(cpp_endian_big)"

/* Override rs6000.h definition.  */
#undef	CPP_SPEC
#define	CPP_SPEC "%{posix: -D_POSIX_SOURCE} %(cpp_sysv) %(cpp_endian) %(cpp_cpu) \
%{mads: %(cpp_os_ads) } \
%{myellowknife: %(cpp_os_yellowknife) } \
%{mmvme: %(cpp_os_mvme) } \
%{msim: %(cpp_os_sim) } \
%{mcall-linux: %(cpp_os_linux) } \
%{mcall-solaris: %(cpp_os_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(cpp_os_default) }}}}}}"

#define	CPP_OS_DEFAULT_SPEC ""

/* Override svr4.h definition.  */
#undef	STARTFILE_SPEC
#define	STARTFILE_SPEC "\
%{mads: %(startfile_ads) } \
%{myellowknife: %(startfile_yellowknife) } \
%{mmvme: %(startfile_mvme) } \
%{msim: %(startfile_sim) } \
%{mcall-linux: %(startfile_linux) } \
%{mcall-solaris: %(startfile_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(startfile_default) }}}}}}"

#define	STARTFILE_DEFAULT_SPEC ""

/* Override svr4.h definition.  */
#undef	LIB_SPEC
#define	LIB_SPEC "\
%{mads: %(lib_ads) } \
%{myellowknife: %(lib_yellowknife) } \
%{mmvme: %(lib_mvme) } \
%{msim: %(lib_sim) } \
%{mcall-linux: %(lib_linux) } \
%{mcall-solaris: %(lib_solaris) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(lib_default) }}}}}}"

/* Override rs6000.h definition.  */
#undef	LIBGCC_SPEC
#define	LIBGCC_SPEC "libgcc.a%s"

#define LIB_DEFAULT_SPEC ""

/* Override svr4.h definition.  */
#undef	ENDFILE_SPEC
#define	ENDFILE_SPEC "\
%{mads: %(endfile_ads)} \
%{myellowknife: %(endfile_yellowknife)} \
%{mmvme: %(endfile_mvme)} \
%{msim: %(endfile_sim)} \
%{mcall-linux: %(endfile_linux) } \
%{mcall-solaris: %(endfile_solaris)} \
%{mvxworks: %(endfile_vxworks) } \
%{!mads: %{!myellowknife: %{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %{!mvxworks: %(endfile_default) }}}}}}}"

#define	ENDFILE_DEFAULT_SPEC ""

/* Motorola ADS support.  */
#define LIB_ADS_SPEC "--start-group -lads -lc --end-group"

#define	STARTFILE_ADS_SPEC "ecrti.o%s crt0.o%s crtbegin.o%s"

#define	ENDFILE_ADS_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_ADS_SPEC "-T ads.ld%s"

#define LINK_OS_ADS_SPEC ""

#define CPP_OS_ADS_SPEC ""

/* Motorola Yellowknife support.  */
#define LIB_YELLOWKNIFE_SPEC "--start-group -lyk -lc --end-group"

#define	STARTFILE_YELLOWKNIFE_SPEC "ecrti.o%s crt0.o%s crtbegin.o%s"

#define	ENDFILE_YELLOWKNIFE_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_YELLOWKNIFE_SPEC "-T yellowknife.ld%s"

#define LINK_OS_YELLOWKNIFE_SPEC ""

#define CPP_OS_YELLOWKNIFE_SPEC ""

/* Motorola MVME support.  */
#define LIB_MVME_SPEC "--start-group -lmvme -lc --end-group"

#define	STARTFILE_MVME_SPEC "ecrti.o%s crt0.o%s crtbegin.o%s"

#define	ENDFILE_MVME_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_MVME_SPEC "-Ttext 0x40000"

#define LINK_OS_MVME_SPEC ""

#define CPP_OS_MVME_SPEC ""

/* PowerPC simulator based on netbsd system calls support.  */
#define LIB_SIM_SPEC "--start-group -lsim -lc --end-group"

#define	STARTFILE_SIM_SPEC "ecrti.o%s sim-crt0.o%s crtbegin.o%s"

#define	ENDFILE_SIM_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_SIM_SPEC ""

#define LINK_OS_SIM_SPEC "-m elf32ppcsim"

#define CPP_OS_SIM_SPEC ""

/* GNU/Linux support.  */
#ifdef USE_GNULIBC_1
#define LIB_LINUX_SPEC "%{mnewlib: --start-group -llinux -lc --end-group } \
%{!mnewlib: -lc }"
#else
#define LIB_LINUX_SPEC "%{mnewlib: --start-group -llinux -lc --end-group } \
%{!mnewlib: %{shared:-lc} %{!shared: %{pthread:-lpthread } \
%{profile:-lc_p} %{!profile:-lc}}}"
#endif

#define	STARTFILE_LINUX_SPEC "\
%{!shared: %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}} \
%{mnewlib: ecrti.o%s} %{!mnewlib: crti.o%s} \
%{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#define	ENDFILE_LINUX_SPEC "%{!shared:crtend.o%s} %{shared:crtendS.o%s} \
%{mnewlib: ecrtn.o%s} %{!mnewlib: crtn.o%s}"

#define LINK_START_LINUX_SPEC ""

#define LINK_OS_LINUX_SPEC "-m elf32ppclinux %{!shared: %{!static: \
  %{rdynamic:-export-dynamic} \
  %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}}}"

#ifdef USE_GNULIBC_1
#define CPP_OS_LINUX_SPEC "-D__unix__ -D__linux__ \
%{!undef:%{!ansi:%{!std=*:-Dunix -Dlinux}%{std=gnu*:-Dunix -Dlinux}}} \
-Asystem(unix) -Asystem(posix)"
#else
#define CPP_OS_LINUX_SPEC "-D__unix__ -D__linux__ \
%{!undef:%{!ansi:%{!std=*:-Dunix -Dlinux}%{std=gnu*:-Dunix -Dlinux}}} \
-Asystem(unix) -Asystem(posix) %{pthread:-D_REENTRANT}"
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

#define	STARTFILE_SOLARIS_SPEC "\
%{!msolaris-cclib: scrti.o%s scrt0.o%s} \
%{msolaris-cclib: /opt/SUNWspro/SC4.0/lib/crti.o%s /opt/SUNWspro/SC4.0/lib/crt1.o%s} \
%{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#define	ENDFILE_SOLARIS_SPEC "\
%{!shared:crtend.o%s} %{shared:crtendS.o%s} \
%{!msolaris-cclib: scrtn.o%s} \
%{msolaris-cclib: /opt/SUNWspro/SC4.0/lib/crtn.o%s}"

#define LINK_START_SOLARIS_SPEC ""

#define LINK_OS_SOLARIS_SPEC ""

#define CPP_OS_SOLARIS_SPEC "-D__ppc -D__sun__=1 -D__unix__ -D__svr4__  -D__SVR4__ \
%{!undef:%{!ansi:%{!std=*:-Dsun=1 -Dunix -DSVR4 -D__EXTENSIONS__} \
               %{std=gnu*:-Dsun=1 -Dunix -DSVR4 -D__EXTENSIONS__}}} \
-Amachine(prep)"

/* VxWorks support.  */
/* VxWorks does all the library stuff itself.  */
#define LIB_VXWORKS_SPEC ""

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#define	STARTFILE_VXWORKS_SPEC ""

#define	ENDFILE_VXWORKS_SPEC ""

/* Because it uses ld -r, vxworks has no start/end files, nor starting
   address.  */

#define LINK_START_VXWORKS_SPEC ""

#define LINK_OS_VXWORKS_SPEC "-r"

#define CPP_OS_VXWORKS_SPEC "\
-DCPU_FAMILY=PPC \
%{!mcpu*: \
  %{mpowerpc*: -DCPU=PPC603} \
  %{!mno-powerpc: -DCPU=PPC603}} \
%{mcpu=powerpc: -DCPU=PPC603} \
%{mcpu=401: -DCPU=PPC403} \
%{mcpu=403: -DCPU=PPC403} \
%{mcpu=601: -DCPU=PPC601} \
%{mcpu=602: -DCPU=PPC603} \
%{mcpu=603: -DCPU=PPC603} \
%{mcpu=603e: -DCPU=PPC603} \
%{mcpu=ec603e: -DCPU=PPC603} \
%{mcpu=604: -DCPU=PPC604} \
%{mcpu=604e: -DCPU=PPC604} \
%{mcpu=620: -DCPU=PPC604} \
%{mcpu=740: -DCPU=PPC603} \
%{mcpu=750: -DCPU=PPC603} \
%{mcpu=801: -DCPU=PPC603} \
%{mcpu=821: -DCPU=PPC603} \
%{mcpu=823: -DCPU=PPC603} \
%{mcpu=860: -DCPU=PPC603}"

/* Define any extra SPECS that the compiler needs to generate.  */
/* Override rs6000.h definition.  */
#undef	SUBTARGET_EXTRA_SPECS
#define	SUBTARGET_EXTRA_SPECS						\
  { "cpp_sysv",			CPP_SYSV_SPEC },			\
  { "cpp_sysv_default",		CPP_SYSV_DEFAULT_SPEC },		\
  { "cpp_endian_default",	CPP_ENDIAN_DEFAULT_SPEC },		\
  { "cpp_endian",		CPP_ENDIAN_SPEC },			\
  { "lib_ads",			LIB_ADS_SPEC },				\
  { "lib_yellowknife",		LIB_YELLOWKNIFE_SPEC },			\
  { "lib_mvme",			LIB_MVME_SPEC },			\
  { "lib_sim",			LIB_SIM_SPEC },				\
  { "lib_linux",		LIB_LINUX_SPEC },			\
  { "lib_solaris",		LIB_SOLARIS_SPEC },			\
  { "lib_vxworks",		LIB_VXWORKS_SPEC },			\
  { "lib_default",		LIB_DEFAULT_SPEC },			\
  { "startfile_ads",		STARTFILE_ADS_SPEC },			\
  { "startfile_yellowknife",	STARTFILE_YELLOWKNIFE_SPEC },		\
  { "startfile_mvme",		STARTFILE_MVME_SPEC },			\
  { "startfile_sim",		STARTFILE_SIM_SPEC },			\
  { "startfile_linux",		STARTFILE_LINUX_SPEC },			\
  { "startfile_solaris",	STARTFILE_SOLARIS_SPEC },		\
  { "startfile_vxworks",	STARTFILE_VXWORKS_SPEC },		\
  { "startfile_default",	STARTFILE_DEFAULT_SPEC },		\
  { "endfile_ads",		ENDFILE_ADS_SPEC },			\
  { "endfile_yellowknife",	ENDFILE_YELLOWKNIFE_SPEC },		\
  { "endfile_mvme",		ENDFILE_MVME_SPEC },			\
  { "endfile_sim",		ENDFILE_SIM_SPEC },			\
  { "endfile_linux",		ENDFILE_LINUX_SPEC },			\
  { "endfile_solaris",		ENDFILE_SOLARIS_SPEC },			\
  { "endfile_vxworks",		ENDFILE_VXWORKS_SPEC },			\
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
  { "link_start_vxworks",	LINK_START_VXWORKS_SPEC },		\
  { "link_start_default",	LINK_START_DEFAULT_SPEC },		\
  { "link_os",			LINK_OS_SPEC },				\
  { "link_os_ads",		LINK_OS_ADS_SPEC },			\
  { "link_os_yellowknife",	LINK_OS_YELLOWKNIFE_SPEC },		\
  { "link_os_mvme",		LINK_OS_MVME_SPEC },			\
  { "link_os_sim",		LINK_OS_SIM_SPEC },			\
  { "link_os_linux",		LINK_OS_LINUX_SPEC },			\
  { "link_os_solaris",		LINK_OS_SOLARIS_SPEC },			\
  { "link_os_vxworks",		LINK_OS_VXWORKS_SPEC },			\
  { "link_os_default",		LINK_OS_DEFAULT_SPEC },			\
  { "cc1_endian_big",		CC1_ENDIAN_BIG_SPEC },			\
  { "cc1_endian_little",	CC1_ENDIAN_LITTLE_SPEC },		\
  { "cc1_endian_default",	CC1_ENDIAN_DEFAULT_SPEC },		\
  { "cpp_endian_big",		CPP_ENDIAN_BIG_SPEC },			\
  { "cpp_endian_little",	CPP_ENDIAN_LITTLE_SPEC },		\
  { "cpp_endian_solaris",	CPP_ENDIAN_SOLARIS_SPEC },		\
  { "cpp_float_default",	CPP_FLOAT_DEFAULT_SPEC },		\
  { "cpp_os_ads",		CPP_OS_ADS_SPEC },			\
  { "cpp_os_yellowknife",	CPP_OS_YELLOWKNIFE_SPEC },		\
  { "cpp_os_mvme",		CPP_OS_MVME_SPEC },			\
  { "cpp_os_sim",		CPP_OS_SIM_SPEC },			\
  { "cpp_os_linux",		CPP_OS_LINUX_SPEC },			\
  { "cpp_os_solaris",		CPP_OS_SOLARIS_SPEC },			\
  { "cpp_os_vxworks",		CPP_OS_VXWORKS_SPEC },			\
  { "cpp_os_default",		CPP_OS_DEFAULT_SPEC },

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#define	MULTILIB_DEFAULTS { "mbig", "mcall-sysv" }

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */
#define PROFILE_BEFORE_PROLOGUE 1

/* Function name to call to do profiling.  */
#define RS6000_MCOUNT "_mcount"
