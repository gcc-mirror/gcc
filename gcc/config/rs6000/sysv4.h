/* Target definitions for GNU compiler for PowerPC running System V.4
   Copyright (C) 1995, Free Software Foundation, Inc.
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
#define	MASK_NO_TRACEBACK	0x08000000	/* eliminate traceback words */
#define MASK_LITTLE_ENDIAN	0x04000000	/* target is little endian */
#define MASK_AIX_CALLS		0x02000000	/* Use AIX calling sequence */
#define MASK_PROTOTYPE		0x01000000	/* Only prototyped fcns pass variable args */

#define	TARGET_NO_BITFIELD_TYPE	(target_flags & MASK_NO_BITFIELD_TYPE)
#define TARGET_STRICT_ALIGN	(target_flags & MASK_STRICT_ALIGN)
#define TARGET_RELOCATABLE	(target_flags & MASK_RELOCATABLE)
#define TARGET_NO_TRACEBACK	(target_flags & MASK_NO_TRACEBACK)
#define TARGET_LITTLE_ENDIAN	(target_flags & MASK_LITTLE_ENDIAN)
#define TARGET_AIX_CALLS	(target_flags & MASK_AIX_CALLS)
#define	TARGET_PROTOTYPE	(target_flags & MASK_PROTOTYPE)
#define	TARGET_TOC		(target_flags & (MASK_64BIT		\
						 | MASK_RELOCATABLE	\
						 | MASK_MINIMAL_TOC))

#define	TARGET_BITFIELD_TYPE	(! TARGET_NO_BITFIELD_TYPE)
#define	TARGET_TRACEBACK	(! TARGET_NO_TRACEBACK)
#define TARGET_BIG_ENDIAN	(! TARGET_LITTLE_ENDIAN)
#define TARGET_NO_AIX_CALLS	(! TARGET_AIX_CALLS)
#define	TARGET_NO_PROTOTYPE	(! TARGET_PROTOTYPE)
#define	TARGET_NO_TOC		(! TARGET_TOC)

#define TARGET_V4_CALLS		TARGET_NO_AIX_CALLS

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
  { "traceback",	-MASK_NO_TRACEBACK },				\
  { "no-traceback",	 MASK_NO_TRACEBACK },				\
  { "little-endian",	 MASK_LITTLE_ENDIAN },				\
  { "little",		 MASK_LITTLE_ENDIAN },				\
  { "big-endian",	-MASK_LITTLE_ENDIAN },				\
  { "big",		-MASK_LITTLE_ENDIAN },				\
  { "no-toc",		 0 },						\
  { "toc",		 MASK_MINIMAL_TOC },				\
  { "full-toc",		 MASK_MINIMAL_TOC },				\
  { "call-aix",		 MASK_AIX_CALLS },				\
  { "call-sysv",	-MASK_AIX_CALLS },				\
  { "prototype",	 MASK_PROTOTYPE },				\
  { "no-prototype",	-MASK_PROTOTYPE },

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
} while (0)

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

/* The XCOFF support uses weird symbol suffixes, which we don't want
   for ELF.  */

#undef RS6000_OUTPUT_BASENAME
#define RS6000_OUTPUT_BASENAME(FILE, NAME) assemble_name (FILE, NAME)

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
      fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);	\
      if (! toc_initialized)						\
	{								\
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1");	\
	  fprintf (asm_out_file, " = .+32768\n");			\
	  toc_initialized = 1;						\
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

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

extern void svr4_traceback ();
extern int rs6000_pic_labelno;

#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    if (TARGET_RELOCATABLE && get_pool_size () != 0)			\
      {									\
	char buf[256];							\
									\
	ASM_OUTPUT_INTERNAL_LABEL (FILE, "LCL", rs6000_pic_labelno);	\
									\
	ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);			\
	fprintf (FILE, (TARGET_POWERPC64) ? "\t.quad " : "\t.long ");	\
	assemble_name (FILE, buf);					\
	putc ('-', FILE);						\
									\
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
    if (TARGET_TRACEBACK)						\
      svr4_traceback (FILE, NAME, DECL);				\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
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
#define ASM_SPEC \
  "-u \
%{mcpu=601: -m601} %{!mcpu=601: -mppc} \
%{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} \
%{mrelocatable} \
%{mlittle} %{mlittle-endian} %{mbig} %{mbig-endian}"

/* Output .file and comments listing what options there are */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
do {									\
  ASM_OUTPUT_OPTIONS (FILE);						\
  output_file_directive ((FILE), main_input_filename);			\
} while (0)

/* This is the end of what might become sysv4.h.  */

/* Allow stabs and dwarf, prefer dwarf.  */
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
#define	DBX_DEBUGGING_INFO
#define	DWARF_DEBUGGING_INFO

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  Discard
   a leading * */
#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME) \
  (VAR) = ((SYMBOL_NAME) + ((SYMBOL_NAME)[0] == '*'))

/* Like block addresses, stabs line numbers are relative to the
   current function.  */

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)				\
do									\
  {									\
    static int sym_lineno = 1;						\
    char *_p;								\
    fprintf (file, "\t.stabn 68,0,%d,.LM%d-",				\
	     line, sym_lineno);						\
    STRIP_NAME_ENCODING (_p, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
    assemble_name (file, _p);						\
    fprintf (file, "\n.LM%d:\n", sym_lineno);				\
    sym_lineno += 1;							\
  }									\
while (0)

/* But, to make this work, we have to output the stabs for the function
   name *first*...  */

#define	DBX_FUNCTION_FIRST

/* This is the end of what might become sysv4dbx.h.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC System V.4)");


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.

   Unlike AIX, this needs real code.  */

#undef	TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)					\
do {									\
  char *sc = reg_names[STATIC_CHAIN_REGNUM];				\
  char *r0 = reg_names[0];						\
									\
  if (STATIC_CHAIN_REGNUM == 0 || !TARGET_NEW_MNEMONICS)		\
    abort ();								\
									\
  if (TARGET_64BIT)							\
    {									\
      fprintf (FILE, "\tmflr %s\n", r0);		/* offset  0 */	\
      fprintf (FILE, "\tbl .LTRAMP1\n");		/* offset  4 */	\
      fprintf (FILE, "\t.long 0,0,0,0\n");		/* offset  8 */	\
      fprintf (FILE, ".LTRAMP1:\n");					\
      fprintf (FILE, "\tmflr %s\n", sc);		/* offset 28 */	\
      fprintf (FILE, "\tmtlr %s\n", r0);		/* offset 32 */	\
      fprintf (FILE, "\tld %s,0(%s)\n", r0, sc);	/* offset 36 */	\
      fprintf (FILE, "\tld %s,8(%s)\n", sc, sc);	/* offset 40 */	\
      fprintf (FILE, "\tmtctr %s\n", r0);		/* offset 44 */	\
      fprintf (FILE, "\tbctr\n");			/* offset 48 */	\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tmflr %s\n", r0);		/* offset  0 */	\
      fprintf (FILE, "\tbl .LTRAMP1\n");		/* offset  4 */	\
      fprintf (FILE, "\t.long 0,0\n");			/* offset  8 */	\
      fprintf (FILE, ".LTRAMP1:\n");					\
      fprintf (FILE, "\tmflr %s\n", sc);		/* offset 20 */	\
      fprintf (FILE, "\tmtlr %s\n", r0);		/* offset 24 */	\
      fprintf (FILE, "\tlwz %s,0(%s)\n", r0, sc);	/* offset 28 */	\
      fprintf (FILE, "\tlwz %s,4(%s)\n", sc, sc);	/* offset 32 */	\
      fprintf (FILE, "\tmtctr %s\n", r0);		/* offset 36 */	\
      fprintf (FILE, "\tbctr\n");			/* offset 40 */	\
    }									\
} while (0)

/* Length in units of the trampoline for entering a nested function.  */

#undef	TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE    (TARGET_64BIT ? 48 : 40)

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#undef	INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CXT)			\
{									\
  rtx reg = gen_reg_rtx (Pmode);					\
									\
  emit_move_insn (reg, FNADDR);						\
  emit_move_insn (gen_rtx (MEM, Pmode,					\
			   plus_constant (ADDR, 8)),			\
		  reg);							\
  emit_move_insn (gen_rtx (MEM, Pmode,					\
			   plus_constant (ADDR, (TARGET_64BIT ? 16 : 12))), \
		  CXT);							\
  emit_insn (gen_sync_isync (gen_rtx (MEM, BLKmode, ADDR)));		\
}


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

#undef CPP_SPEC
#define CPP_SPEC "\
%{posix: -D_POSIX_SOURCE} \
%{mrelocatable: -D_RELOCATABLE} \
%{mcall-sysv: -D_CALL_SYSV} %{mcall-aix: -D_CALL_AIX} %{!mcall-sysv: %{!mcall-aix: -D_CALL_SYSV}} \
%{msoft-float: -D_SOFT_FLOAT} %{mcpu=403: -D_SOFT_FLOAT} \
%{mlittle: -D_LITTLE_ENDIAN -Amachine(littleendian)} \
%{mlittle-endian: -D_LITTLE_ENDIAN -Amachine(littleendian)} \
%{!mlittle: %{!mlittle-endian: -D_BIG_ENDIAN -Amachine(bigendian)}} \
%{!mcpu*: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{mno-powerpc: %{!mpower: %{!mpower2: -D_ARCH_COM}}} \
  %{!mno-powerpc: -D_ARCH_PPC}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=403: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC}"
