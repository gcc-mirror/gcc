/* Target definitions for PowerPC running Darwin (Mac OS X).
   Copyright (C) 1997, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

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

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Darwin/PowerPC)");

/* The "Darwin ABI" is mostly like AIX, but with some key differences.  */

#define DEFAULT_ABI ABI_DARWIN

/* The object file format is Mach-O.  */

#define TARGET_OBJECT_FORMAT OBJECT_MACHO

/* We're not ever going to do TOCs.  */

#define TARGET_TOC 0
#define TARGET_NO_TOC 1

/* The Darwin ABI always includes AltiVec, can't be (validly) turned
   off.  */

#define SUBTARGET_OVERRIDE_OPTIONS  \
  rs6000_altivec_abi = 1;

#define CPP_PREDEFINES "-D__ppc__ -D__POWERPC__ -D__NATURAL_ALIGNMENT__ -D__MACH__ -D__BIG_ENDIAN__ -D__APPLE__"

/* We want -fPIC by default, unless we're using -static to compile for
   the kernel or some such.  */

#define CC1_SPEC "%{!static:-fPIC}"

/* Make both r2 and r3 available for allocation.  */
#define FIXED_R2 0
#define FIXED_R13 0

/* Base register for access to local variables of the function.  */

#undef  FRAME_POINTER_REGNUM
#define FRAME_POINTER_REGNUM 30

#undef  PIC_OFFSET_TABLE_REGNUM
#define PIC_OFFSET_TABLE_REGNUM 31

/* Pad the outgoing args area to 16 bytes instead of the usual 8.  */

#undef STARTING_FRAME_OFFSET
#define STARTING_FRAME_OFFSET						\
  (RS6000_ALIGN (current_function_outgoing_args_size, 16)		\
   + RS6000_VARARGS_AREA						\
   + RS6000_SAVE_AREA)

#undef STACK_DYNAMIC_OFFSET
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
  (RS6000_ALIGN (current_function_outgoing_args_size, 16)		\
   + (STACK_POINTER_OFFSET))

/* Define cutoff for using external functions to save floating point.
   Currently on Darwin, always use inline stores.  */

#undef	FP_SAVE_INLINE
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 64)

/* Always use the "debug" register names, they're what the assembler
   wants to see.  */

#undef REGISTER_NAMES
#define REGISTER_NAMES DEBUG_REGISTER_NAMES

/* This outputs NAME to FILE.  */

#undef  RS6000_OUTPUT_BASENAME
#define RS6000_OUTPUT_BASENAME(FILE, NAME)	\
    assemble_name (FILE, NAME);

/* Output before instructions.  */
/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#undef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.globl ", FILE);	\
       RS6000_OUTPUT_BASENAME (FILE, NAME); putc ('\n', FILE);} while (0)

/* This is how to output an internal label prefix.  rs6000.c uses this
   when generating traceback tables.  */
/* Not really used for Darwin?  */

#undef ASM_OUTPUT_INTERNAL_LABEL_PREFIX
#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, "%s", PREFIX)

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP ".data"

/* This says how to output an assembler line to define a global common
   symbol.  */
/* ? */
#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  do { fputs (".comm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       fprintf ((FILE), ",%d\n", (SIZE)); } while (0)

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", SIZE)

/* Override the standard rs6000 definition.  */

#undef ASM_COMMENT_START
#define ASM_COMMENT_START ";"

/* FP save and restore routines.  */
#define	SAVE_FP_PREFIX "._savef"
#define SAVE_FP_SUFFIX ""
#define	RESTORE_FP_PREFIX "._restf"
#define RESTORE_FP_SUFFIX ""

/* Generate insns to call the profiler.  */

#define PROFILE_HOOK(LABEL)   output_profile_hook (LABEL)

/* Function name to call to do profiling.  */

#define RS6000_MCOUNT "*mcount"

/* Default processor: a G4.  */

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT  PROCESSOR_PPC7400

/* Default target flag settings.  Despite the fact that STMW/LMW
   serializes, it's still a big codesize win to use them.  Use FSEL by
   default as well.  */

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_MULTIPLE | MASK_NEW_MNEMONICS \
                      | MASK_PPC_GFXOPT)

/* Since Darwin doesn't do TOCs, stub this out.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)  0

/* Unlike most other PowerPC targets, chars are signed, for
   consistency with other Darwin architectures.  */

#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR (1)

/* Given an rtx X being reloaded into a reg required to be      
   in class CLASS, return the class of reg to actually use.     
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
  
   On the RS/6000, we have to return NO_REGS when we want to reload a
   floating-point CONST_DOUBLE to force it to be copied to memory.

   Don't allow R0 when loading the address of, or otherwise furtling with,
   a SYMBOL_REF.  */

#undef PREFERRED_RELOAD_CLASS
#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  (((GET_CODE (X) == CONST_DOUBLE			\
    && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)	\
   ? NO_REGS						\
   : (GET_MODE_CLASS (GET_MODE (X)) == MODE_INT 	\
      && (CLASS) == NON_SPECIAL_REGS)			\
   ? GENERAL_REGS					\
   : (GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == HIGH)	\
   ? BASE_REGS						\
   : (CLASS)))

/* Fix for emit_group_load (): force large constants to be pushed via regs.  */
#define ALWAYS_PUSH_CONSTS_USING_REGS_P		1

/* Darwin word-aligns FP doubles but doubleword-aligns 64-bit ints.  */
#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
  (TYPE_MODE (TREE_CODE (TREE_TYPE (FIELD)) == ARRAY_TYPE \
	      ? get_inner_array_type (FIELD) \
	      : TREE_TYPE (FIELD)) == DFmode \
   ? MIN ((COMPUTED), 32) : (COMPUTED))

/* Darwin increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)	\
  ((TREE_CODE (STRUCT) == RECORD_TYPE			\
    || TREE_CODE (STRUCT) == UNION_TYPE			\
    || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)		\
   && TYPE_FIELDS (STRUCT) != 0				\
   && DECL_MODE (TYPE_FIELDS (STRUCT)) == DFmode	\
   ? MAX (MAX ((COMPUTED), (SPECIFIED)), 64)		\
   : MAX ((COMPUTED), (SPECIFIED)))
/* XXX: Darwin supports neither .quad, or .llong, but it also doesn't
   support 64 bit powerpc either, so this just keeps things happy.  */
#define DOUBLE_INT_ASM_OP "\t.quad\t"

/* Get HOST_WIDE_INT and CONST_INT to be 32 bits, for compile time
   space/speed.  */
#undef MAX_LONG_TYPE_SIZE
#define MAX_LONG_TYPE_SIZE 32
