/* Target definitions for PowerPC running Darwin (Mac OS X).
   Copyright (C) 1997, 2000, 2001, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the
   Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Darwin/PowerPC)");

/* The "Darwin ABI" is mostly like AIX, but with some key differences.  */

#define DEFAULT_ABI ABI_DARWIN

/* The object file format is Mach-O.  */

#define TARGET_OBJECT_FORMAT OBJECT_MACHO

/* We're not ever going to do TOCs.  */

#define TARGET_TOC 0
#define TARGET_NO_TOC 1

/* Darwin switches.  */
/* Use dynamic-no-pic codegen (no picbase reg; not suitable for shlibs.)  */
#define MASK_MACHO_DYNAMIC_NO_PIC 0x00800000

#define TARGET_DYNAMIC_NO_PIC	(target_flags & MASK_MACHO_DYNAMIC_NO_PIC)

/* Handle #pragma weak and #pragma pack.  */
#define HANDLE_SYSV_PRAGMA 1


#define TARGET_OS_CPP_BUILTINS()                \
  do                                            \
    {                                           \
      builtin_define ("__ppc__");               \
      builtin_define ("__POWERPC__");           \
      builtin_define ("__NATURAL_ALIGNMENT__"); \
      builtin_define ("__MACH__");              \
      builtin_define ("__APPLE__");             \
    }                                           \
  while (0)


/*  */
#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
  {"dynamic-no-pic",	MASK_MACHO_DYNAMIC_NO_PIC,			\
      N_("Generate code suitable for executables (NOT shared libs)")},	\
  {"no-dynamic-no-pic",	-MASK_MACHO_DYNAMIC_NO_PIC, ""},


/* The Darwin ABI always includes AltiVec, can't be (validly) turned
   off.  */

#define SUBTARGET_OVERRIDE_OPTIONS				  	\
do {									\
  rs6000_altivec_abi = 1;						\
  rs6000_altivec_vrsave = 1;						\
  if (DEFAULT_ABI == ABI_DARWIN)					\
  {									\
    if (MACHO_DYNAMIC_NO_PIC_P)						\
      {									\
        if (flag_pic)							\
            warning ("-mdynamic-no-pic overrides -fpic or -fPIC");	\
        flag_pic = 0;							\
      }									\
    else if (flag_pic == 1)						\
      {									\
        /* Darwin doesn't support -fpic.  */				\
        warning ("-fpic is not supported; -fPIC assumed");		\
        flag_pic = 2;							\
      }									\
  }									\
}while(0)

/* We want -fPIC by default, unless we're using -static to compile for
   the kernel or some such.  */


#define CC1_SPEC "\
%{gused: -feliminate-unused-debug-symbols %<gused }\
%{static: %{Zdynamic: %e conflicting code gen style switches are used}}\
%{!static:%{!mdynamic-no-pic:-fPIC}}"

/* It's virtually impossible to predict all the possible combinations
   of -mcpu and -maltivec and whatnot, so just supply
   -force_cpusubtype_ALL if any are seen.  Radar 3492132 against the
   assembler is asking for a .machine directive so we could get this
   really right.  */
#define ASM_SPEC "-arch ppc \
  %{Zforce_cpusubtype_ALL:-force_cpusubtype_ALL} \
  %{!Zforce_cpusubtype_ALL:%{maltivec|mcpu=*|mpowerpc64:-force_cpusubtype_ALL}}"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS			\
  { "darwin_arch", "ppc" },

/* Make both r2 and r3 available for allocation.  */
#define FIXED_R2 0
#define FIXED_R13 0

/* Base register for access to local variables of the function.  */

#undef  FRAME_POINTER_REGNUM
#define FRAME_POINTER_REGNUM 30

#undef  RS6000_PIC_OFFSET_TABLE_REGNUM
#define RS6000_PIC_OFFSET_TABLE_REGNUM 31

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

/* These are used by -fbranch-probabilities */
#define HOT_TEXT_SECTION_NAME "__TEXT,__text,regular,pure_instructions"
#define UNLIKELY_EXECUTED_TEXT_SECTION_NAME \
                              "__TEXT,__text2,regular,pure_instructions"

/* Define cutoff for using external functions to save floating point.
   Currently on Darwin, always use inline stores.  */

#undef	FP_SAVE_INLINE
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 64)

/* The assembler wants the alternate register names, but without
   leading percent sign.  */
#undef REGISTER_NAMES
#define REGISTER_NAMES							\
{									\
     "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",		\
     "r8",  "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",		\
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",		\
     "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",		\
     "f8",  "f9", "f10", "f11", "f12", "f13", "f14", "f15",		\
    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",		\
    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",		\
     "mq",  "lr", "ctr",  "ap",						\
    "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7",		\
    "xer",								\
     "v0",  "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",             \
     "v8",  "v9", "v10", "v11", "v12", "v13", "v14", "v15",             \
    "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",             \
    "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",             \
    "vrsave", "vscr",							\
    "spe_acc", "spefscr"                                                \
}

/* This outputs NAME to FILE.  */

#undef  RS6000_OUTPUT_BASENAME
#define RS6000_OUTPUT_BASENAME(FILE, NAME)	\
    assemble_name (FILE, NAME)

/* Globalizing directive for a label.  */
#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP "\t.globl "
#undef TARGET_ASM_GLOBALIZE_LABEL

/* This is how to output an internal label prefix.  rs6000.c uses this
   when generating traceback tables.  */
/* Not really used for Darwin?  */

#undef ASM_OUTPUT_INTERNAL_LABEL_PREFIX
#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, "%s", PREFIX)

/* This says how to output an assembler line to define a global common
   symbol.  */
/* ? */
#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  do { fputs (".comm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED"\n",\
		(SIZE)); } while (0)

/* Override the standard rs6000 definition.  */

#undef ASM_COMMENT_START
#define ASM_COMMENT_START ";"

/* FP save and restore routines.  */
#define	SAVE_FP_PREFIX "._savef"
#define SAVE_FP_SUFFIX ""
#define	RESTORE_FP_PREFIX "._restf"
#define RESTORE_FP_SUFFIX ""

/* This is how to output an assembler line that says to advance
   the location counter to a multiple of 2**LOG bytes using the
   "nop" instruction as padding.  */

#define ASM_OUTPUT_ALIGN_WITH_NOP(FILE,LOG)                   \
  do                                                          \
    {                                                         \
      if ((LOG) < 3)                                          \
        {                                                     \
          ASM_OUTPUT_ALIGN (FILE,LOG);                        \
        }                                                     \
      else /* nop == ori r0,r0,0 */                           \
        fprintf (FILE, "\t.align32 %d,0x60000000\n", (LOG));  \
    } while (0)

/* Generate insns to call the profiler.  */

#define PROFILE_HOOK(LABEL)   output_profile_hook (LABEL)

/* Function name to call to do profiling.  */

#define RS6000_MCOUNT "*mcount"

/* Default processor: a G4.  */

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT  PROCESSOR_PPC7400

/* Default target flag settings.  Despite the fact that STMW/LMW
   serializes, it's still a big code size win to use them.  Use FSEL by
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
#define PREFERRED_RELOAD_CLASS(X,CLASS)				\
  ((GET_CODE (X) == CONST_DOUBLE				\
    && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)		\
   ? NO_REGS							\
   : ((GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == HIGH)	\
      && reg_class_subset_p (BASE_REGS, (CLASS)))		\
   ? BASE_REGS							\
   : (GET_MODE_CLASS (GET_MODE (X)) == MODE_INT			\
      && (CLASS) == NON_SPECIAL_REGS)				\
   ? GENERAL_REGS						\
   : (CLASS))

/* Fix for emit_group_load (): force large constants to be pushed via regs.  */
#define ALWAYS_PUSH_CONSTS_USING_REGS_P		1

/* This now supports a natural alignment mode */
/* Darwin word-aligns FP doubles but doubleword-aligns 64-bit ints.  */
#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
  (TARGET_ALIGN_NATURAL ? (COMPUTED) : \
  (TYPE_MODE (TREE_CODE (TREE_TYPE (FIELD)) == ARRAY_TYPE \
	      ? get_inner_array_type (FIELD) \
	      : TREE_TYPE (FIELD)) == DFmode \
   ? MIN ((COMPUTED), 32) : (COMPUTED)))

/* Darwin increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)			\
  ((TREE_CODE (STRUCT) == RECORD_TYPE					\
    || TREE_CODE (STRUCT) == UNION_TYPE					\
    || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)				\
   && TARGET_ALIGN_NATURAL == 0                         		\
   ? rs6000_special_round_type_align (STRUCT, COMPUTED, SPECIFIED)	\
   : (TARGET_ALTIVEC && TREE_CODE (STRUCT) == VECTOR_TYPE) 		\
   ? MAX (MAX ((COMPUTED), (SPECIFIED)), 128)          			 \
   : MAX ((COMPUTED), (SPECIFIED)))

/* XXX: Darwin supports neither .quad, or .llong, but it also doesn't
   support 64 bit PowerPC either, so this just keeps things happy.  */
#define DOUBLE_INT_ASM_OP "\t.quad\t"

/* Get HOST_WIDE_INT and CONST_INT to be 32 bits, for compile time
   space/speed.  */
#undef MAX_LONG_TYPE_SIZE
#define MAX_LONG_TYPE_SIZE 32

/* For binary compatibility with 2.95; Darwin C APIs use bool from
   stdbool.h, which was an int-sized enum in 2.95.  */
#define BOOL_TYPE_SIZE INT_TYPE_SIZE

#undef REGISTER_TARGET_PRAGMAS
#define REGISTER_TARGET_PRAGMAS DARWIN_REGISTER_TARGET_PRAGMAS

