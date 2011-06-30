/* Target definitions for x86 running Darwin.
   Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Enable Mach-O bits in generic x86 code.  */
#undef TARGET_MACHO
#define TARGET_MACHO 1

#define TARGET_VERSION fprintf (stderr, " (i686 Darwin)");

#undef  TARGET_64BIT
#define TARGET_64BIT OPTION_ISA_64BIT

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
#ifdef __x86_64__
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif

/* Size of the Obj-C jump buffer.  */
#define OBJC_JBLEN ((TARGET_64BIT) ? ((9 * 2) + 3 + 16) : (18))

#undef TARGET_FPMATH_DEFAULT
#define TARGET_FPMATH_DEFAULT (TARGET_SSE ? FPMATH_SSE : FPMATH_387)

#define TARGET_OS_CPP_BUILTINS()                \
  do                                            \
    {                                           \
      builtin_define ("__LITTLE_ENDIAN__");     \
      darwin_cpp_builtins (pfile);		\
    }                                           \
  while (0)

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD 64

#undef FORCE_PREFERRED_STACK_BOUNDARY_IN_MAIN
#define FORCE_PREFERRED_STACK_BOUNDARY_IN_MAIN (0)

#undef TARGET_KEEPS_VECTOR_ALIGNED_STACK
#define TARGET_KEEPS_VECTOR_ALIGNED_STACK 1

/* On Darwin, the stack is 128-bit aligned at the point of every call.
   Failure to ensure this will lead to a crash in the system libraries
   or dynamic loader.  */
#undef STACK_BOUNDARY
#define STACK_BOUNDARY \
 ((profile_flag || (TARGET_64BIT && ix86_abi == MS_ABI)) \
  ? 128 : BITS_PER_WORD)

#undef MAIN_STACK_BOUNDARY
#define MAIN_STACK_BOUNDARY 128

/* Since we'll never want a stack boundary less aligned than 128 bits
   we need the extra work here otherwise bits of gcc get very grumpy
   when we ask for lower alignment.  We could just reject values less
   than 128 bits for Darwin, but it's easier to up the alignment if
   it's below the minimum.  */
#undef PREFERRED_STACK_BOUNDARY
#define PREFERRED_STACK_BOUNDARY			\
  MAX (128, ix86_preferred_stack_boundary)

/* We want -fPIC by default, unless we're using -static to compile for
   the kernel or some such.  */

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) \
  %<mdynamic-no-pic " /* For now, we just ignore this flag */ " \
  %{!mkernel:%{!static:%{!mdynamic-no-pic:-fPIC}}} \
  %{!mmacosx-version-min=*:-mmacosx-version-min=%(darwin_minversion)} \
  %{g: %{!fno-eliminate-unused-debug-symbols: -feliminate-unused-debug-symbols }}"

#undef ASM_SPEC
#define ASM_SPEC "-arch %(darwin_arch) -force_cpusubtype_ALL"

#define DARWIN_ARCH_SPEC "%{m64:x86_64;:i386}"
#define DARWIN_SUBARCH_SPEC DARWIN_ARCH_SPEC

/* Determine a minimum version based on compiler options.  */
#define DARWIN_MINVERSION_SPEC				\
 "%{!m64|fgnu-runtime:10.4;				\
    ,objective-c|,objc-cpp-output:10.5;			\
    ,objective-c-header:10.5;				\
    ,objective-c++|,objective-c++-cpp-output:10.5;	\
    ,objective-c++-header|,objc++-cpp-output:10.5;	\
    :10.4}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %{mpc32:crtprec32.o%s} \
   %{mpc64:crtprec64.o%s} \
   %{mpc80:crtprec80.o%s}"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS                                   \
  DARWIN_EXTRA_SPECS                                            \
  { "darwin_arch", DARWIN_ARCH_SPEC },                          \
  { "darwin_crt2", "" },                                        \
  { "darwin_subarch", DARWIN_SUBARCH_SPEC },

/* Use the following macro for any Darwin/x86-specific command-line option
   translation.  */
#define SUBTARGET_OPTION_TRANSLATE_TABLE \
  { "", "" }

/* The Darwin assembler mostly follows AT&T syntax.  */
#undef ASSEMBLER_DIALECT
#define ASSEMBLER_DIALECT ASM_ATT

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.  This macro controls what to do: by default, don't
   print %cl.  */

#define SHIFT_DOUBLE_OMITS_COUNT 0

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END darwin_file_end

/* Define the syntax of pseudo-ops, labels and comments.  */

/* String containing the assembler's comment-starter.  */

#define ASM_COMMENT_START "#"

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387.  */

#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_128BIT_LONG_DOUBLE)

/* For darwin we want to target specific processor features as a minimum,
   but these unfortunately don't correspond to a specific processor.  */
#undef TARGET_SUBTARGET32_ISA_DEFAULT
#define TARGET_SUBTARGET32_ISA_DEFAULT (OPTION_MASK_ISA_MMX		\
					| OPTION_MASK_ISA_SSE		\
					| OPTION_MASK_ISA_SSE2		\
					| OPTION_MASK_ISA_SSE3)

#undef TARGET_SUBTARGET64_ISA_DEFAULT
#define TARGET_SUBTARGET64_ISA_DEFAULT TARGET_SUBTARGET32_ISA_DEFAULT

/* For now, disable dynamic-no-pic.  We'll need to go through i386.c
   with a fine-tooth comb looking for refs to flag_pic!  */
#define MASK_MACHO_DYNAMIC_NO_PIC 0
#define TARGET_DYNAMIC_NO_PIC	  (target_flags & MASK_MACHO_DYNAMIC_NO_PIC)

#undef GOT_SYMBOL_NAME
#define GOT_SYMBOL_NAME MACHOPIC_FUNCTION_BASE_NAME

/* Define the syntax of pseudo-ops, labels and comments.  */

#define LPREFIX "L"

/* These are used by -fbranch-probabilities */
#define HOT_TEXT_SECTION_NAME "__TEXT,__text,regular,pure_instructions"
#define UNLIKELY_EXECUTED_TEXT_SECTION_NAME \
                              "__TEXT,__unlikely,regular,pure_instructions"

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_BYTE "\t.byte\t"
#define ASM_SHORT "\t.word\t"
#define ASM_LONG "\t.long\t"
#define ASM_QUAD "\t.quad\t"

#define SUBTARGET_ENCODE_SECTION_INFO  darwin_encode_section_info

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
 do { if ((LOG) != 0)			\
        {				\
          if (in_section == text_section) \
            fprintf (FILE, "\t%s %d,0x90\n", ALIGN_ASM_OP, (LOG)); \
          else				\
            fprintf (FILE, "\t%s %d\n", ALIGN_ASM_OP, (LOG)); \
        }				\
    } while (0)

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED"\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED"\n", (ROUNDED)))

/* Darwin profiling -- call mcount.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
    do {								\
      if (MACHOPIC_INDIRECT && !TARGET_64BIT)				\
	{								\
	  const char *name = machopic_mcount_stub_name ();		\
	  fprintf (FILE, "\tcall %s\n", name+1);  /*  skip '&'  */	\
	  machopic_validate_stub_or_non_lazy_ptr (name);		\
	}								\
      else fprintf (FILE, "\tcall mcount\n");				\
    } while (0)

#define C_COMMON_OVERRIDE_OPTIONS					\
  do {									\
    SUBTARGET_C_COMMON_OVERRIDE_OPTIONS;				\
  } while (0)

/* Darwin on x86_64 uses dwarf-2 by default.  Pre-darwin9 32-bit
   compiles default to stabs+.  darwin9+ defaults to dwarf-2.  */
#ifndef DARWIN_PREFER_DWARF
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE (TARGET_64BIT ? DWARF2_DEBUG : DBX_DEBUG)
#endif

/* Darwin uses the standard DWARF register numbers but the default
   register numbers for STABS.  Fortunately for 64-bit code the
   default and the standard are the same.  */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) 					\
  (TARGET_64BIT ? dbx64_register_map[n]				\
   : write_symbols == DWARF2_DEBUG ? svr4_dbx_register_map[n]	\
   : dbx_register_map[n])

/* Unfortunately, the 32-bit EH information also doesn't use the standard
   DWARF register numbers.  */
#define DWARF2_FRAME_REG_OUT(n, for_eh)					\
  (! (for_eh) || write_symbols != DWARF2_DEBUG || TARGET_64BIT ? (n)	\
   : (n) == 5 ? 4							\
   : (n) == 4 ? 5							\
   : (n) >= 11 && (n) <= 18 ? (n) + 1					\
   : (n))

#undef REGISTER_SUBTARGET_PRAGMAS
#define REGISTER_SUBTARGET_PRAGMAS() DARWIN_REGISTER_TARGET_PRAGMAS()

#undef TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES darwin_set_default_type_attributes

/* For 64-bit, we need to add 4 because @GOTPCREL is relative to the
   end of the instruction, but without the 4 we'd only have the right
   address for the start of the instruction.  */
#undef ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX
#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(FILE, ENCODING, SIZE, ADDR, DONE)	\
  if (TARGET_64BIT)				                                \
    {                                                                           \
      if ((SIZE) == 4 && ((ENCODING) & 0x70) == DW_EH_PE_pcrel)			\
        {                                                                       \
	   fputs (ASM_LONG, FILE);                                              \
	   assemble_name (FILE, XSTR (ADDR, 0));				\
	   fputs ("+4@GOTPCREL", FILE);                                         \
	   goto DONE;                                                           \
        }									\
    }										\
  else                                                                          \
    {										\
      if (ENCODING == ASM_PREFERRED_EH_DATA_FORMAT (2, 1))                      \
        {                                                                       \
          darwin_non_lazy_pcrel (FILE, ADDR);                                   \
          goto DONE;								\
        }                                                                       \
    }

/* This needs to move since i386 uses the first flag and other flags are
   used in Mach-O.  */
#undef MACHO_SYMBOL_FLAG_VARIABLE
#define MACHO_SYMBOL_FLAG_VARIABLE ((SYMBOL_FLAG_MACH_DEP) << 3)

#define SUBTARGET32_DEFAULT_CPU "i686"

#define SUBTARGET_INIT_BUILTINS					\
do {								\
  darwin_rename_builtins ();					\
} while(0)
 
/* The system ___divdc3 routine in libSystem on darwin10 is not
   accurate to 1ulp, ours is, so we avoid ever using the system name
   for this routine and instead install a non-conflicting name that is
   accurate.  See darwin_rename_builtins.  */
#ifdef L_divdc3
#define DECLARE_LIBRARY_RENAMES \
  asm(".text; ___divdc3: jmp ___ieee_divdc3 ; .globl ___divdc3");
#endif
