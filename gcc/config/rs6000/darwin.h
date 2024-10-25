/* Target definitions for PowerPC running Darwin (Mac OS X).
   Copyright (C) 1997-2024 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#undef DARWIN_PPC
#define DARWIN_PPC 1

/* The "Darwin ABI" is mostly like AIX, but with some key differences.  */

#define DEFAULT_ABI ABI_DARWIN

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
#ifdef __powerpc64__
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif

/* The object file format is Mach-O.  */

#define TARGET_OBJECT_FORMAT OBJECT_MACHO

/* Size of the Obj-C jump buffer.  */
#define OBJC_JBLEN ((TARGET_64BIT) ? (26*2 + 18*2 + 129 + 1) : (26 + 18*2 + 129 + 1))

/* We're not ever going to do TOCs.  */

#define TARGET_HAS_TOC 0

/* Override the default rs6000 definition.  */
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__POWERPC__");			\
      builtin_define ("__PPC__");			\
      if (TARGET_64BIT)					\
	{						\
	  builtin_define ("__ppc64__");			\
	  builtin_define ("__PPC64__");			\
	  builtin_define ("__powerpc64__");		\
	  builtin_assert ("cpu=powerpc64");		\
	  builtin_assert ("machine=powerpc64");		\
	}						\
      else						\
	{						\
	  builtin_define ("__ppc__");			\
	  builtin_define_std ("PPC");			\
	  builtin_assert ("cpu=powerpc");		\
	  builtin_assert ("machine=powerpc");		\
	}						\
      builtin_define ("__NATURAL_ALIGNMENT__");		\
      darwin_cpp_builtins (pfile);			\
    }							\
  while (0)

#define SUBTARGET_OVERRIDE_OPTIONS darwin_rs6000_override_options ()

#define C_COMMON_OVERRIDE_OPTIONS do {					\
  /* On powerpc, __cxa_get_exception_ptr is available starting in the	\
     10.4.6 libstdc++.dylib.  */					\
  if (strverscmp (darwin_macosx_version_min, "10.4.6") < 0		\
      && flag_use_cxa_get_exception_ptr == 2)				\
    flag_use_cxa_get_exception_ptr = 0;					\
  if (flag_mkernel)							\
    flag_no_builtin = 1;						\
  SUBTARGET_C_COMMON_OVERRIDE_OPTIONS;					\
} while (0)

/* Darwin has 128-bit long double support in libc in 10.4 and later.
   Default to 128-bit long doubles even on earlier platforms for ABI
   consistency; arithmetic will work even if libc and libm support is
   not available.  */

#define RS6000_DEFAULT_LONG_DOUBLE_SIZE 128

/* Machine dependent libraries.
   Include libmx when targeting Darwin 7.0 and above, but before libSystem,
   since the functions are actually in libSystem but for 7.x compatibility
   we want them to be looked for in libmx first.
   Include libSystemStubs when compiling against 10.3 - 10.6 SDKs (we assume
   this is the case when targetting these) - but not for 64-bit long double.
   Don't do either for m64, the library is either a dummy or non-existent.
*/

#undef LIB_SPEC
#define LIB_SPEC \
"%{!static:								\
  %{!m64:%{!mlong-double-64:						\
    %{pg:%:version-compare(>< 10.3 10.7 mmacosx-version-min= -lSystemStubs_profile)} \
    %{!pg:%:version-compare(>< 10.3 10.7 mmacosx-version-min= -lSystemStubs)} \
     %:version-compare(>< 10.3 10.4 mmacosx-version-min= -lmx)}}	\
  -lSystem								\
}"

#undef DARWIN_HEAP_T_LIB
#define DARWIN_HEAP_T_LIB " "

/* We want -fPIC by default, unless we're using -static to compile for
   the kernel or some such.  The "-faltivec" option should have been
   called "-maltivec" all along.  */

#define CC1_SPEC "\
  %(cc1_cpu) \
  %{g: %{!fno-eliminate-unused-debug-symbols: -feliminate-unused-debug-symbols }} \
  %{static: %{Zdynamic: %e conflicting code gen style switches are used}}\
  %{!mkernel:%{!static:%{!mdynamic-no-pic:-fPIC}}} \
  %{faltivec:-maltivec -include altivec.h} %{fno-altivec:-mno-altivec} \
  %<faltivec %<fno-altivec " \
  DARWIN_CC1_SPEC

/* Default to PPC for single arch builds.  */
#define DARWIN_ARCH_SPEC "ppc"

#define DARWIN_SUBARCH_SPEC "			\
 %{mcpu=601:ppc601;				\
   mcpu=603:ppc603;				\
   mcpu=603e:ppc603;				\
   mcpu=604:ppc604;				\
   mcpu=604e:ppc604e;				\
   mcpu=740:ppc750;				\
   mcpu=750:ppc750;				\
   mcpu=G3:ppc750;				\
   mcpu=7400:ppc7400;				\
   mcpu=G4:ppc7400;				\
   mcpu=7450:ppc7450;				\
   mcpu=970:ppc970;				\
   mcpu=power4:ppc970;				\
   mcpu=G5:ppc970;				\
   :ppc}"

/* We need to jam the crt to 10.5 for 10.6 (Rosetta) use.  */
#undef DARWIN_CRT1_SPEC
#define DARWIN_CRT1_SPEC						\
  "%:version-compare(!> 10.5 mmacosx-version-min= -lcrt1.o)		\
   %:version-compare(>< 10.5 10.7 mmacosx-version-min= -lcrt1.10.5.o)	\
   %{fgnu-tm: -lcrttms.o}"

/* crt2.o is at least partially required for 10.3.x and earlier.
   It deals with registration of the unwind frames, where this is not
   automatically provided by the system.  So we need it for any case that
   might use exceptions.  */
#undef DARWIN_CRT2_SPEC
#define DARWIN_CRT2_SPEC \
"%{!m64:%{shared-libgcc|static-libstdc++|fexceptions|fobjc-exceptions|fgnu-runtime: \
   %:version-compare(!> 10.4 mmacosx-version-min= crt2.o%s) \
  }}"

/* crt3 deals with providing cxa_atexit on earlier systems (or fixing it up,
   for broken versions).  It's only needed for c++ code, so we can make it
   conditional on shared-libgcc since that's forced on for c++.  */
#undef DARWIN_CRT3_SPEC
#define DARWIN_CRT3_SPEC \
"%{!m64:%{shared-libgcc|static-libstdc++:							\
   %:version-compare(>< 10.4 10.5 mmacosx-version-min= crt3.o%s) \
   %:version-compare(!> 10.4 mmacosx-version-min= crt3_2.o%s) \
  }}"

/* As for crt1, we need to force the dylib crt for 10.6.  */
#undef DARWIN_DYLIB1_SPEC
#define DARWIN_DYLIB1_SPEC						\
  "%:version-compare(!> 10.5 mmacosx-version-min= -ldylib1.o)		\
   %:version-compare(>< 10.5 10.7 mmacosx-version-min= -ldylib1.10.5.o)"

/* Likewise, the bundle crt.  */
#undef DARWIN_BUNDLE1_SPEC
#define DARWIN_BUNDLE1_SPEC \
"%{!static:%:version-compare(< 10.7 mmacosx-version-min= -lbundle1.o)	\
	   %{fgnu-tm: -lcrttms.o}}"

/* The PPC regs save/restore functions are leaves and could, conceivably
   be used by the tm destructor.  */
#undef ENDFILE_SPEC
#define ENDFILE_SPEC TM_DESTRUCTOR " -lef_ppc"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS			\
  DARWIN_EXTRA_SPECS                            \
  { "darwin_arch", DARWIN_ARCH_SPEC },		\
  { "darwin_crt2", DARWIN_CRT2_SPEC },		\
  { "darwin_subarch", DARWIN_SUBARCH_SPEC },

/* Output a .machine directive.  */
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START rs6000_darwin_file_start

/* Make both r2 and r13 available for allocation.  */
#define FIXED_R2 0
#define FIXED_R13 TARGET_64BIT

/* Base register for access to local variables of the function.  */

#undef  HARD_FRAME_POINTER_REGNUM
#define HARD_FRAME_POINTER_REGNUM 30

#undef  RS6000_PIC_OFFSET_TABLE_REGNUM
#define RS6000_PIC_OFFSET_TABLE_REGNUM 31

#undef FIRST_SAVED_GP_REGNO
#define FIRST_SAVED_GP_REGNO 13

/* Darwin's stack must remain 16-byte aligned for both 32 and 64 bit
   ABIs.  */

#undef  STACK_BOUNDARY
#define STACK_BOUNDARY 128

/* Offset within stack frame to start allocating local variables at.
   For supported Darwin versions, FRAME_GROWS_DOWNWARD is true, therefore
   this value is the offset to the END of the first local allocated.

   On the RS/6000, the frame pointer is the same as the stack pointer,
   except for dynamic allocations.  So we start after the fixed area and
   outgoing parameter area.

   If the function uses dynamic stack space (CALLS_ALLOCA is set), that
   space needs to be aligned to STACK_BOUNDARY, i.e. the sum of the
   sizes of the fixed area and the parameter area must be a multiple of
   STACK_BOUNDARY.  */

#undef RS6000_STARTING_FRAME_OFFSET
#define RS6000_STARTING_FRAME_OFFSET					\
  (cfun->calls_alloca							\
   ? RS6000_ALIGN (crtl->outgoing_args_size + RS6000_SAVE_AREA, 16)	\
   : (RS6000_ALIGN (crtl->outgoing_args_size, 16) + RS6000_SAVE_AREA))

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.cc' for details.

   This value must be a multiple of STACK_BOUNDARY (hard coded in
   `emit-rtl.cc').  */

#undef STACK_DYNAMIC_OFFSET
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
  RS6000_ALIGN (crtl->outgoing_args_size.to_constant()			\
		+ STACK_POINTER_OFFSET, 16)

/* Darwin uses a function call if everything needs to be saved/restored.  */

#undef WORLD_SAVE_P
#define WORLD_SAVE_P(INFO) ((INFO)->world_save_p)

/* We don't use these on Darwin, they are just place-holders.  */
#define SAVE_FP_PREFIX ""
#define SAVE_FP_SUFFIX ""
#define RESTORE_FP_PREFIX ""
#define RESTORE_FP_SUFFIX ""

/* The assembler wants the alternate register names, but without
   leading percent sign.  */
#undef REGISTER_NAMES
#define REGISTER_NAMES							\
{									\
  /* GPRs */								\
     "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",		\
     "r8",  "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",		\
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",		\
  /* FPRs */								\
     "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",		\
     "f8",  "f9", "f10", "f11", "f12", "f13", "f14", "f15",		\
    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",		\
    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",		\
  /* VRs */								\
     "v0",  "v1",  "v2",  "v3",  "v4",  "v5",  "v6",  "v7",		\
     "v8",  "v9", "v10", "v11", "v12", "v13", "v14", "v15",		\
    "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23",		\
    "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31",		\
  /* lr ctr ca ap */							\
     "lr", "ctr", "xer",  "ap",						\
  /* cr0..cr7 */							\
    "cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7",		\
  /* vrsave vscr sfp */							\
    "vrsave", "vscr", "sfp"						\
}

/* This outputs NAME to FILE.  */

#undef  RS6000_OUTPUT_BASENAME
#define RS6000_OUTPUT_BASENAME(FILE, NAME)	\
    assemble_name (FILE, NAME)

/* Globalizing directive for a label.  */
#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP "\t.globl "
#undef TARGET_ASM_GLOBALIZE_LABEL

/* This is how to output an internal label prefix.  rs6000.cc uses this
   when generating traceback tables.  */
/* Not really used for Darwin?  */

#undef ASM_OUTPUT_INTERNAL_LABEL_PREFIX
#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  fprintf (FILE, "%s", PREFIX)

/* Override the standard rs6000 definition.  */

#undef ASM_COMMENT_START
#define ASM_COMMENT_START ";"

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

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
/* This is supported in cctools 465 and later.  The macro test
   above prevents using it in earlier build environments.  */
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)          \
  if ((LOG) > 0)                                             \
    {                                                         \
      if ((MAX_SKIP) <= 0)                                    \
        fprintf ((FILE), "\t.p2align %d\n", (LOG));           \
      else                                                    \
        fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP)); \
    }
#endif

/* Generate insns to call the profiler.  */

#define PROFILE_HOOK(LABEL)   output_profile_hook (LABEL)

/* Function name to call to do profiling.  */

#define RS6000_MCOUNT "*mcount"

/* Default processor: G4, and G5 for 64-bit.  */

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT  PROCESSOR_PPC7400
#undef PROCESSOR_DEFAULT64
#define PROCESSOR_DEFAULT64  PROCESSOR_POWER4

/* Default target flag settings.  Despite the fact that STMW/LMW
   serializes, it's still a big code size win to use them.  Use FSEL by
   default as well.  */

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (OPTION_MASK_MULTIPLE | OPTION_MASK_PPC_GFXOPT)

/* Darwin always uses IBM long double, never IEEE long double.  */
#undef  TARGET_IEEEQUAD
#define TARGET_IEEEQUAD 0

#undef  TARGET_IEEEQUAD_DEFAULT
#define TARGET_IEEEQUAD_DEFAULT 0

/* Since Darwin doesn't do TOCs, stub this out.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)  ((void)X, (void)MODE, 0)

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
  ((CONSTANT_P (X)						\
    && reg_classes_intersect_p ((CLASS), FLOAT_REGS))		\
   ? NO_REGS							\
   : ((SYMBOL_REF_P (X) || GET_CODE (X) == HIGH)		\
      && reg_class_subset_p (BASE_REGS, (CLASS)))		\
   ? BASE_REGS							\
   : (GET_MODE_CLASS (GET_MODE (X)) == MODE_INT			\
      && (CLASS) == GEN_OR_FLOAT_REGS)				\
   ? GENERAL_REGS						\
   : (CLASS))

/* Compute field alignment.
   This implements the 'power' alignment rule by pegging the alignment of
   items (beyond the first aggregate field) to 32 bits.  The pegging is
   suppressed for vector and long double items (both 128 in size).
   There is a dummy use of the FIELD argument to avoid an unused variable
   warning (see PR59496).  */
#define ADJUST_FIELD_ALIGN(FIELD, TYPE, COMPUTED)		\
  ((void) (FIELD),						\
    (TARGET_ALIGN_NATURAL					\
     ? (COMPUTED)						\
     : (COMPUTED) == 128					\
	? 128							\
	: MIN ((COMPUTED), 32)))

/* Darwin increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)			  \
  (RECORD_OR_UNION_TYPE_P (STRUCT)				  \
   && TARGET_ALIGN_NATURAL == 0						  \
   ? darwin_rs6000_special_round_type_align (STRUCT, COMPUTED, SPECIFIED) \
   : (VECTOR_TYPE_P (STRUCT)					  \
      && ALTIVEC_VECTOR_MODE (TYPE_MODE (STRUCT)))			  \
   ? MAX (MAX ((COMPUTED), (SPECIFIED)), 128)				  \
   : MAX ((COMPUTED), (SPECIFIED)))

/* Specify padding for the last element of a block move between
   registers and memory.  FIRST is nonzero if this is the only
   element.  */
#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (!(FIRST) ? PAD_UPWARD : targetm.calls.function_arg_padding (MODE, TYPE))

#define DOUBLE_INT_ASM_OP "\t.quad\t"

/* For binary compatibility with 2.95; Darwin C APIs use bool from
   stdbool.h, which was an int-sized enum in 2.95.  Users can explicitly
   choose to have sizeof(bool)==1 with the -mone-byte-bool switch. */
#define BOOL_TYPE_SIZE (darwin_one_byte_bool ? CHAR_TYPE_SIZE : INT_TYPE_SIZE)

#undef REGISTER_TARGET_PRAGMAS
#define REGISTER_TARGET_PRAGMAS() \
  do \
    { \
      DARWIN_REGISTER_TARGET_PRAGMAS(); \
      targetm.target_option.pragma_parse = rs6000_pragma_target_parse; \
      targetm.resolve_overloaded_builtin = altivec_resolve_overloaded_builtin; \
    } \
  while (0)

#ifdef IN_LIBGCC2
#include <stdbool.h>
#endif

/* True, iff we're generating fast turn around debugging code.  When
   true, we arrange for function prologues to start with 5 nops so
   that gdb may insert code to redirect them, and for data to be
   accessed indirectly.  The runtime uses this indirection to forward
   references for data to the original instance of that data.  */

#define TARGET_FIX_AND_CONTINUE (darwin_fix_and_continue)

/* This is the reserved direct dispatch address for Objective-C.  */
#define OFFS_MSGSEND_FAST		0xFFFEFF00

/* This is the reserved ivar address Objective-C.  */
#define OFFS_ASSIGNIVAR_FAST		0xFFFEFEC0

/* Old versions of Mac OS/Darwin don't have C99 functions available.  */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION darwin_libc_has_function

/* When generating kernel code or kexts, we don't use Altivec by
   default, as kernel code doesn't save/restore those registers.  */
#define OS_MISSING_ALTIVEC (flag_mkernel || flag_apple_kext)

/* Darwin has support for section anchors on powerpc*.
   It is disabled for any section containing a "zero-sized item" (because these
   are re-written as size=1 to be compatible with the OSX ld64).
   The re-writing would interfere with the computation of anchor offsets.
   Therefore, we place zero-sized items in their own sections and make such
   sections unavailable to section anchoring.  */

#undef TARGET_ASM_OUTPUT_ANCHOR
#define TARGET_ASM_OUTPUT_ANCHOR darwin_asm_output_anchor

#undef TARGET_USE_ANCHORS_FOR_SYMBOL_P
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P darwin_use_anchors_for_symbol_p

#undef DARWIN_SECTION_ANCHORS
#define DARWIN_SECTION_ANCHORS 1

/* PPC Darwin has to rename some of the long double builtins.  */
#undef  SUBTARGET_INIT_BUILTINS
#define SUBTARGET_INIT_BUILTINS						\
do {									\
  darwin_patch_builtins ();						\
  rs6000_builtin_decls[(unsigned) (RS6000_BIF_CFSTRING)]		\
    = darwin_init_cfstring_builtins ((unsigned) (RS6000_BIF_CFSTRING)); \
} while(0)

/* So far, there is no rs6000_fold_builtin, if one is introduced, then
   this will need to be modified similar to the x86 case.  */
#define TARGET_FOLD_BUILTIN SUBTARGET_FOLD_BUILTIN

/* First available SYMBOL flag bit for use by subtargets.  */
#define SYMBOL_FLAG_SUBT_DEP (SYMBOL_FLAG_MACH_DEP)

/* Use standard DWARF numbering for DWARF debugging information.  */
#define RS6000_USE_DWARF_NUMBERING

