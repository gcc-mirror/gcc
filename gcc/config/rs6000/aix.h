/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.

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

/* Yes!  We are AIX!  */
#define DEFAULT_ABI ABI_AIX
#undef TARGET_AIX
#define TARGET_AIX 1
/* The AIX linker will discard static constructors in object files before
   collect has a chance to see them, so scan the object files directly.  */
#define COLLECT_EXPORT_LIST

/* This is the only version of nm that collect2 can work with.  */
#define REAL_NM_FILE_NAME "/usr/ucb/nm"

#define USER_LABEL_PREFIX  ""
/* Don't turn -B into -L if the argument specifies a relative file name.  */
#define RELATIVE_PREFIX_NOT_LINKDIR

/* Because of the above, we must have gcc search itself to find libgcc.a.  */
#define LINK_LIBGCC_SPECIAL_1

/* Names to predefine in the preprocessor for this target machine.  */
#define CPP_PREDEFINES "-D_IBMR2 -D_POWER -D_AIX -D_AIX32 -D_LONG_LONG \
-Asystem=unix -Asystem=aix -Acpu=rs6000 -Amachine=rs6000"

/* Define appropriate architecture macros for preprocessor depending on
   target switches.  */

#define CPP_SPEC "%{posix: -D_POSIX_SOURCE}\
   %{ansi: -D_ANSI_C_SOURCE}\
   %(cpp_cpu)"

#undef CPP_DEFAULT_SPEC
#define CPP_DEFAULT_SPEC "-D_ARCH_PWR"

#undef ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC ""

/* Tell the assembler to assume that all undefined names are external.

   Don't do this until the fixed IBM assembler is more generally available.
   When this becomes permanently defined, the ASM_OUTPUT_EXTERNAL,
   ASM_OUTPUT_EXTERNAL_LIBCALL, and RS6000_OUTPUT_BASENAME macros will no
   longer be needed.  Also, the extern declaration of mcount in ASM_FILE_START
   will no longer be needed.  */

/* #define ASM_SPEC "-u %(asm_cpu)" */

/* Default location of syscalls.exp under AIX */
#ifndef CROSS_COMPILE
#define LINK_SYSCALLS_SPEC "-bI:/lib/syscalls.exp"
#else
#define LINK_SYSCALLS_SPEC ""
#endif

/* Default location of libg.exp under AIX */
#ifndef CROSS_COMPILE
#define LINK_LIBG_SPEC "-bexport:/usr/lib/libg.exp"
#else
#define LINK_LIBG_SPEC ""
#endif

/* Define the options for the binder: Start text at 512, align all segments
   to 512 bytes, and warn if there is text relocation.

   The -bhalt:4 option supposedly changes the level at which ld will abort,
   but it also suppresses warnings about multiply defined symbols and is
   used by the AIX cc command.  So we use it here.

   -bnodelcsect undoes a poor choice of default relating to multiply-defined
   csects.  See AIX documentation for more information about this.

   -bM:SRE tells the linker that the output file is Shared REusable.  Note
   that to actually build a shared library you will also need to specify an
   export list with the -Wl,-bE option.  */

#define LINK_SPEC "-T512 -H512 %{!r:-btextro} -bhalt:4 -bnodelcsect\
%{static:-bnso %(link_syscalls) } \
%{!shared:%{g*: %(link_libg) }} %{shared:-bM:SRE}"

/* Profiled library versions are used by linking with special directories.  */
#define LIB_SPEC "%{pg:-L/lib/profiled -L/usr/lib/profiled}\
%{p:-L/lib/profiled -L/usr/lib/profiled} %{!shared:%{g*:-lg}} -lc"

/* AIX word-aligns FP doubles but doubleword-aligns 64-bit ints.  */
#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
  (TYPE_MODE (TREE_CODE (TREE_TYPE (FIELD)) == ARRAY_TYPE \
	      ? get_inner_array_type (FIELD) \
	      : TREE_TYPE (FIELD)) == DFmode \
   ? MIN ((COMPUTED), 32) : (COMPUTED))

/* AIX increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)	\
  ((TREE_CODE (STRUCT) == RECORD_TYPE			\
    || TREE_CODE (STRUCT) == UNION_TYPE			\
    || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)		\
   && TYPE_FIELDS (STRUCT) != 0				\
   && DECL_MODE (TYPE_FIELDS (STRUCT)) == DFmode	\
   ? MAX (MAX ((COMPUTED), (SPECIFIED)), 64)		\
   : MAX ((COMPUTED), (SPECIFIED)))



/* Indicate that jump tables go in the text section.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Enable AIX XL compiler calling convention breakage compatibility.  */
#undef TARGET_XL_CALL
#define MASK_XL_CALL		0x40000000
#define	TARGET_XL_CALL		(target_flags & MASK_XL_CALL)
#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES		\
  {"xl-call", 		MASK_XL_CALL,					\
   N_("Always pass floating-point arguments in memory") },		\
  {"no-xl-call",	- MASK_XL_CALL,					\
   N_("Don't always pass floating-point arguments in memory") },	\
  SUBSUBTARGET_SWITCHES
#define SUBSUBTARGET_SWITCHES 

/* Define any extra SPECS that the compiler needs to generate.  */
#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "link_syscalls",            LINK_SYSCALLS_SPEC },			\
  { "link_libg",                LINK_LIBG_SPEC }

/* Define cutoff for using external functions to save floating point.  */
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) == 62 || (FIRST_REG) == 63)

/* Optabs entries for the int->float routines and quad FP operations
   using the standard AIX names.  */
#define ADDTF3_LIBCALL "_xlqadd"
#define DIVTF3_LIBCALL "_xlqdiv"
#define MULTF3_LIBCALL "_xlqmul"
#define SUBTF3_LIBCALL "_xlqsub"

#define INIT_TARGET_OPTABS						\
  do {									\
    if (! TARGET_POWER2 && ! TARGET_POWERPC && TARGET_HARD_FLOAT)	\
      {									\
	fixdfsi_libfunc = init_one_libfunc (RS6000_ITRUNC);		\
	fixunsdfsi_libfunc = init_one_libfunc (RS6000_UITRUNC);		\
      }									\
    if (TARGET_HARD_FLOAT)						\
      {									\
	add_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (ADDTF3_LIBCALL);				\
	sub_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (SUBTF3_LIBCALL);				\
	smul_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (MULTF3_LIBCALL);				\
	sdiv_optab->handlers[(int) TFmode].libfunc			\
	  = init_one_libfunc (DIVTF3_LIBCALL);				\
      }									\
  } while (0)

/* AIX always has a TOC.  */
#define TARGET_NO_TOC		0
#define	TARGET_TOC		1

#define FIXED_R2 1
/* AIX allows r13 to be used.  */
#define FIXED_R13 0

/* __throw will restore its own return address to be the same as the
   return address of the function that the throw is being made to.
   This is unfortunate, because we want to check the original
   return address to see if we need to restore the TOC.
   So we have to squirrel it away with this.  */
#define SETUP_FRAME_ADDRESSES() rs6000_aix_emit_builtin_unwind_init ()

#define PROFILE_HOOK(LABEL)   output_profile_hook (LABEL)

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION ;
