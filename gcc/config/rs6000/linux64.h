/* Definitions of target machine for GNU compiler,
   for 64 bit PowerPC linux.
   Copyright (C) 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

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

#ifndef RS6000_BI_ARCH

#undef	DEFAULT_ABI
#define	DEFAULT_ABI ABI_AIX

#undef	TARGET_64BIT
#define	TARGET_64BIT 1

#define	DEFAULT_ARCH64_P 1
#define	RS6000_BI_ARCH_P 0

#else

#define	DEFAULT_ARCH64_P (TARGET_DEFAULT & MASK_64BIT)
#define	RS6000_BI_ARCH_P 1

#endif

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
#ifdef __powerpc64__
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif

#undef	TARGET_AIX
#define	TARGET_AIX TARGET_64BIT

#undef PROCESSOR_DEFAULT64
#define PROCESSOR_DEFAULT64 PROCESSOR_PPC630

#undef	TARGET_RELOCATABLE
#define	TARGET_RELOCATABLE (!TARGET_64BIT && (target_flags & MASK_RELOCATABLE))

#undef	RS6000_ABI_NAME
#define	RS6000_ABI_NAME (TARGET_64BIT ? "aixdesc" : "sysv")

#define INVALID_64BIT "-m%s not supported in this configuration"
#define INVALID_32BIT INVALID_64BIT

#undef	SUBSUBTARGET_OVERRIDE_OPTIONS
#define	SUBSUBTARGET_OVERRIDE_OPTIONS				\
  do								\
    {								\
      if (rs6000_alignment_string == 0)				\
	rs6000_alignment_flags = MASK_ALIGN_NATURAL;		\
      if (TARGET_64BIT)						\
	{							\
	  if (DEFAULT_ABI != ABI_AIX)				\
	    {							\
	      rs6000_current_abi = ABI_AIX;			\
	      error (INVALID_64BIT, "call");			\
	    }							\
	  if (target_flags & MASK_RELOCATABLE)			\
	    {							\
	      target_flags &= ~MASK_RELOCATABLE;		\
	      error (INVALID_64BIT, "relocatable");		\
	    }							\
	  if (target_flags & MASK_EABI)				\
	    {							\
	      target_flags &= ~MASK_EABI;			\
	      error (INVALID_64BIT, "eabi");			\
	    }							\
	  if (target_flags & MASK_PROTOTYPE)			\
	    {							\
	      target_flags &= ~MASK_PROTOTYPE;			\
	      error (INVALID_64BIT, "prototype");		\
	    }							\
          if ((target_flags & MASK_POWERPC64) == 0)		\
	    {							\
	      target_flags |= MASK_POWERPC64;			\
	      error ("-m64 requires a PowerPC64 cpu");		\
	    }							\
	}							\
      else							\
	{							\
	  if (!RS6000_BI_ARCH_P)				\
	    error (INVALID_32BIT, "32");			\
	  if (TARGET_PROFILE_KERNEL)				\
	    {							\
	      target_flags &= ~MASK_PROFILE_KERNEL;		\
	      error (INVALID_32BIT, "profile-kernel");		\
	    }							\
	}							\
    }								\
  while (0)

#ifdef	RS6000_BI_ARCH

#undef	OVERRIDE_OPTIONS
#define	OVERRIDE_OPTIONS \
  rs6000_override_options (((TARGET_DEFAULT ^ target_flags) & MASK_64BIT) \
			   ? (char *) 0 : TARGET_CPU_DEFAULT)

#endif

#undef	ASM_DEFAULT_SPEC
#undef	ASM_SPEC
#undef	LINK_OS_LINUX_SPEC

#ifndef	RS6000_BI_ARCH
#define	ASM_DEFAULT_SPEC "-mppc64"
#define	ASM_SPEC         "%(asm_spec64) %(asm_spec_common)"
#define	LINK_OS_LINUX_SPEC "%(link_os_linux_spec64)"
#else
#if DEFAULT_ARCH64_P
#define	ASM_DEFAULT_SPEC "-mppc%{!m32:64}"
#define	ASM_SPEC         "%{m32:%(asm_spec32)}%{!m32:%(asm_spec64)} %(asm_spec_common)"
#define	LINK_OS_LINUX_SPEC "%{m32:%(link_os_linux_spec32)}%{!m32:%(link_os_linux_spec64)}"
#else
#define	ASM_DEFAULT_SPEC "-mppc%{m64:64}"
#define	ASM_SPEC         "%{!m64:%(asm_spec32)}%{m64:%(asm_spec64)} %(asm_spec_common)"
#define	LINK_OS_LINUX_SPEC "%{!m64:%(link_os_linux_spec32)}%{m64:%(link_os_linux_spec64)}"
#endif
#endif

#define ASM_SPEC32 "-a32 %{n} %{T} %{Ym,*} %{Yd,*} \
%{mrelocatable} %{mrelocatable-lib} %{fpic:-K PIC} %{fPIC:-K PIC} \
%{memb} %{!memb: %{msdata: -memb} %{msdata=eabi: -memb}} \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-freebsd: -mbig} \
    %{mcall-i960-old: -mlittle} \
    %{mcall-linux: -mbig} \
    %{mcall-gnu: -mbig} \
    %{mcall-netbsd: -mbig} \
}}}}"

#define ASM_SPEC64 "-a64"

#define ASM_SPEC_COMMON "%(asm_cpu) \
%{.s: %{mregnames} %{mno-regnames}} %{.S: %{mregnames} %{mno-regnames}} \
%{v:-V} %{Qy:} %{!Qn:-Qy} %{Wa,*:%*} \
%{mlittle} %{mlittle-endian} %{mbig} %{mbig-endian}"

#undef	SUBSUBTARGET_EXTRA_SPECS
#define SUBSUBTARGET_EXTRA_SPECS \
  { "asm_spec_common",		ASM_SPEC_COMMON },			\
  { "asm_spec32",		ASM_SPEC32 },				\
  { "asm_spec64",		ASM_SPEC64 },				\
  { "link_os_linux_spec32",	LINK_OS_LINUX_SPEC32 },			\
  { "link_os_linux_spec64",	LINK_OS_LINUX_SPEC64 },

#undef	MULTILIB_DEFAULTS
#if DEFAULT_ARCH64_P
#define MULTILIB_DEFAULTS { "m64" }
#else
#define MULTILIB_DEFAULTS { "m32" }
#endif

#ifndef RS6000_BI_ARCH

/* 64-bit PowerPC Linux is always big-endian.  */
#undef	TARGET_LITTLE_ENDIAN
#define TARGET_LITTLE_ENDIAN	0

/* 64-bit PowerPC Linux always has a TOC.  */
#undef  TARGET_TOC
#define	TARGET_TOC		1

/* Some things from sysv4.h we don't do when 64 bit.  */
#undef	TARGET_RELOCATABLE
#define	TARGET_RELOCATABLE	0
#undef	TARGET_EABI
#define	TARGET_EABI		0
#undef	TARGET_PROTOTYPE
#define	TARGET_PROTOTYPE	0

#endif

#define	MASK_PROFILE_KERNEL	0x00100000

/* Non-standard profiling for kernels, which just saves LR then calls
   _mcount without worrying about arg saves.  The idea is to change
   the function prologue as little as possible as it isn't easy to
   account for arg save/restore code added just for _mcount.  */
#define TARGET_PROFILE_KERNEL	(target_flags & MASK_PROFILE_KERNEL)

/* Override sysv4.h.  */
#undef	EXTRA_SUBTARGET_SWITCHES
#define EXTRA_SUBTARGET_SWITCHES					\
  {"profile-kernel",	 MASK_PROFILE_KERNEL,				\
   N_("Call mcount for profiling before a function prologue") },	\
  {"no-profile-kernel",	-MASK_PROFILE_KERNEL,				\
   N_("Call mcount for profiling after a function prologue") },

/* We use glibc _mcount for profiling.  */
#define NO_PROFILE_COUNTERS TARGET_64BIT
#define PROFILE_HOOK(LABEL) \
  do { if (TARGET_64BIT) output_profile_hook (LABEL); } while (0)

/* We don't need to generate entries in .fixup.  */
#undef RELOCATABLE_NEEDS_FIXUP

/* PowerPC64 Linux word-aligns FP doubles when -malign-power is given.  */
#undef  ADJUST_FIELD_ALIGN
#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) \
  ((TARGET_ALTIVEC && TREE_CODE (TREE_TYPE (FIELD)) == VECTOR_TYPE)	\
   ? 128								\
   : (TARGET_64BIT							\
      && TARGET_ALIGN_NATURAL == 0					\
      && TYPE_MODE (TREE_CODE (TREE_TYPE (FIELD)) == ARRAY_TYPE		\
		    ? get_inner_array_type (FIELD)			\
		    : TREE_TYPE (FIELD)) == DFmode)			\
   ? MIN ((COMPUTED), 32)						\
   : (COMPUTED))

/* PowerPC64 Linux increases natural record alignment to doubleword if
   the first field is an FP double, only if in power alignment mode.  */
#undef  ROUND_TYPE_ALIGN
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)			\
  ((TARGET_ALTIVEC && TREE_CODE (STRUCT) == VECTOR_TYPE)		\
   ? MAX (MAX ((COMPUTED), (SPECIFIED)), 128)				\
   : (TARGET_64BIT							\
      && (TREE_CODE (STRUCT) == RECORD_TYPE				\
	  || TREE_CODE (STRUCT) == UNION_TYPE				\
	  || TREE_CODE (STRUCT) == QUAL_UNION_TYPE)			\
      && TARGET_ALIGN_NATURAL == 0)					\
   ? rs6000_special_round_type_align (STRUCT, COMPUTED, SPECIFIED)	\
   : MAX ((COMPUTED), (SPECIFIED)))

/* Indicate that jump tables go in the text section.  */
#undef  JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION TARGET_64BIT

/* The linux ppc64 ABI isn't explicit on whether aggregates smaller
   than a doubleword should be padded upward or downward.  You could
   reasonably assume that they follow the normal rules for structure
   layout treating the parameter area as any other block of memory,
   then map the reg param area to registers.  ie. pad updard.
   Setting both of the following defines results in this behavior.
   Setting just the first one will result in aggregates that fit in a
   doubleword being padded downward, and others being padded upward.
   Not a bad idea as this results in struct { int x; } being passed
   the same way as an int.  */
#define AGGREGATE_PADDING_FIXED TARGET_64BIT
#define AGGREGATES_PAD_UPWARD_ALWAYS 0

/* We don't want anything in the reg parm area being passed on the
   stack.  */
#define MUST_PASS_IN_STACK(MODE, TYPE)				\
  ((TARGET_64BIT						\
    && (TYPE) != 0						\
    && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST		\
	|| TREE_ADDRESSABLE (TYPE)))				\
   || (!TARGET_64BIT						\
       && default_must_pass_in_stack ((MODE), (TYPE))))

/* Specify padding for the last element of a block move between
   registers and memory.  FIRST is nonzero if this is the only
   element.  */
#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (!(FIRST) ? upward : FUNCTION_ARG_PADDING (MODE, TYPE))

/* __throw will restore its own return address to be the same as the
   return address of the function that the throw is being made to.
   This is unfortunate, because we want to check the original
   return address to see if we need to restore the TOC.
   So we have to squirrel it away with this.  */
#define SETUP_FRAME_ADDRESSES() \
  do { if (TARGET_64BIT) rs6000_aix_emit_builtin_unwind_init (); } while (0)

/* Override svr4.h  */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()            		\
  do							\
    {							\
      if (TARGET_64BIT)					\
	{						\
	  builtin_define ("__PPC__");			\
	  builtin_define ("__PPC64__");			\
	  builtin_define ("__powerpc__");		\
	  builtin_define ("__powerpc64__");		\
	  builtin_define ("__PIC__");			\
	  builtin_assert ("cpu=powerpc64");		\
	  builtin_assert ("machine=powerpc64");		\
	}						\
      else						\
	{						\
	  builtin_define_std ("PPC");			\
	  builtin_define_std ("powerpc");		\
	  builtin_assert ("cpu=powerpc");		\
	  builtin_assert ("machine=powerpc");		\
	  TARGET_OS_SYSV_CPP_BUILTINS ();		\
	}						\
    }							\
  while (0)

#undef  CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_linux)"

/* The GNU C++ standard library currently requires _GNU_SOURCE being
   defined on glibc-based systems. This temporary hack accomplishes this,
   it should go away as soon as libstdc++-v3 has a real fix.  */
#undef  CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef  LINK_SHLIB_SPEC
#define LINK_SHLIB_SPEC "%{shared:-shared} %{!shared: %{static:-static}}"

#undef  LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_linux)"

#undef  STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_linux)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_linux)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_linux)"

#undef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC "%(link_os_linux)"

#define LINK_OS_LINUX_SPEC32 "-m elf32ppclinux %{!shared: %{!static: \
  %{rdynamic:-export-dynamic} \
  %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}}}"

#define LINK_OS_LINUX_SPEC64 "-m elf64ppc %{!shared: %{!static: \
  %{rdynamic:-export-dynamic} \
  %{!dynamic-linker:-dynamic-linker /lib64/ld64.so.1}}}"

#undef  TOC_SECTION_ASM_OP
#define TOC_SECTION_ASM_OP \
  (TARGET_64BIT						\
   ? "\t.section\t\".toc\",\"aw\""			\
   : "\t.section\t\".got\",\"aw\"")

#undef  MINIMAL_TOC_SECTION_ASM_OP
#define MINIMAL_TOC_SECTION_ASM_OP \
  (TARGET_64BIT						\
   ? "\t.section\t\".toc1\",\"aw\""			\
   : ((TARGET_RELOCATABLE || flag_pic)			\
      ? "\t.section\t\".got2\",\"aw\""			\
      : "\t.section\t\".got1\",\"aw\""))

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC64 GNU/Linux)");

/* Must be at least as big as our pointer type.  */
#undef	SIZE_TYPE
#define	SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "unsigned int")

#undef	PTRDIFF_TYPE
#define	PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#undef	WCHAR_TYPE
#define	WCHAR_TYPE (TARGET_64BIT ? "int" : "long int")
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Override rs6000.h definition.  */
#undef  ASM_APP_ON
#define ASM_APP_ON "#APP\n"

/* Override rs6000.h definition.  */
#undef  ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* PowerPC no-op instruction.  */
#undef  RS6000_CALL_GLUE
#define RS6000_CALL_GLUE (TARGET_64BIT ? "nop" : "cror 31,31,31")

#undef  RS6000_MCOUNT
#define RS6000_MCOUNT "_mcount"

#ifdef __powerpc64__
/* _init and _fini functions are built from bits spread across many
   object files, each potentially with a different TOC pointer.  For
   that reason, place a nop after the call so that the linker can
   restore the TOC pointer if a TOC adjusting call stub is needed.  */
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
  asm (SECTION_OP "\n"					\
"	bl ." #FUNC "\n"				\
"	nop\n"						\
"	.previous");
#endif

/* FP save and restore routines.  */
#undef  SAVE_FP_PREFIX
#define SAVE_FP_PREFIX (TARGET_64BIT ? "._savef" : "_savefpr_")
#undef  SAVE_FP_SUFFIX
#define SAVE_FP_SUFFIX (TARGET_64BIT ? "" : "_l")
#undef  RESTORE_FP_PREFIX
#define RESTORE_FP_PREFIX (TARGET_64BIT ? "._restf" : "_restfpr_")
#undef  RESTORE_FP_SUFFIX
#define RESTORE_FP_SUFFIX (TARGET_64BIT ? "" : "_l")

/* Dwarf2 debugging.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* This is how to declare the size of a function.  */
#undef	ASM_DECLARE_FUNCTION_SIZE
#define	ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do									\
    {									\
      if (!flag_inhibit_size_directive)					\
	{								\
	  fputs ("\t.size\t", (FILE));					\
	  if (TARGET_64BIT)						\
	    putc ('.', (FILE));						\
	  assemble_name ((FILE), (FNAME));				\
	  fputs (",.-", (FILE));					\
	  if (TARGET_64BIT)						\
	    putc ('.', (FILE));						\
	  assemble_name ((FILE), (FNAME));				\
	  putc ('\n', (FILE));						\
	}								\
    }									\
  while (0)

/* Return nonzero if this entry is to be written into the constant
   pool in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF
   or a CONST containing one of them.  If -mfp-in-toc (the default),
   we also do this for floating-point constants.  We actually can only
   do this if the FP formats of the target and host machines are the
   same, but we can't check that since not every file that uses
   GO_IF_LEGITIMATE_ADDRESS_P includes real.h.  We also do this when
   we can write the entry into the TOC and the entry is not larger
   than a TOC entry.  */

#undef  ASM_OUTPUT_SPECIAL_POOL_ENTRY_P
#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)			\
  (TARGET_TOC								\
   && (GET_CODE (X) == SYMBOL_REF					\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
       || GET_CODE (X) == LABEL_REF					\
       || (GET_CODE (X) == CONST_INT 					\
	   && GET_MODE_BITSIZE (MODE) <= GET_MODE_BITSIZE (Pmode))	\
       || (GET_CODE (X) == CONST_DOUBLE					\
	   && ((TARGET_64BIT						\
		&& (TARGET_POWERPC64					\
		    || TARGET_MINIMAL_TOC				\
		    || (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT	\
			&& ! TARGET_NO_FP_IN_TOC)))			\
	       || (!TARGET_64BIT					\
		   && !TARGET_NO_FP_IN_TOC				\
		   && !TARGET_RELOCATABLE				\
		   && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT	\
		   && BITS_PER_WORD == HOST_BITS_PER_INT)))))

/* This is the same as the dbxelf.h version, except that we need to
   use the function code label, not the function descriptor.  */
#undef	ASM_OUTPUT_SOURCE_LINE
#define	ASM_OUTPUT_SOURCE_LINE(FILE, LINE, COUNTER)			\
do									\
  {									\
    char temp[256];							\
    ASM_GENERATE_INTERNAL_LABEL (temp, "LM", COUNTER);			\
    fprintf (FILE, "\t.stabn 68,0,%d,", LINE);				\
    assemble_name (FILE, temp);						\
    putc ('-', FILE);							\
    if (TARGET_64BIT)							\
      putc ('.', FILE);							\
    assemble_name (FILE,						\
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));\
    putc ('\n', FILE);							\
    (*targetm.asm_out.internal_label) (FILE, "LM", COUNTER);		\
  }									\
while (0)

/* Similarly, we want the function code label here.  */
#define DBX_OUTPUT_BRAC(FILE, NAME, BRAC) \
  do									\
    {									\
      const char *flab;							\
      fprintf (FILE, "%s%d,0,0,", ASM_STABN_OP, BRAC);			\
      assemble_name (FILE, NAME);					\
      putc ('-', FILE);							\
      if (current_function_func_begin_label != NULL_TREE)		\
	flab = IDENTIFIER_POINTER (current_function_func_begin_label);	\
      else								\
	{								\
	  if (TARGET_64BIT)						\
	    putc ('.', FILE);						\
	  flab = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);	\
	}								\
      assemble_name (FILE, flab);					\
      putc ('\n', FILE);						\
    }									\
  while (0)

#define DBX_OUTPUT_LBRAC(FILE, NAME) DBX_OUTPUT_BRAC (FILE, NAME, N_LBRAC)
#define DBX_OUTPUT_RBRAC(FILE, NAME) DBX_OUTPUT_BRAC (FILE, NAME, N_RBRAC)

/* Another case where we want the dot name.  */
#define	DBX_OUTPUT_NFUN(FILE, LSCOPE, DECL)				\
  do									\
    {									\
      fprintf (FILE, "%s\"\",%d,0,0,", ASM_STABS_OP, N_FUN);		\
      assemble_name (FILE, LSCOPE);					\
      putc ('-', FILE);							\
      if (TARGET_64BIT)							\
        putc ('.', FILE);						\
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (DECL), 0), 0));	\
      putc ('\n', FILE);						\
    }									\
  while (0)

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.  */
#undef	ASM_PREFERRED_EH_DATA_FORMAT
#define	ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  ((TARGET_64BIT || flag_pic || TARGET_RELOCATABLE)			\
   ? (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel		\
      | (TARGET_64BIT ? DW_EH_PE_udata8 : DW_EH_PE_sdata4))		\
   : DW_EH_PE_absptr)

/* For backward compatibility, we must continue to use the AIX
   structure return convention.  */
#undef DRAFT_V4_STRUCT_RET
#define DRAFT_V4_STRUCT_RET (!TARGET_64BIT)

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#define TARGET_HAS_F_SETLKW

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#ifdef IN_LIBGCC2
#include <signal.h>
#ifdef __powerpc64__
#include <sys/ucontext.h>

enum { SIGNAL_FRAMESIZE = 128 };

#else

/* During the 2.5 kernel series the kernel ucontext was changed, but
   the new layout is compatible with the old one, so we just define
   and use the old one here for simplicity and compatibility.  */

struct kernel_old_ucontext {
  unsigned long     uc_flags;
  struct ucontext  *uc_link;
  stack_t           uc_stack;
  struct sigcontext_struct uc_mcontext;
  sigset_t          uc_sigmask;
};
enum { SIGNAL_FRAMESIZE = 64 };
#endif

#endif

#ifdef __powerpc64__

/* If the current unwind info (FS) does not contain explicit info
   saving R2, then we have to do a minor amount of code reading to
   figure out if it was saved.  The big problem here is that the
   code that does the save/restore is generated by the linker, so
   we have no good way to determine at compile time what to do.  */

#define MD_FROB_UPDATE_CONTEXT(CTX, FS)					\
  do {									\
    if ((FS)->regs.reg[2].how == REG_UNSAVED)				\
      {									\
	unsigned int *insn						\
	  = (unsigned int *)						\
	    _Unwind_GetGR ((CTX), LINK_REGISTER_REGNUM);		\
	if (*insn == 0xE8410028)					\
	  _Unwind_SetGRPtr ((CTX), 2, (CTX)->cfa + 40);			\
      }									\
  } while (0)

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned char *pc_ = (CONTEXT)->ra;					\
    struct sigcontext *sc_;						\
    long new_cfa_;							\
    int i_;								\
									\
    /* addi r1, r1, 128; li r0, 0x0077; sc  (sigreturn) */		\
    /* addi r1, r1, 128; li r0, 0x00AC; sc  (rt_sigreturn) */		\
    if (*(unsigned int *) (pc_+0) != 0x38210000 + SIGNAL_FRAMESIZE	\
	|| *(unsigned int *) (pc_+8) != 0x44000002)			\
      break;								\
    if (*(unsigned int *) (pc_+4) == 0x38000077)			\
      {									\
	struct sigframe {						\
	  char gap[SIGNAL_FRAMESIZE];					\
	  struct sigcontext sigctx;					\
	} *rt_ = (CONTEXT)->cfa;					\
	sc_ = &rt_->sigctx;						\
      }									\
    else if (*(unsigned int *) (pc_+4) == 0x380000AC)			\
      {									\
	struct rt_sigframe {						\
	  int tramp[6];							\
	  struct siginfo *pinfo;					\
	  struct ucontext *puc;						\
	} *rt_ = (struct rt_sigframe *) pc_;				\
	sc_ = &rt_->puc->uc_mcontext;					\
      }									\
    else								\
      break;								\
    									\
    new_cfa_ = sc_->regs->gpr[STACK_POINTER_REGNUM];			\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = STACK_POINTER_REGNUM;				\
    (FS)->cfa_offset = new_cfa_ - (long) (CONTEXT)->cfa;		\
    									\
    for (i_ = 0; i_ < 32; i_++)						\
      if (i_ != STACK_POINTER_REGNUM)					\
	{	    							\
	  (FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	  (FS)->regs.reg[i_].loc.offset 				\
	    = (long)&(sc_->regs->gpr[i_]) - new_cfa_;			\
	}								\
									\
    (FS)->regs.reg[LINK_REGISTER_REGNUM].how = REG_SAVED_OFFSET;	\
    (FS)->regs.reg[LINK_REGISTER_REGNUM].loc.offset 			\
      = (long)&(sc_->regs->link) - new_cfa_;				\
									\
    (FS)->regs.reg[ARG_POINTER_REGNUM].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[ARG_POINTER_REGNUM].loc.offset 			\
      = (long)&(sc_->regs->nip) - new_cfa_;				\
    (FS)->retaddr_column = ARG_POINTER_REGNUM;				\
    goto SUCCESS;							\
  } while (0)

#else

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned char *pc_ = (CONTEXT)->ra;					\
    struct sigcontext *sc_;						\
    long new_cfa_;							\
    int i_;								\
									\
    /* li r0, 0x7777; sc  (sigreturn old)  */				\
    /* li r0, 0x0077; sc  (sigreturn new)  */				\
    /* li r0, 0x6666; sc  (rt_sigreturn old)  */			\
    /* li r0, 0x00AC; sc  (rt_sigreturn new)  */			\
    if (*(unsigned int *) (pc_+4) != 0x44000002)			\
      break;								\
    if (*(unsigned int *) (pc_+0) == 0x38007777				\
	|| *(unsigned int *) (pc_+0) == 0x38000077)			\
      {									\
	struct sigframe {						\
	  char gap[SIGNAL_FRAMESIZE];					\
	  struct sigcontext sigctx;					\
	} *rt_ = (CONTEXT)->cfa;					\
	sc_ = &rt_->sigctx;						\
      }									\
    else if (*(unsigned int *) (pc_+0) == 0x38006666			\
	     || *(unsigned int *) (pc_+0) == 0x380000AC)		\
      {									\
	struct rt_sigframe {						\
	  char gap[SIGNAL_FRAMESIZE];					\
	  unsigned long _unused[2];					\
	  struct siginfo *pinfo;					\
	  void *puc;							\
	  struct siginfo info;						\
	  struct kernel_old_ucontext uc;				\
	} *rt_ = (CONTEXT)->cfa;					\
	sc_ = &rt_->uc.uc_mcontext;					\
      }									\
    else								\
      break;								\
    									\
    new_cfa_ = sc_->regs->gpr[STACK_POINTER_REGNUM];			\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = STACK_POINTER_REGNUM;				\
    (FS)->cfa_offset = new_cfa_ - (long) (CONTEXT)->cfa;		\
    									\
    for (i_ = 0; i_ < 32; i_++)						\
      if (i_ != STACK_POINTER_REGNUM)					\
	{	    							\
	  (FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	  (FS)->regs.reg[i_].loc.offset 				\
	    = (long)&(sc_->regs->gpr[i_]) - new_cfa_;			\
	}								\
									\
    (FS)->regs.reg[LINK_REGISTER_REGNUM].how = REG_SAVED_OFFSET;	\
    (FS)->regs.reg[LINK_REGISTER_REGNUM].loc.offset 			\
      = (long)&(sc_->regs->link) - new_cfa_;				\
									\
    (FS)->regs.reg[CR0_REGNO].how = REG_SAVED_OFFSET;			\
    (FS)->regs.reg[CR0_REGNO].loc.offset 				\
      = (long)&(sc_->regs->nip) - new_cfa_;				\
    (FS)->retaddr_column = CR0_REGNO;					\
    goto SUCCESS;							\
  } while (0)

#endif


#define OS_MISSING_POWERPC64 !TARGET_64BIT
