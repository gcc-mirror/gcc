/* Definitions for MIPS running Linux-based GNU systems with ELF format.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

#define ASM_OUTPUT_ALIGNED_BSS mips_output_aligned_bss

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME mips_declare_object_name

#undef TARGET_VERSION
#if TARGET_ENDIAN_DEFAULT == 0
#define TARGET_VERSION fprintf (stderr, " (MIPSel GNU/Linux with ELF)");
#else
#define TARGET_VERSION fprintf (stderr, " (MIPS GNU/Linux with ELF)");
#endif

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* If we don't set MASK_ABICALLS, we can't default to PIC.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ABICALLS|MASK_GAS)

#define TARGET_OS_CPP_BUILTINS()				\
    do {							\
	LINUX_TARGET_OS_CPP_BUILTINS();				\
	builtin_define ("__PIC__");				\
	builtin_define ("__pic__");				\
        builtin_assert ("machine=mips");			\
	/* The GNU C++ standard library requires this.  */	\
	if (c_dialect_cxx ())					\
	  builtin_define ("_GNU_SOURCE");			\
								\
      if (mips_abi == ABI_N32)					\
      {								\
        builtin_define ("_ABIN32=2");				\
        builtin_define ("_MIPS_SIM=_ABIN32");			\
        builtin_define ("_MIPS_SZLONG=32");			\
        builtin_define ("_MIPS_SZPTR=32");			\
      }								\
     else if (mips_abi == ABI_64)				\
      {								\
        builtin_define ("_ABI64=3");				\
        builtin_define ("_MIPS_SIM=_ABI64");			\
        builtin_define ("_MIPS_SZLONG=64");			\
        builtin_define ("_MIPS_SZPTR=64");			\
      }								\
     else							\
      {								\
	builtin_define ("_ABIO32=1");			\
	builtin_define ("_MIPS_SIM=_ABIO32");		\
        builtin_define ("_MIPS_SZLONG=32");			\
        builtin_define ("_MIPS_SZPTR=32");			\
      }								\
     if (TARGET_FLOAT64)					\
        builtin_define ("_MIPS_FPSET=32");			\
     else							\
        builtin_define ("_MIPS_FPSET=16");			\
								\
     if (TARGET_INT64)						\
        builtin_define ("_MIPS_SZINT=64");			\
     else							\
        builtin_define ("_MIPS_SZINT=32");			\
} while (0)

#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
%{fno-PIC:-U__PIC__ -U__pic__} %{fno-pic:-U__PIC__ -U__pic__} \
%{fPIC|fPIE|fpic|fpie:-D__PIC__ -D__pic__} \
%{pthread:-D_REENTRANT}"

/* From iris5.h */
/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 0

/* Borrowed from sparc/linux.h */
#undef LINK_SPEC
#define LINK_SPEC \
 "%(endian_spec) \
  %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
        %{static:-static}}}"

#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "\
%{mabi=64: -64} \
%{!fno-PIC:%{!fno-pic:-KPIC}} \
%{fno-PIC:-non_shared} %{fno-pic:-non_shared}"

/* The MIPS assembler has different syntax for .set. We set it to
   .dummy to trap any errors.  */
#undef SET_ASM_OP
#define SET_ASM_OP "\t.dummy\t"

#undef ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {									\
	fputc ( '\t', FILE);						\
	assemble_name (FILE, LABEL1);					\
	fputs ( " = ", FILE);						\
	assemble_name (FILE, LABEL2);					\
	fputc ( '\n', FILE);						\
 } while (0)

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
	fputs ("\t.ent\t", STREAM);					\
	assemble_name (STREAM, NAME);					\
	putc ('\n', STREAM);						\
      }									\
    ASM_OUTPUT_TYPE_DIRECTIVE (STREAM, NAME, "function");		\
    assemble_name (STREAM, NAME);					\
    fputs (":\n", STREAM);						\
  } while (0)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
	fputs ("\t.end\t", STREAM);					\
	assemble_name (STREAM, NAME);					\
	putc ('\n', STREAM);						\
      }									\
  } while (0)

/* Tell function_prologue in mips.c that we have already output the .ent/.end
   pseudo-ops.  */
#undef FUNCTION_NAME_ALREADY_DECLARED
#define FUNCTION_NAME_ALREADY_DECLARED 1

/* The glibc _mcount stub will save $v0 for us.  Don't mess with saving
   it, since ASM_OUTPUT_REG_PUSH/ASM_OUTPUT_REG_POP do not work in the
   presence of $gp-relative calls.  */
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP

#undef LIB_SPEC
#define LIB_SPEC "\
%{shared: -lc} \
%{!static:-rpath-link %R/lib:%R/usr/lib} \
%{!shared: %{pthread:-lpthread} \
  %{profile:-lc_p} %{!profile: -lc}}"

#ifndef inhibit_libc
/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */
#ifdef IN_LIBGCC2
#include <signal.h>

/* The third parameter to the signal handler points to something with
 * this structure defined in asm/ucontext.h, but the name clashes with
 * struct ucontext from sys/ucontext.h so this private copy is used.  */
typedef struct _sig_ucontext {
    unsigned long         uc_flags;
    struct _sig_ucontext  *uc_link;
    stack_t               uc_stack;
    struct sigcontext uc_mcontext;
    sigset_t      uc_sigmask;
} _sig_ucontext_t;

#endif /* IN_LIBGCC2  */

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)            \
  do {                                                               \
    u_int32_t *pc_ = (u_int32_t *) (CONTEXT)->ra;                \
    struct sigcontext *sc_;                                          \
    _Unwind_Ptr new_cfa_;                                            \
    int i_;                                                          \
                                                                     \
    /* 24021061 li v0, 0x1061 (rt_sigreturn)*/                       \
    /* 0000000c syscall    */                                        \
    /*    or */                                                      \
    /* 24021017 li v0, 0x1017 (sigreturn) */                         \
    /* 0000000c syscall  */                                          \
    if (*(pc_ + 1) != 0x0000000c)                                    \
      break;                                                         \
    if (*(pc_ + 0) == 0x24021017)                                    \
      {                                                              \
        struct sigframe {                                            \
          u_int32_t  trampoline[2];                                \
          struct sigcontext sigctx;                                  \
        } *rt_ = (CONTEXT)->ra;                                      \
        sc_ = &rt_->sigctx;                                          \
      }                                                              \
    else if (*(pc_ + 0) == 0x24021061)                               \
      {                                                              \
        struct rt_sigframe {                                         \
          u_int32_t  trampoline[2];                                \
          struct siginfo info;                                       \
          _sig_ucontext_t uc;                                        \
        } *rt_ = (CONTEXT)->ra;                                      \
        sc_ = &rt_->uc.uc_mcontext;                                  \
      }                                                              \
    else                                                             \
      break;                                                         \
                                                                     \
    new_cfa_ = (_Unwind_Ptr)sc_;                                     \
    (FS)->cfa_how = CFA_REG_OFFSET;                                  \
    (FS)->cfa_reg = STACK_POINTER_REGNUM;                            \
    (FS)->cfa_offset = new_cfa_ - (_Unwind_Ptr) (CONTEXT)->cfa;      \
                                                                     \
    for (i_ = 0; i_ < 32; i_++) {                                    \
      (FS)->regs.reg[i_].how = REG_SAVED_OFFSET;                     \
      (FS)->regs.reg[i_].loc.offset                                  \
        = (_Unwind_Ptr)&(sc_->sc_regs[i_]) - new_cfa_;               \
    }                                                                \
    (FS)->regs.reg[SIGNAL_UNWIND_RETURN_COLUMN].how = REG_SAVED_OFFSET; \
    (FS)->regs.reg[SIGNAL_UNWIND_RETURN_COLUMN].loc.offset           \
        = (_Unwind_Ptr)&(sc_->sc_pc) - new_cfa_;                     \
    (FS)->retaddr_column = SIGNAL_UNWIND_RETURN_COLUMN;              \
                                                                     \
    goto SUCCESS;                                                    \
  } while (0)
#endif
