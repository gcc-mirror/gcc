/* Definitions for Linux for S/390.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).

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

#ifndef _LINUX_H
#define _LINUX_H

/* Target specific version string.  */

#ifdef DEFAULT_TARGET_64BIT
#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Linux for zSeries)");
#else
#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Linux for S/390)");
#endif


/* Target specific type definitions.  */

/* ??? Do we really want long as size_t on 31-bit?  */
#undef  SIZE_TYPE
#define SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "long unsigned int")
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#undef  WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* Target specific preprocessor settings.  */

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("linux");		\
      builtin_define_std ("unix");		\
      builtin_assert ("system=linux");		\
      builtin_assert ("system=unix");		\
      builtin_define ("__ELF__");		\
      builtin_define ("__gnu_linux__");		\
      if (flag_pic)				\
        {					\
          builtin_define ("__PIC__");		\
          builtin_define ("__pic__");		\
        }					\
    }						\
  while (0)


/* Target specific assembler settings.  */

#ifdef DEFAULT_TARGET_64BIT
#undef  ASM_SPEC
#define ASM_SPEC "%{m31:-m31 -Aesa}"
#else
#undef  ASM_SPEC
#define ASM_SPEC "%{m64:-m64 -Aesame}"
#endif


/* Target specific linker settings.  */

#ifdef DEFAULT_TARGET_64BIT
#define MULTILIB_DEFAULTS { "m64" }
#else
#define MULTILIB_DEFAULTS { "m31" }
#endif

#define LINK_ARCH31_SPEC \
  "-m elf_s390 \
   %{shared:-shared} \
   %{!shared: \
      %{static:-static} \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld.so.1}}}"

#define LINK_ARCH64_SPEC \
  "-m elf64_s390 \
   %{shared:-shared} \
   %{!shared: \
      %{static:-static} \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker /lib/ld64.so.1}}}"

#ifdef DEFAULT_TARGET_64BIT
#undef  LINK_SPEC
#define LINK_SPEC "%{m31:%(link_arch31)} %{!m31:%(link_arch64)}"
#else
#undef  LINK_SPEC
#define LINK_SPEC "%{m64:%(link_arch64)} %{!m64:%(link_arch31)}"
#endif


/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.  */

#define EXTRA_SPECS \
  { "link_arch31",	LINK_ARCH31_SPEC },	\
  { "link_arch64",	LINK_ARCH64_SPEC },	\

#define ASM_FILE_END(FILE) \
  do {									\
    named_section_flags (".note.GNU-stack",				\
			 SECTION_DEBUG					\
			 | (trampolines_created ? SECTION_CODE : 0));	\
  } while (0)

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned char *pc_ = (CONTEXT)->ra;					\
    long new_cfa_;							\
    int i_;								\
									\
    typedef struct 							\
      {									\
        unsigned long psw_mask;						\
        unsigned long psw_addr;						\
        unsigned long gprs[16];						\
        unsigned int  acrs[16];						\
        unsigned int  fpc;						\
        unsigned int  __pad;						\
        double        fprs[16];						\
      } __attribute__ ((__aligned__ (8))) sigregs_;			\
									\
    sigregs_ *regs_;							\
									\
    /* svc $__NR_sigreturn or svc $__NR_rt_sigreturn  */		\
    if (pc_[0] != 0x0a || (pc_[1] != 119 && pc_[1] != 173))		\
      break;								\
									\
    /* New-style RT frame:  						\
	retcode + alignment (8 bytes)					\
	siginfo (128 bytes)						\
	ucontext (contains sigregs)  */					\
    if ((CONTEXT)->ra == (CONTEXT)->cfa)				\
      {									\
	struct ucontext_						\
	  {								\
	    unsigned long     uc_flags;					\
	    struct ucontext_ *uc_link;					\
	    unsigned long     uc_stack[3];				\
	    sigregs_          uc_mcontext;				\
	  } *uc_ = (CONTEXT)->cfa + 8 + 128;				\
									\
	regs_ = &uc_->uc_mcontext;					\
      }									\
									\
    /* Old-style RT frame and all non-RT frames:			\
	old signal mask (8 bytes)					\
	pointer to sigregs  */						\
    else								\
      {									\
	regs_ = *(sigregs_ **)((CONTEXT)->cfa + 8);			\
      }									\
      									\
    new_cfa_ = regs_->gprs[15] + 16*sizeof(long) + 32;			\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = 15;							\
    (FS)->cfa_offset = 							\
      new_cfa_ - (long) (CONTEXT)->cfa + 16*sizeof(long) + 32;		\
									\
    for (i_ = 0; i_ < 16; i_++)						\
      {									\
	(FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_].loc.offset = 				\
	  (long)&regs_->gprs[i_] - new_cfa_;				\
      }									\
    for (i_ = 0; i_ < 16; i_++)						\
      {									\
	(FS)->regs.reg[16+i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[16+i_].loc.offset = 				\
	  (long)&regs_->fprs[i_] - new_cfa_;				\
      }									\
									\
    /* Load return addr from PSW into dummy register 32.  */		\
    (FS)->regs.reg[32].how = REG_SAVED_OFFSET;				\
    (FS)->regs.reg[32].loc.offset = (long)&regs_->psw_addr - new_cfa_;	\
    (FS)->retaddr_column = 32;						\
									\
    goto SUCCESS;							\
  } while (0)

#endif
