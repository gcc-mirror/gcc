/* Definitions of target machine for GNU compiler,
   for PowerPC machines running Linux.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by Michael Meissner (meissner@cygnus.com).

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

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()          \
  do                                      \
    {                                     \
      builtin_define_std ("PPC");         \
      builtin_define_std ("powerpc");     \
      builtin_assert ("cpu=powerpc");     \
      builtin_assert ("machine=powerpc"); \
      TARGET_OS_SYSV_CPP_BUILTINS ();	  \
    }                                     \
  while (0)

#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_linux)"

/* The GNU C++ standard library currently requires _GNU_SOURCE being
   defined on glibc-based systems. This temporary hack accomplishes this,
   it should go away as soon as libstdc++-v3 has a real fix.  */
#undef  CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef  LINK_SHLIB_SPEC
#define LINK_SHLIB_SPEC "%{shared:-shared} %{!shared: %{static:-static}}"

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_linux)"

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_linux)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_linux)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_linux)"

#undef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC "%(link_os_linux)"

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC GNU/Linux)");

/* Override rs6000.h definition.  */
#undef  ASM_APP_ON
#define ASM_APP_ON "#APP\n"

/* Override rs6000.h definition.  */
#undef  ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* For backward compatibility, we must continue to use the AIX
   structure return convention.  */
#undef  DRAFT_V4_STRUCT_RET
#define DRAFT_V4_STRUCT_RET 1

/* We are 32-bit all the time, so optimize a little.  */
#undef TARGET_64BIT
#define TARGET_64BIT 0
 
/* We don't need to generate entries in .fixup.  */
#undef RELOCATABLE_NEEDS_FIXUP

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#define TARGET_HAS_F_SETLKW

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#ifdef IN_LIBGCC2
#include <signal.h>

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

#define OS_MISSING_POWERPC64 1
