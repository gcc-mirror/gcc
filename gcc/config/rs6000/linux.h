/* Definitions of target machine for GNU compiler,
   for powerpc machines running Linux.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001 Free Software Foundation, 
   Inc.
   Contributed by Michael Meissner (meissner@cygnus.com).

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

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-DPPC -D__ELF__ -Dpowerpc -Acpu=powerpc -Amachine=powerpc"

#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_linux)"

/* The GNU C++ standard library currently requires _GNU_SOURCE being
   defined on glibc-based systems. This temporary hack accomplishes this,
   it should go away as soon as libstdc++-v3 has a real fix.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef LINK_SHLIB_SPEC
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

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC GNU/Linux)");

/* Override rs6000.h definition.  */
#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

/* Override rs6000.h definition.  */
#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* For backward compatibility, we must continue to use the AIX
   structure return convention.  */
#undef DRAFT_V4_STRUCT_RET
#define DRAFT_V4_STRUCT_RET 1

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#ifdef IN_LIBGCC2
#include <signal.h>
#include <sys/ucontext.h>

enum { SIGNAL_FRAMESIZE = 64 };
#endif

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned char *pc_ = (CONTEXT)->ra;					\
    struct sigcontext *sc_;						\
    long new_cfa_;							\
    int i_;								\
									\
    /* li r0, 0x7777; sc  (rt_sigreturn)  */				\
    /* li r0, 0x6666; sc  (sigreturn)  */				\
    if (((*(unsigned int *) (pc_+0) == 0x38007777)			\
	 || (*(unsigned int *) (pc_+0) == 0x38006666))			\
	&& (*(unsigned int *) (pc_+4)  == 0x44000002))			\
	sc_ = (CONTEXT)->cfa + SIGNAL_FRAMESIZE;			\
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
    /* The unwinder expects the IP to point to the following insn,	\
       whereas the kernel returns the address of the actual		\
       faulting insn.  */						\
    sc_->regs->nip += 4;  						\
    (FS)->regs.reg[CR0_REGNO].how = REG_SAVED_OFFSET;			\
    (FS)->regs.reg[CR0_REGNO].loc.offset 				\
      = (long)&(sc_->regs->nip) - new_cfa_;				\
    (FS)->retaddr_column = CR0_REGNO;					\
    goto SUCCESS;							\
  } while (0)

