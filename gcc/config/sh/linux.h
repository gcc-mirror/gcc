/* Definitions for SH running Linux-based GNU systems using ELF
   Copyright (C) 1999, 2000, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Kazumoto Kojima <kkojima@rr.iij4u.or.jp>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Run-time Target Specification.  */
#undef TARGET_VERSION
#define TARGET_VERSION  fputs (" (SH GNU/Linux with ELF)", stderr);

/* We're not SYSVR4, not having /usr/ccs */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* This was defined in linux.h.  Define it here also.  */
#define HANDLE_PRAGMA_PACK_PUSH_POP

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

/* Enable DWARF 2 exceptions.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

/* ??? Current SH linux linker has a problem for DW_EH_PE_textrel.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)                      \
  (flag_pic                                                             \
    ? ((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4 \
   : DW_EH_PE_absptr)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{posix:-D_POSIX_SOURCE} \
   %{pthread:-D_REENTRANT -D_PTHREADS} \
"

#define TARGET_OS_CPP_BUILTINS() \
do { \
  builtin_define_std ("unix"); \
  builtin_define ("__gnu_linux__"); \
  builtin_define_std ("linux"); \
  builtin_assert ("system=linux"); \
  builtin_assert ("system=unix"); \
  builtin_assert ("system=posix"); \
} while (0)

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (TARGET_CPU_DEFAULT | USERMODE_BIT | TARGET_ENDIAN_DEFAULT)

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#undef SUBTARGET_LINK_EMUL_SUFFIX
#define SUBTARGET_LINK_EMUL_SUFFIX "_linux"
#undef SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC \
  "%{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
   %{static:-static}"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared: -lc} \
   %{!static:-rpath-link %R/lib:%R/usr/lib} \
   %{!shared: \
     %{mieee-fp:-lieee} \
     %{profile:-lc_p} %{!profile: -lc}}"

#if defined(HAVE_LD_EH_FRAME_HDR)
#undef LINK_EH_SPEC
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif

#undef STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Output assembler code to STREAM to call the profiler.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM,LABELNO)				\
  do {									\
    if (flag_pic)							\
      {									\
	fprintf (STREAM, "\tmov.l\t3f,r1\n");				\
	fprintf (STREAM, "\tmova\t3f,r0\n");				\
	fprintf (STREAM, "\tadd\tr1,r0\n");				\
	fprintf (STREAM, "\tmov.l\t1f,r1\n");				\
	fprintf (STREAM, "\tmov.l\t@(r0,r1),r1\n");			\
      }									\
    else								\
      fprintf (STREAM, "\tmov.l\t1f,r1\n");				\
    fprintf (STREAM, "\tsts.l\tpr,@-r15\n");				\
    fprintf (STREAM, "\tmova\t2f,r0\n");				\
    fprintf (STREAM, "\tjmp\t@r1\n");					\
    fprintf (STREAM, "\tlds\tr0,pr\n");					\
    fprintf (STREAM, "\t.align\t2\n");					\
    if (flag_pic)							\
      {									\
	fprintf (STREAM, "1:\t.long\tmcount@GOT\n");			\
	fprintf (STREAM, "3:\t.long\t_GLOBAL_OFFSET_TABLE_\n");		\
      }									\
    else								\
      fprintf (STREAM, "1:\t.long\tmcount\n");				\
    fprintf (STREAM, "2:\tlds.l\t@r15+,pr\n");				\
  } while (0)

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#ifdef IN_LIBGCC2
#include <signal.h>
#include <sys/ucontext.h>
#include "insn-constants.h"

# if defined (__SH5__)
#define SH_DWARF_FRAME_GP0	0
#define SH_DWARF_FRAME_FP0	(__SH5__ == 32 ? 245 : 77)
#define SH_DWARF_FRAME_XD0	289
#define SH_DWARF_FRAME_BT0	68
#define SH_DWARF_FRAME_PR	241
#define SH_DWARF_FRAME_PR_MEDIA	18
#define SH_DWARF_FRAME_GBR	238
#define SH_DWARF_FRAME_MACH	239
#define SH_DWARF_FRAME_MACL	240
#define SH_DWARF_FRAME_PC	64
#define SH_DWARF_FRAME_SR	65
#define SH_DWARF_FRAME_FPUL	244
#define SH_DWARF_FRAME_FPSCR	243
#else
#define SH_DWARF_FRAME_GP0	0
#define SH_DWARF_FRAME_FP0	25
#define SH_DWARF_FRAME_XD0	87
#define SH_DWARF_FRAME_PR	17
#define SH_DWARF_FRAME_GBR	19
#define SH_DWARF_FRAME_MACH	20
#define SH_DWARF_FRAME_MACL	21
#define SH_DWARF_FRAME_PC	16
#define SH_DWARF_FRAME_SR	22
#define SH_DWARF_FRAME_FPUL	23
#define SH_DWARF_FRAME_FPSCR	24
#endif /* defined (__SH5__) */

#if defined (__SH5__)
/* MD_FALLBACK_FRAME_STATE_FOR is not yet defined for SHMEDIA.  */
#else /* defined (__SH5__) */

#if defined (__SH3E__) || defined (__SH4__)
#define SH_FALLBACK_FRAME_FLOAT_STATE(SC, FS, CFA)			\
  do {									\
    int i_, r_;								\
									\
    r_ = SH_DWARF_FRAME_FP0;						\
    for (i_ = 0; i_ < 16; i_++)						\
      {		    							\
	(FS)->regs.reg[r_+i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[r_+i_].loc.offset 				\
	  = (long)&((SC)->sc_fpregs[i_]) - (CFA);			\
      }									\
									\
    r_ = SH_DWARF_FRAME_XD0	;					\
    for (i_ = 0; i_ < 8; i_++)						\
      {		    							\
	(FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_].loc.offset	 				\
	  = (long)&((SC)->sc_xfpregs[2*i_]) - (CFA);			\
      }									\
									\
    (FS)->regs.reg[SH_DWARF_FRAME_FPUL].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_FPUL].loc.offset			\
      = (long)&((SC)->sc_fpul) - (CFA);					\
    (FS)->regs.reg[SH_DWARF_FRAME_FPSCR].how = REG_SAVED_OFFSET;	\
    (FS)->regs.reg[SH_DWARF_FRAME_FPSCR].loc.offset			\
      = (long)&((SC)->sc_fpscr) - (CFA);				\
  } while (0)

#else
#define SH_FALLBACK_FRAME_FLOAT_STATE(SC, FS, CFA)
#endif

#define MD_FALLBACK_FRAME_STATE_FOR(CONTEXT, FS, SUCCESS)		\
  do {									\
    unsigned char *pc_ = (CONTEXT)->ra;					\
    struct sigcontext *sc_;						\
    long new_cfa_;							\
    int i_;								\
									\
    /* mov.w 1f,r3; trapa #0x10; 1: .short 0x77  (sigreturn)  */	\
    /* mov.w 1f,r3; trapa #0x10; 1: .short 0xad  (rt_sigreturn)  */	\
    /* Newer kernel uses pad instructions to avoid an SH-4 core bug.  */\
    /* mov.w 1f,r3; trapa #0x10; or r0,r0; or r0,r0; or r0,r0; or r0,r0;\
       or r0,r0; 1: .short 0x77  (sigreturn)  */			\
    /* mov.w 1f,r3; trapa #0x10; or r0,r0; or r0,r0; or r0,r0; or r0,r0;\
       or r0,r0; 1: .short 0xad  (rt_sigreturn)  */			\
    if (((*(unsigned short *) (pc_+0)  == 0x9300)			\
	 && (*(unsigned short *) (pc_+2)  == 0xc310)			\
	 && (*(unsigned short *) (pc_+4)  == 0x0077))			\
	|| (((*(unsigned short *) (pc_+0)  == 0x9305)			\
	    && (*(unsigned short *) (pc_+2)  == 0xc310)			\
	    && (*(unsigned short *) (pc_+14)  == 0x0077))))		\
      sc_ = (CONTEXT)->cfa;						\
    else if (((*(unsigned short *) (pc_+0) == 0x9300)			\
	      && (*(unsigned short *) (pc_+2)  == 0xc310)		\
	      && (*(unsigned short *) (pc_+4)  == 0x00ad))		\
	     || (((*(unsigned short *) (pc_+0) == 0x9305)		\
		 && (*(unsigned short *) (pc_+2)  == 0xc310)		\
		 && (*(unsigned short *) (pc_+14)  == 0x00ad))))	\
      {									\
	struct rt_sigframe {						\
	  struct siginfo info;						\
	  struct ucontext uc;						\
	} *rt_ = (CONTEXT)->cfa;					\
	sc_ = (struct sigcontext *) &rt_->uc.uc_mcontext;		\
      }									\
    else								\
      break;								\
    									\
    new_cfa_ = sc_->sc_regs[15];					\
    (FS)->cfa_how = CFA_REG_OFFSET;					\
    (FS)->cfa_reg = 15;							\
    (FS)->cfa_offset = new_cfa_ - (long) (CONTEXT)->cfa;		\
    									\
    for (i_ = 0; i_ < 15; i_++)						\
      {		    							\
	(FS)->regs.reg[i_].how = REG_SAVED_OFFSET;			\
	(FS)->regs.reg[i_].loc.offset	 				\
	  = (long)&(sc_->sc_regs[i_]) - new_cfa_;			\
      }									\
									\
    (FS)->regs.reg[SH_DWARF_FRAME_PR].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_PR].loc.offset			\
      = (long)&(sc_->sc_pr) - new_cfa_;					\
    (FS)->regs.reg[SH_DWARF_FRAME_SR].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_SR].loc.offset			\
      = (long)&(sc_->sc_sr) - new_cfa_;					\
    (FS)->regs.reg[SH_DWARF_FRAME_GBR].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_GBR].loc.offset			\
      = (long)&(sc_->sc_gbr) - new_cfa_;				\
    (FS)->regs.reg[SH_DWARF_FRAME_MACH].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_MACH].loc.offset			\
      = (long)&(sc_->sc_mach) - new_cfa_;				\
    (FS)->regs.reg[SH_DWARF_FRAME_MACL].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_MACL].loc.offset			\
      = (long)&(sc_->sc_macl) - new_cfa_;				\
									\
     SH_FALLBACK_FRAME_FLOAT_STATE(sc_, (FS), new_cfa_);		\
									\
    /* The unwinder expects the PC to point to the following insn,	\
       whereas the kernel returns the address of the actual		\
       faulting insn.  */						\
    sc_->sc_pc += 2;	  						\
    (FS)->regs.reg[SH_DWARF_FRAME_PC].how = REG_SAVED_OFFSET;		\
    (FS)->regs.reg[SH_DWARF_FRAME_PC].loc.offset			\
      = (long)&(sc_->sc_pc) - new_cfa_;					\
    (FS)->retaddr_column = SH_DWARF_FRAME_PC;				\
    goto SUCCESS;							\
  } while (0)

#endif /* defined (__SH5__) */
#endif /* IN_LIBGCC2 */

/* For SH3 and SH4, we use a slot of the unwind frame which correspond
   to a fake register number 16 as a placeholder for the return address
   in MD_FALLBACK_FRAME_STATE_FOR and its content will be read with
   _Unwind_GetGR which uses dwarf_reg_size_table to get the size of
   the register.  So the entry of dwarf_reg_size_table corresponding to
   this slot must be set.  To do this, we redefine DBX_REGISTER_NUMBER
   so as to return itself for 16.  */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) \
  ((! TARGET_SH5 && (REGNO) == 16) ? 16 : SH_DBX_REGISTER_NUMBER (REGNO))
