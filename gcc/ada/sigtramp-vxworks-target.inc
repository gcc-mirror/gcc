/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                      S I G T R A M P - T A R G E T                       *
 *                                                                          *
 *                     Asm Implementation Include File                      *
 *                                                                          *
 *         Copyright (C) 2011-2018, Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * In particular,  you can freely  distribute your programs  built with the *
 * GNAT Pro compiler, including any required library run-time units,  using *
 * any licensing terms  of your choosing.  See the AdaCore Software License *
 * for full details.                                                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/***************************************************************
 * VxWorks target specific part of the __gnat_sigtramp service *
 ***************************************************************/

/* Note: This target specific part is kept in a separate file to avoid
   duplication of its code for the vxworks and vxworks-vxsim asm
   implementation files.  */

/* ---------------------------
   -- And now the asm stubs --
   ---------------------------

   They all have a common structure with blocks of asm sequences queued one
   after the others.  Typically:

   SYMBOL_START

   CFI_DIRECTIVES
     CFI_DEF_CFA,
     CFI_COMMON_REGISTERS,
     ...

   STUB_BODY
     asm code to establish frame, setup the cfa reg value,
     call the real signal handler, ...

   SYMBOL_END
*/

/*--------------------------------
  -- Misc constants and helpers --
  -------------------------------- */

/* asm string construction helpers.  */

#define STR(TEXT) #TEXT
/* stringify expanded TEXT, surrounding it with double quotes.  */

#define S(E) STR(E)
/* stringify E, which will resolve as text but may contain macros
   still to be expanded.  */

/* asm (TEXT) outputs <tab>TEXT. These facilitate the output of
   multine contents:  */
#define TAB(S) "\t" S
#define CR(S)  S "\n"

#undef TCR
#define TCR(S) TAB(CR(S))

/* REGNO constants, dwarf column numbers for registers of interest.  */

#if defined (__PPC__)

#define REGNO_LR  65
#define REGNO_CTR 66
#define REGNO_CR  70
#define REGNO_XER 76
#define REGNO_GR(N) (N)

#define REGNO_PC  67  /* ARG_POINTER_REGNUM  */

#define FUNCTION "@function"

#elif defined (ARMEL)

#define REGNO_G_REG_OFFSET(N) (N)

#define FUNCTION "%function"

#ifdef __aarch64__
#define REGNO_PC_OFFSET  80  /* aka V16, a scratch register */
#else
#define REGNO_PC_OFFSET  15  /* PC_REGNUM */
#endif

/* Mapping of CFI Column, Gcc Regno, Signal context offset for _LP64

   Name	   CFI	   GCC	   SCTX
   G0-G30  0-30    0-30    0-30
   SP      31      31      31
   PC                      32
   V0-V31  64-95   32-63   N/A

*/

#elif defined (i386)

/* These are the cfi colunm numbers */

#define REGNO_EDI 7
#define REGNO_ESI 6
#define REGNO_EBP 5
#define REGNO_ESP 4
#define REGNO_EBX 3
#define REGNO_EDX 2
#define REGNO_ECX 1
#define REGNO_EAX 0
#define REGNO_EFLAGS 9
#define REGNO_SET_PC 8 /* aka %eip */

#define FUNCTION "@function"

/* Mapping of CFI Column, Gcc Regno, Signal context offset for 32bit

   Name	   CFI	   GCC	   SCTX
   %eax	    0	    0	    7
   %ecx	    1	    2	    6
   %edx	    2	    1	    5
   %ebx     3	    3	    4
   %esp	    4	    7	    3
   %ebp	    5	    6	    2
   %esi	    6	    4	    1
   %edi	    7	    5	    0
   %eflags  9	   17 	    8
   %eip	    8	  n/a	    9


   In general:
   There is no unique numbering for the x86 architecture.  It's parameterized
   by DWARF_FRAME_REGNUM, which is DBX_REGISTER_NUMBER except for Windows, and
   the latter depends on the platform.
*/

#elif defined (__x86_64__)

/* These are the cfi colunm numbers */

#define REGNO_RAX 0
#define REGNO_RDX 1
#define REGNO_RCX 2
#define REGNO_RBX 3
#define REGNO_RSI 4
#define REGNO_RDI 5
#define REGNO_RBP 6
#define REGNO_RSP 7
#define REGNO_R8 8
#define REGNO_R9 9
#define REGNO_R10 10
#define REGNO_R11 11
#define REGNO_R12 12
#define REGNO_R13 13
#define REGNO_R14 14
#define REGNO_R15 15
#define REGNO_RPC 16 /* aka %rip */
#define REGNO_EFLAGS 49
#define REGNO_FS 54

#define FUNCTION "@function"

#else
Not_implemented;
#endif /* REGNO constants */


/*------------------------------
  -- Stub construction blocks --
  ------------------------------ */

/* CFA setup block
   ---------------
   Only non-volatile registers are suitable for a CFA base. These are the
   only ones we can expect to be able retrieve from the unwinding context
   while walking up the chain, saved by at least the bottom-most exception
   propagation services.  We set a non-volatile register to the value we
   need in the stub body that follows.  */

#if defined (__PPC__)

/* Use r15 for PPC.  Note that r14 is inappropriate here, even though it
   is non-volatile according to the ABI, because GCC uses it as an extra
   SCRATCH on SPE targets.  */

#define CFA_REG 15

#elif defined (ARMEL)

#ifdef __aarch64__
#define CFA_REG 19
#else
/* Use r8 for ARM.  Any of r4-r8 should work.  */
#define CFA_REG 8
#endif

#elif defined (i386)

#define CFA_REG 7

#elif defined (__x86_64__)

/* R15 register */
#define CFA_REG 15

#else
Not_implemented;
#endif /* CFA setup block */

#define CFI_DEF_CFA \
CR(".cfi_def_cfa " S(CFA_REG) ", 0")

/* Register location blocks
   ------------------------
   Rules to find registers of interest from the CFA. This should comprise
   all the non-volatile registers relevant to the interrupted context.

   Note that we include r1 in this set, unlike the libgcc unwinding
   fallbacks.  This is useful for fallbacks to allow the use of r1 in CFI
   expressions and the absence of rule for r1 gets compensated by using the
   target CFA instead.  We don't need the expression facility here and
   setup a fake CFA to allow very simple offset expressions, so having a
   rule for r1 is the proper thing to do.  We for sure have observed
   crashes in some cases without it.  */

#if defined (__PPC__)

#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_SET_##REG)

#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(GR(0)))  \
TCR(COMMON_CFI(GR(1)))  \
TCR(COMMON_CFI(GR(2)))  \
TCR(COMMON_CFI(GR(3)))  \
TCR(COMMON_CFI(GR(4)))  \
TCR(COMMON_CFI(GR(5)))  \
TCR(COMMON_CFI(GR(6)))  \
TCR(COMMON_CFI(GR(7)))  \
TCR(COMMON_CFI(GR(8)))  \
TCR(COMMON_CFI(GR(9)))  \
TCR(COMMON_CFI(GR(10)))  \
TCR(COMMON_CFI(GR(11)))  \
TCR(COMMON_CFI(GR(12)))  \
TCR(COMMON_CFI(GR(13)))  \
TCR(COMMON_CFI(GR(14))) \
TCR(COMMON_CFI(GR(15))) \
TCR(COMMON_CFI(GR(16))) \
TCR(COMMON_CFI(GR(17))) \
TCR(COMMON_CFI(GR(18))) \
TCR(COMMON_CFI(GR(19))) \
TCR(COMMON_CFI(GR(20))) \
TCR(COMMON_CFI(GR(21))) \
TCR(COMMON_CFI(GR(22))) \
TCR(COMMON_CFI(GR(23))) \
TCR(COMMON_CFI(GR(24))) \
TCR(COMMON_CFI(GR(25))) \
TCR(COMMON_CFI(GR(26))) \
TCR(COMMON_CFI(GR(27))) \
TCR(COMMON_CFI(GR(28))) \
TCR(COMMON_CFI(GR(29))) \
TCR(COMMON_CFI(GR(30))) \
TCR(COMMON_CFI(GR(31))) \
TCR(COMMON_CFI(LR)) \
TCR(COMMON_CFI(CR)) \
TCR(COMMON_CFI(CTR)) \
TCR(COMMON_CFI(XER)) \
TCR(COMMON_CFI(PC)) \
TCR(".cfi_return_column " S(REGNO_PC))

/* Trampoline body block
   ---------------------  */

#if !defined (__PPC64__)
#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("stwu %r1,-16(%r1)")  \
TCR("mflr %r0")	\
TCR("stw %r0,20(%r1)")	\
TCR("stw %r" S(CFA_REG) ",8(%r1)")	\
TCR("")			\
TCR("# Setup CFA_REG = context, which we'll retrieve as our CFA value") \
TCR("mr %r" S(CFA_REG) ", %r7") \
TCR("")			\
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received in r3, r4 and r5") \
TCR("mtctr %r6") \
TCR("bctrl")	\
TCR("")		\
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("lwz %r" S(CFA_REG) ",8(%r1)")	\
TCR("lwz %r0,20(%r1)")	\
TCR("mtlr %r0")		\
TCR("")			\
TCR("addi %r1,%r1,16")	\
TCR("blr")
#else
#define SIGTRAMP_BODY \
CR("") \
TCR("0:") \
TCR("addis 2,12,.TOC.-0@ha") \
TCR("addi 2,2,.TOC.-0@l") \
TCR(".localentry	__gnat_sigtramp_common,.-__gnat_sigtramp_common") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("mflr %r0")	\
TCR("std %r0,16(%r1)")	\
TCR("stdu %r1,-32(%r1)")  \
TCR("std %r2,24(%r1)")	\
TCR("std %r" S(CFA_REG) ",8(%r1)")	\
TCR("")			\
TCR("# Setup CFA_REG = context, which we'll retrieve as our CFA value") \
TCR("mr %r" S(CFA_REG) ", %r7") \
TCR("")			\
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received in r3, r4 and r5") \
TCR("mr %r12,%r6") \
TCR("mtctr %r6") \
TCR("bctrl")	\
TCR("")		\
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("ld %r" S(CFA_REG) ",8(%r1)")	\
TCR("ld %r2,24(%r1)")	\
TCR("addi %r1,%r1,32")  \
TCR("ld %r0,16(%r1)")	\
TCR("mtlr %r0")		\
TCR("blr")
#endif

#elif defined (ARMEL)

#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_SET_##REG)

#ifdef __aarch64__
#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(G_REG_OFFSET(0)))  \
TCR(COMMON_CFI(G_REG_OFFSET(1)))  \
TCR(COMMON_CFI(G_REG_OFFSET(2)))  \
TCR(COMMON_CFI(G_REG_OFFSET(3)))  \
TCR(COMMON_CFI(G_REG_OFFSET(4)))  \
TCR(COMMON_CFI(G_REG_OFFSET(5)))  \
TCR(COMMON_CFI(G_REG_OFFSET(6)))  \
TCR(COMMON_CFI(G_REG_OFFSET(7)))  \
TCR(COMMON_CFI(G_REG_OFFSET(8)))  \
TCR(COMMON_CFI(G_REG_OFFSET(9)))  \
TCR(COMMON_CFI(G_REG_OFFSET(10)))  \
TCR(COMMON_CFI(G_REG_OFFSET(11)))  \
TCR(COMMON_CFI(G_REG_OFFSET(12)))  \
TCR(COMMON_CFI(G_REG_OFFSET(13)))  \
TCR(COMMON_CFI(G_REG_OFFSET(14))) \
TCR(COMMON_CFI(G_REG_OFFSET(15))) \
TCR(COMMON_CFI(G_REG_OFFSET(16))) \
TCR(COMMON_CFI(G_REG_OFFSET(17))) \
TCR(COMMON_CFI(G_REG_OFFSET(18))) \
TCR(COMMON_CFI(G_REG_OFFSET(19))) \
TCR(COMMON_CFI(G_REG_OFFSET(20))) \
TCR(COMMON_CFI(G_REG_OFFSET(21))) \
TCR(COMMON_CFI(G_REG_OFFSET(22))) \
TCR(COMMON_CFI(G_REG_OFFSET(23))) \
TCR(COMMON_CFI(G_REG_OFFSET(24))) \
TCR(COMMON_CFI(G_REG_OFFSET(25))) \
TCR(COMMON_CFI(G_REG_OFFSET(26))) \
TCR(COMMON_CFI(G_REG_OFFSET(27))) \
TCR(COMMON_CFI(G_REG_OFFSET(28))) \
TCR(COMMON_CFI(G_REG_OFFSET(29))) \
TCR(COMMON_CFI(G_REG_OFFSET(30))) \
TCR(COMMON_CFI(PC_OFFSET)) \
TCR(".cfi_return_column " S(REGNO_PC_OFFSET))
#else
#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(G_REG_OFFSET(0)))  \
TCR(COMMON_CFI(G_REG_OFFSET(1)))  \
TCR(COMMON_CFI(G_REG_OFFSET(2)))  \
TCR(COMMON_CFI(G_REG_OFFSET(3)))  \
TCR(COMMON_CFI(G_REG_OFFSET(4)))  \
TCR(COMMON_CFI(G_REG_OFFSET(5)))  \
TCR(COMMON_CFI(G_REG_OFFSET(6)))  \
TCR(COMMON_CFI(G_REG_OFFSET(7)))  \
TCR(COMMON_CFI(G_REG_OFFSET(8)))  \
TCR(COMMON_CFI(G_REG_OFFSET(9)))  \
TCR(COMMON_CFI(G_REG_OFFSET(10)))  \
TCR(COMMON_CFI(G_REG_OFFSET(11)))  \
TCR(COMMON_CFI(G_REG_OFFSET(12)))  \
TCR(COMMON_CFI(G_REG_OFFSET(13)))  \
TCR(COMMON_CFI(G_REG_OFFSET(14))) \
TCR(COMMON_CFI(PC_OFFSET)) \
TCR(".cfi_return_column " S(REGNO_PC_OFFSET))
#endif

/* Trampoline body block
   ---------------------  */
#ifdef __aarch64__
#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate the frame (16bytes aligned) and push FP and LR") \
TCR("stp x29, x30, [sp, #-32]!") \
TCR("add x29, sp, 0") \
TCR("# Store register used to hold the CFA on stack (pro forma)") \
TCR("str x" S(CFA_REG) ", [sp, 16]")  \
TCR("# Set the CFA reg from the 5th arg") \
TCR("mov x" S(CFA_REG) ", x4") \
TCR("# Call the handler") \
TCR("blr x3") \
TCR("# Release our frame and return (should never get here!).") \
TCR("ldr x" S(CFA_REG) ", [sp, 16]") \
TCR("ldp x29, x30, [sp], 32") \
TCR("ret")
#else
#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("mov	ip, sp") \
TCR("stmfd	sp!, {r"S(CFA_REG)", fp, ip, lr, pc}") \
TCR("# Setup CFA_REG = context, which we'll retrieve as our CFA value") \
TCR("ldr	r"S(CFA_REG)", [ip]") \
TCR("")                 \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received in r0, r1 and r2") \
TCR("sub	fp, ip, #4") \
TCR("blx	r3") \
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("ldmfd	sp, {r"S(CFA_REG)", fp, sp, pc}")
#endif

#elif defined (i386)

#if CPU == SIMNT || CPU == SIMPENTIUM || CPU == SIMLINUX
#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_SET_##REG)
#else
#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_##REG)
#endif

#define PC_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_##REG)

#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(EDI)) \
TCR(COMMON_CFI(ESI)) \
TCR(COMMON_CFI(EBP)) \
TCR(COMMON_CFI(ESP)) \
TCR(COMMON_CFI(EBX)) \
TCR(COMMON_CFI(EDX)) \
TCR(COMMON_CFI(ECX)) \
TCR(COMMON_CFI(EAX)) \
TCR(COMMON_CFI(EFLAGS)) \
TCR(PC_CFI(SET_PC)) \
TCR(".cfi_return_column " S(REGNO_SET_PC))

/* Trampoline body block
   ---------------------  */

#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("pushl	%ebp") \
TCR("movl	%esp, %ebp") \
TCR("pushl	%edi") \
TCR("subl	$24, %esp") \
TCR("# Setup CFA_REG = context, which we'll retrieve as our CFA value") \
TCR("movl	24(%ebp), %edi") \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received") \
TCR("movl	16(%ebp), %eax") \
TCR("movl	%eax, 8(%esp)") \
TCR("movl	12(%ebp), %eax") \
TCR("movl	%eax, 4(%esp)") \
TCR("movl	8(%ebp), %eax") \
TCR("movl	%eax, (%esp)") \
TCR("call	*20(%ebp)") \
TCR("# Restore our callee-saved items, release our frame and return") \
TCR("popl	%edi") \
TCR("leave") \
TCR("ret")

#elif defined (__x86_64__)

#define COMMON_CFI(REG) \
  ".cfi_offset " S(REGNO_##REG) "," S(REG_##REG)

#define CFI_COMMON_REGS \
CR("# CFI for common registers\n") \
TCR(COMMON_CFI(R15)) \
TCR(COMMON_CFI(R14)) \
TCR(COMMON_CFI(R13)) \
TCR(COMMON_CFI(R12)) \
TCR(COMMON_CFI(R11)) \
TCR(COMMON_CFI(R10)) \
TCR(COMMON_CFI(R9)) \
TCR(COMMON_CFI(R8)) \
TCR(COMMON_CFI(RDI)) \
TCR(COMMON_CFI(RSI)) \
TCR(COMMON_CFI(RBP)) \
TCR(COMMON_CFI(RSP)) \
TCR(COMMON_CFI(RBX)) \
TCR(COMMON_CFI(RDX)) \
TCR(COMMON_CFI(RCX)) \
TCR(COMMON_CFI(RAX)) \
TCR(COMMON_CFI(RPC)) \
TCR(".cfi_return_column " S(REGNO_RPC))

/* Trampoline body block
   ---------------------  */

#define SIGTRAMP_BODY \
CR("") \
TCR("# Allocate frame and save the non-volatile") \
TCR("# registers we're going to modify") \
TCR("subq	$8, %rsp") \
TCR("# Setup CFA_REG = context, which we'll retrieve as our CFA value") \
TCR("movq	%r8, %r15") \
TCR("# Call the real handler. The signo, siginfo and sigcontext") \
TCR("# arguments are the same as those we received") \
TCR("call	*%rcx") \
TCR("# This part should never be executed") \
TCR("addq	$8, %rsp") \
TCR("ret")

#else
Not_implemented;
#endif /* CFI_COMMON_REGS and SIGTRAMP_BODY */

/* Symbol definition block
   -----------------------  */

#ifdef __x86_64__
#define FUNC_ALIGN TCR(".p2align 4,,15")
#else
#define FUNC_ALIGN
#endif

#define SIGTRAMP_START(SYM) \
CR("# " S(SYM) " cfi trampoline") \
TCR(".type " S(SYM) ", "FUNCTION) \
CR("") \
FUNC_ALIGN \
CR(S(SYM) ":") \
TCR(".cfi_startproc") \
TCR(".cfi_signal_frame")

/* Symbol termination block
   ------------------------  */

#define SIGTRAMP_END(SYM) \
CR(".cfi_endproc") \
TCR(".size " S(SYM) ", .-" S(SYM))

/*----------------------------
  -- And now, the real code --
  ---------------------------- */

/* Text section start.  The compiler isn't aware of that switch.  */

asm (".text\n"
     TCR(".align 2"));
