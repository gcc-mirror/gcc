/* 
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */
/* Boehm, November 17, 1995 12:13 pm PST */
# include "gc_priv.h"
# include <stdio.h>
# include <setjmp.h>
# if defined(OS2) || defined(CX_UX)
#   define _setjmp(b) setjmp(b)
#   define _longjmp(b,v) longjmp(b,v)
# endif
# ifdef AMIGA
#   include <dos.h>
# endif

#if defined(__MWERKS__) && !defined(POWERPC)

asm static void PushMacRegisters()
{
    sub.w   #4,sp                   // reserve space for one parameter.
    move.l  a2,(sp)
    jsr		GC_push_one
    move.l  a3,(sp)
    jsr		GC_push_one
    move.l  a4,(sp)
    jsr		GC_push_one
#   if !__option(a6frames)
	// <pcb> perhaps a6 should be pushed if stack frames are not being used.    
  	move.l	a6,(sp)
  	jsr		GC_push_one
#   endif
	// skip a5 (globals), a6 (frame pointer), and a7 (stack pointer)
    move.l  d2,(sp)
    jsr		GC_push_one
    move.l  d3,(sp)
    jsr		GC_push_one
    move.l  d4,(sp)
    jsr		GC_push_one
    move.l  d5,(sp)
    jsr		GC_push_one
    move.l  d6,(sp)
    jsr		GC_push_one
    move.l  d7,(sp)
    jsr		GC_push_one
    add.w   #4,sp                   // fix stack.
    rts
}

#endif /* __MWERKS__ */

/* Routine to mark from registers that are preserved by the C compiler. */
/* This must be ported to every new architecture.  There is a generic   */
/* version at the end, that is likely, but not guaranteed to work       */
/* on your architecture.  Run the test_setjmp program to see whether    */
/* there is any chance it will work.                                    */

void GC_push_regs()
{
#       ifdef RT
	  register long TMP_SP; /* must be bound to r11 */
#       endif
#       ifdef VAX
	  /* VAX - generic code below does not work under 4.2 */
	  /* r1 through r5 are caller save, and therefore     */
	  /* on the stack or dead.                            */
	  asm("pushl r11");     asm("calls $1,_GC_push_one");
	  asm("pushl r10"); 	asm("calls $1,_GC_push_one");
	  asm("pushl r9");	asm("calls $1,_GC_push_one");
	  asm("pushl r8");	asm("calls $1,_GC_push_one");
	  asm("pushl r7");	asm("calls $1,_GC_push_one");
	  asm("pushl r6");	asm("calls $1,_GC_push_one");
#       endif
#       if defined(M68K) && (defined(SUNOS4) || defined(NEXT))
	/*  M68K SUNOS - could be replaced by generic code */
	  /* a0, a1 and d1 are caller save          */
	  /*  and therefore are on stack or dead.   */
	
	  asm("subqw #0x4,sp");		/* allocate word on top of stack */

	  asm("movl a2,sp@");	asm("jbsr _GC_push_one");
	  asm("movl a3,sp@");	asm("jbsr _GC_push_one");
	  asm("movl a4,sp@");	asm("jbsr _GC_push_one");
	  asm("movl a5,sp@");	asm("jbsr _GC_push_one");
	  /* Skip frame pointer and stack pointer */
	  asm("movl d1,sp@");	asm("jbsr _GC_push_one");
	  asm("movl d2,sp@");	asm("jbsr _GC_push_one");
	  asm("movl d3,sp@");	asm("jbsr _GC_push_one");
	  asm("movl d4,sp@");	asm("jbsr _GC_push_one");
	  asm("movl d5,sp@");	asm("jbsr _GC_push_one");
	  asm("movl d6,sp@");	asm("jbsr _GC_push_one");
	  asm("movl d7,sp@");	asm("jbsr _GC_push_one");

	  asm("addqw #0x4,sp");		/* put stack back where it was	*/
#       endif

#       if defined(M68K) && defined(HP)
	/*  M68K HP - could be replaced by generic code */
	  /* a0, a1 and d1 are caller save.  */
	
	  asm("subq.w &0x4,%sp");	/* allocate word on top of stack */

	  asm("mov.l %a2,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %a3,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %a4,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %a5,(%sp)"); asm("jsr _GC_push_one");
	  /* Skip frame pointer and stack pointer */
	  asm("mov.l %d1,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %d2,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %d3,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %d4,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %d5,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %d6,(%sp)"); asm("jsr _GC_push_one");
	  asm("mov.l %d7,(%sp)"); asm("jsr _GC_push_one");

	  asm("addq.w &0x4,%sp");	/* put stack back where it was	*/
#       endif /* M68K HP */

#       ifdef AMIGA
	/*  AMIGA - could be replaced by generic code 			*/
	  /* a0, a1, d0 and d1 are caller save */
	  GC_push_one(getreg(REG_A2));
	  GC_push_one(getreg(REG_A3));
	  GC_push_one(getreg(REG_A4));
	  GC_push_one(getreg(REG_A5));
	  GC_push_one(getreg(REG_A6));
	  /* Skip stack pointer */
	  GC_push_one(getreg(REG_D2));
	  GC_push_one(getreg(REG_D3));
	  GC_push_one(getreg(REG_D4));
	  GC_push_one(getreg(REG_D5));
	  GC_push_one(getreg(REG_D6));
	  GC_push_one(getreg(REG_D7));
#       endif

#	if defined(M68K) && defined(MACOS)
#	if defined(THINK_C)
#         define PushMacReg(reg) \
              move.l  reg,(sp) \
              jsr             GC_push_one
	  asm {
              sub.w   #4,sp                   ; reserve space for one parameter.
              PushMacReg(a2);
              PushMacReg(a3);
              PushMacReg(a4);
              ; skip a5 (globals), a6 (frame pointer), and a7 (stack pointer)
              PushMacReg(d2);
              PushMacReg(d3);
              PushMacReg(d4);
              PushMacReg(d5);
              PushMacReg(d6);
              PushMacReg(d7);
              add.w   #4,sp                   ; fix stack.
	  }
#	  undef PushMacReg
#	endif /* THINK_C */
#	if defined(__MWERKS__)
	  PushMacRegisters();
#	endif	/* __MWERKS__ */
#   endif	/* MACOS */

#       if defined(I386) &&!defined(OS2) &&!defined(SVR4) &&!defined(MSWIN32) \
	&& !defined(SCO) && !defined(SCO_ELF) && !(defined(LINUX) \
	&& defined(__ELF__)) && !defined(DOS4GW) && !defined(FREEBSD)
	/* I386 code, generic code does not appear to work */
	/* It does appear to work under OS2, and asms dont */
	/* This is used for some 38g UNIX variants and for CYGWIN32 */
	  asm("pushl %eax");  asm("call _GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ecx");  asm("call _GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %edx");  asm("call _GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ebp");  asm("call _GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %esi");  asm("call _GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %edi");  asm("call _GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ebx");  asm("call _GC_push_one"); asm("addl $4,%esp");
#       endif

#	if defined(I386) && (defined(LINUX) || defined(FREEBSD)) && defined(__ELF__)
	/* This is modified for Linux/FreeBSD with ELF (Note: _ELF_ only) */
	  asm("pushl %eax");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ecx");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %edx");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ebp");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %esi");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %edi");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ebx");  asm("call GC_push_one"); asm("addl $4,%esp");
#	endif

#       if defined(I386) && defined(MSWIN32) && !defined(USE_GENERIC)
	/* I386 code, Microsoft variant		*/
	  __asm  push eax
	  __asm  call GC_push_one
	  __asm  add esp,4
	  __asm  push ebx
	  __asm  call GC_push_one
	  __asm  add esp,4
	  __asm  push ecx
	  __asm  call GC_push_one
	  __asm  add esp,4
	  __asm  push edx
	  __asm  call GC_push_one
	  __asm  add esp,4
	  __asm  push ebp
	  __asm  call GC_push_one
	  __asm  add esp,4
	  __asm  push esi
	  __asm  call GC_push_one
	  __asm  add esp,4
	  __asm  push edi
	  __asm  call GC_push_one
	  __asm  add esp,4
#       endif

#       if defined(I386) && (defined(SVR4) || defined(SCO) || defined(SCO_ELF))
	/* I386 code, SVR4 variant, generic code does not appear to work */
	  asm("pushl %eax");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ebx");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ecx");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %edx");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %ebp");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %esi");  asm("call GC_push_one"); asm("addl $4,%esp");
	  asm("pushl %edi");  asm("call GC_push_one"); asm("addl $4,%esp");
#       endif

#       ifdef NS32K
	  asm ("movd r3, tos"); asm ("bsr ?_GC_push_one"); asm ("adjspb $-4");
	  asm ("movd r4, tos"); asm ("bsr ?_GC_push_one"); asm ("adjspb $-4");
	  asm ("movd r5, tos"); asm ("bsr ?_GC_push_one"); asm ("adjspb $-4");
	  asm ("movd r6, tos"); asm ("bsr ?_GC_push_one"); asm ("adjspb $-4");
	  asm ("movd r7, tos"); asm ("bsr ?_GC_push_one"); asm ("adjspb $-4");
#       endif

#       ifdef SPARC
	  {
	      word GC_save_regs_in_stack();
	      
	      /* generic code will not work */
	      (void)GC_save_regs_in_stack();
	  }
#       endif

#	ifdef RT
	    GC_push_one(TMP_SP);    /* GC_push_one from r11 */

	    asm("cas r11, r6, r0"); GC_push_one(TMP_SP);	/* r6 */
	    asm("cas r11, r7, r0"); GC_push_one(TMP_SP);	/* through */
	    asm("cas r11, r8, r0"); GC_push_one(TMP_SP);	/* r10 */
	    asm("cas r11, r9, r0"); GC_push_one(TMP_SP);
	    asm("cas r11, r10, r0"); GC_push_one(TMP_SP);

	    asm("cas r11, r12, r0"); GC_push_one(TMP_SP); /* r12 */
	    asm("cas r11, r13, r0"); GC_push_one(TMP_SP); /* through */
	    asm("cas r11, r14, r0"); GC_push_one(TMP_SP); /* r15 */
	    asm("cas r11, r15, r0"); GC_push_one(TMP_SP);
#       endif

#       if defined(M68K) && defined(SYSV)
  	/*  Once again similar to SUN and HP, though setjmp appears to work.
  		--Parag
  	 */
#        ifdef __GNUC__
  	  asm("subqw #0x4,%sp");	/* allocate word on top of stack */
  
  	  asm("movl %a2,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %a3,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %a4,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %a5,%sp@");	asm("jbsr GC_push_one");
  	  /* Skip frame pointer and stack pointer */
  	  asm("movl %d1,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %d2,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %d3,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %d4,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %d5,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %d6,%sp@");	asm("jbsr GC_push_one");
  	  asm("movl %d7,%sp@");	asm("jbsr GC_push_one");
  
  	  asm("addqw #0x4,%sp");	/* put stack back where it was	*/
#        else /* !__GNUC__*/
  	  asm("subq.w &0x4,%sp");	/* allocate word on top of stack */
  
  	  asm("mov.l %a2,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %a3,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %a4,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %a5,(%sp)"); asm("jsr GC_push_one");
  	  /* Skip frame pointer and stack pointer */
  	  asm("mov.l %d1,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %d2,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %d3,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %d4,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %d5,(%sp)"); asm("jsr GC_push_one");
   	  asm("mov.l %d6,(%sp)"); asm("jsr GC_push_one");
  	  asm("mov.l %d7,(%sp)"); asm("jsr GC_push_one");
  
  	  asm("addq.w &0x4,%sp");	/* put stack back where it was	*/
#        endif /* !__GNUC__ */
#       endif /* M68K/SYSV */


#     if defined(HP_PA) || defined(M88K) || defined(POWERPC) || (defined(I386) && (defined(OS2) || defined(USE_GENERIC))) || defined(UTS4)
	/* Generic code                          */
	/* The idea is due to Parag Patel at HP. */
	/* We're not sure whether he would like  */
	/* to be he acknowledged for it or not.  */
	{
	    static jmp_buf regs;
	    register word * i = (word *) regs;
	    register ptr_t lim = (ptr_t)(regs) + (sizeof regs);

	    /* Setjmp on Sun 3s doesn't clear all of the buffer.  */
	    /* That tends to preserve garbage.  Clear it.         */
		for (; (char *)i < lim; i++) {
		    *i = 0;
		}
#	    if defined(POWERPC) || defined(MSWIN32) || defined(UTS4)
		(void) setjmp(regs);
#	    else
	        (void) _setjmp(regs);
#	    endif
	    GC_push_all_stack((ptr_t)regs, lim);
	}
#     endif

      /* other machines... */
#       if !(defined M68K) && !(defined VAX) && !(defined RT) 
#	if !(defined SPARC) && !(defined I386) && !(defined NS32K)
#	if !defined(HP_PA) && !defined(M88K) && !defined(POWERPC)
#	if !defined(UTS4)
	    --> bad news <--
# 	endif
#       endif
#       endif
#       endif
}

/* On register window machines, we need a way to force registers into 	*/
/* the stack.	Return sp.						*/
# ifdef SPARC
    asm("	.seg 	\"text\"");
#   ifdef SVR4
      asm("	.globl	GC_save_regs_in_stack");
      asm("GC_save_regs_in_stack:");
      asm("	.type GC_save_regs_in_stack,#function");
#   else
      asm("	.globl	_GC_save_regs_in_stack");
      asm("_GC_save_regs_in_stack:");
#   endif
    asm("	ta	0x3   ! ST_FLUSH_WINDOWS");
    asm("	mov	%sp,%o0");
    asm("	retl");
    asm("	nop");
#   ifdef SVR4
      asm("	.GC_save_regs_in_stack_end:");
      asm("	.size GC_save_regs_in_stack,.GC_save_regs_in_stack_end-GC_save_regs_in_stack");
#   endif
#   ifdef LINT
	word GC_save_regs_in_stack() { return(0 /* sp really */);}
#   endif
# endif


/* GC_clear_stack_inner(arg, limit) clears stack area up to limit and	*/
/* returns arg.  Stack clearing is crucial on SPARC, so we supply	*/
/* an assembly version that's more careful.  Assumes limit is hotter	*/
/* than sp, and limit is 8 byte aligned.				*/
#if defined(ASM_CLEAR_CODE) && !defined(THREADS)
#ifndef SPARC
	--> fix it
#endif
# ifdef SUNOS4
    asm(".globl _GC_clear_stack_inner");
    asm("_GC_clear_stack_inner:");
# else
    asm(".globl GC_clear_stack_inner");
    asm("GC_clear_stack_inner:");
    asm(".type GC_save_regs_in_stack,#function");
# endif
  asm("mov %sp,%o2");		/* Save sp	*/
  asm("add %sp,-8,%o3");	/* p = sp-8	*/
  asm("clr %g1");		/* [g0,g1] = 0	*/
  asm("add %o1,-0x60,%sp");	/* Move sp out of the way,	*/
  				/* so that traps still work.	*/
  				/* Includes some extra words	*/
  				/* so we can be sloppy below.	*/
  asm("loop:");
  asm("std %g0,[%o3]");		/* *(long long *)p = 0	*/
  asm("cmp %o3,%o1");
  asm("bgu loop	");		/* if (p > limit) goto loop	*/
    asm("add %o3,-8,%o3");	/* p -= 8 (delay slot) */
  asm("retl");
    asm("mov %o2,%sp");		/* Restore sp., delay slot	*/
  /* First argument = %o0 = return value */
#   ifdef SVR4
      asm("	.GC_clear_stack_inner_end:");
      asm("	.size GC_clear_stack_inner,.GC_clear_stack_inner_end-GC_clear_stack_inner");
#   endif
  
# ifdef LINT
    /*ARGSUSED*/
    ptr_t GC_clear_stack_inner(arg, limit)
    ptr_t arg; word limit;
    { return(arg); }
# endif
#endif  
