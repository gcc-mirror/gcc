/* Special varargs support for i860.
   Copyright (C) 2001  Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#if defined(__svr4__) || defined(__alliant__)
	.text
	.align	4

/* The Alliant needs the added underscore.  */
	.globl	__builtin_saveregs
__builtin_saveregs:
	.globl	___builtin_saveregs
___builtin_saveregs:

	andnot	0x0f,%sp,%sp	/* round down to 16-byte boundary */
	adds	-96,%sp,%sp  /* allocate stack space for reg save
			   area and also for a new va_list
			   structure */
	/* Save all argument registers in the arg reg save area.  The
	   arg reg save area must have the following layout (according
	   to the svr4 ABI):

	struct {
	  union  {
	    float freg[8];
	    double dreg[4];
	  } float_regs;
	  long	ireg[12];
	};
	*/

	fst.q	%f8,  0(%sp) /* save floating regs (f8-f15)  */
	fst.q	%f12,16(%sp) 

	st.l	%r16,32(%sp) /* save integer regs (r16-r27) */
	st.l	%r17,36(%sp) 
	st.l	%r18,40(%sp)
	st.l	%r19,44(%sp)
	st.l	%r20,48(%sp)
	st.l	%r21,52(%sp)
	st.l	%r22,56(%sp)
	st.l	%r23,60(%sp)
	st.l	%r24,64(%sp)
	st.l	%r25,68(%sp)
	st.l	%r26,72(%sp)
	st.l	%r27,76(%sp)

	adds	80,%sp,%r16  /* compute the address of the new
			   va_list structure.  Put in into
			   r16 so that it will be returned
			   to the caller.  */

	/* Initialize all fields of the new va_list structure.  This
	   structure looks like:

	typedef struct {
	    unsigned long	ireg_used;
	    unsigned long	freg_used;
	    long	*reg_base;
	    long	*mem_ptr;
	} va_list;
	*/

	st.l	%r0, 0(%r16) /* nfixed */
	st.l	%r0, 4(%r16) /* nfloating */
	st.l    %sp, 8(%r16) /* __va_ctl points to __va_struct.  */
	bri	%r1	/* delayed return */
	st.l	%r28,12(%r16) /* pointer to overflow args */

#else /* not __svr4__ */
#if defined(__PARAGON__)
	/*
	 *	we'll use SVR4-ish varargs but need SVR3.2 assembler syntax,
	 *	and we stand a better chance of hooking into libraries
	 *	compiled by PGI.  [andyp@ssd.intel.com]
	 */
	.text
	.align	4
	.globl	__builtin_saveregs
__builtin_saveregs:
	.globl	___builtin_saveregs
___builtin_saveregs:

	andnot	0x0f,sp,sp	/* round down to 16-byte boundary */
	adds	-96,sp,sp	/* allocate stack space for reg save
			   area and also for a new va_list
			   structure */
	/* Save all argument registers in the arg reg save area.  The
	   arg reg save area must have the following layout (according
	   to the svr4 ABI):

	struct {
	  union  {
	    float freg[8];
	    double dreg[4];
	  } float_regs;
	  long	ireg[12];
	};
	*/

	fst.q	f8,  0(sp)
	fst.q	f12,16(sp) 
	st.l	r16,32(sp)
	st.l	r17,36(sp) 
	st.l	r18,40(sp)
	st.l	r19,44(sp)
	st.l	r20,48(sp)
	st.l	r21,52(sp)
	st.l	r22,56(sp)
	st.l	r23,60(sp)
	st.l	r24,64(sp)
	st.l	r25,68(sp)
	st.l	r26,72(sp)
	st.l	r27,76(sp)

	adds	80,sp,r16  /* compute the address of the new
			   va_list structure.  Put in into
			   r16 so that it will be returned
			   to the caller.  */

	/* Initialize all fields of the new va_list structure.  This
	   structure looks like:

	typedef struct {
	    unsigned long	ireg_used;
	    unsigned long	freg_used;
	    long	*reg_base;
	    long	*mem_ptr;
	} va_list;
	*/

	st.l	r0, 0(r16) /* nfixed */
	st.l	r0, 4(r16) /* nfloating */
	st.l    sp, 8(r16) /* __va_ctl points to __va_struct.  */
	bri	r1	/* delayed return */
	st.l	r28,12(r16) /* pointer to overflow args */
#else /* not __PARAGON__ */
	.text
	.align	4

	.globl	___builtin_saveregs
___builtin_saveregs:
	mov	sp,r30
	andnot	0x0f,sp,sp
	adds	-96,sp,sp  /* allocate sufficient space on the stack */

/* Fill in the __va_struct.  */
	st.l	r16, 0(sp) /* save integer regs (r16-r27) */
	st.l	r17, 4(sp) /* int	fixed[12] */
	st.l	r18, 8(sp)
	st.l	r19,12(sp)
	st.l	r20,16(sp)
	st.l	r21,20(sp)
	st.l	r22,24(sp)
	st.l	r23,28(sp)
	st.l	r24,32(sp)
	st.l	r25,36(sp)
	st.l	r26,40(sp)
	st.l	r27,44(sp)

	fst.q	f8, 48(sp) /* save floating regs (f8-f15) */
	fst.q	f12,64(sp) /* int floating[8] */

/* Fill in the __va_ctl.  */
	st.l    sp, 80(sp) /* __va_ctl points to __va_struct.  */
	st.l	r28,84(sp) /* pointer to more args */
	st.l	r0, 88(sp) /* nfixed */
	st.l	r0, 92(sp) /* nfloating */

	adds	80,sp,r16  /* return address of the __va_ctl.  */
	bri	r1
	mov	r30,sp
		/* recover stack and pass address to start 
		   of data.  */
#endif /* not __PARAGON__ */
#endif /* not __svr4__ */
