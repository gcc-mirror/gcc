/* The start module mcrt0.s for the SysV68 Motorola 3300 Delta Series.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Manfred Hollstein (manfred@lts.sel.alcatel.de).

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

#ifdef MOTOROLA
# define COMM		comm
# define DATA		data
# define EVEN		even
# define FILE(n)	file	n
# define GLOBAL_SYM(s)	global	s
# define LOCAL_LABEL(l)	L%##l
# define IDENT(s)	ident	s
# define TEXT		text
#else /* Assume we are using GNU as.  */
# define COMM		.comm
# define DATA		.data
# define EVEN		.even
# define FILE(name)	.file	name
# define GLOBAL_SYM(s)	.globl	s
# define LOCAL_LABEL(l)	.L.##l
# define IDENT(s)	.section .comment;.asciz s
# define TEXT		.text
#endif

	FILE ("mcrt0.s")
	TEXT
	GLOBAL_SYM (_start)
_start:	mov.l	%d0,splimit%
	subq.w	&8,%sp
	mov.l	8(%sp),(%sp)
	lea	12(%sp),%a0
	mov.l	%a0,___Argv
	mov.l	%a0,4(%sp)
	mov.l	%a0,%a1
LOCAL_LABEL(0):
	tst.l	(%a0)+
	bne.b	LOCAL_LABEL(0)
#ifdef SGS_CMP_ORDER
	cmpa.l	%a0,(%a1)
#else
	cmpa.l	(%a1),%a0
#endif
	blt.b	LOCAL_LABEL(1)
	subq.w	&4,%a0
LOCAL_LABEL(1):
	mov.l	%a0,8(%sp)
	mov.l	%a0,environ
	jsr	initfpu
	
	sub	&8,%sp
	clr.l	%d0		/* if (! isatty (fileno (stderr))) */
	mov.b	_iob+27,%d0
	mov.l	%d0,-(%sp)
	jsr	isatty
	addq.w	&4,%sp
	tst.l	%d0
	bne.b	LOCAL_LABEL(isatty)
	clr.l	-(%sp)		/*	setbuf (stderr, NULL) */
	pea	_iob+28
	jsr	setbuf
	addq.w	&8,%sp
LOCAL_LABEL(isatty):
	addq.w	&8,%sp

	mov.l	&600,-(%sp)
	mov.l	&etext,%d1
	subi.l	&LOCAL_LABEL(endofstart),%d1
	addq.l	&1,%d1
	bclr	&0,%d1
	addi.l	&4812,%d1
	asr.l	&1,%d1
	mov.l	%d1,-(%sp)
	add.l	%d1,%d1
	mov.l	%d1,-(%sp)
	jsr	sbrk
	addq.w	&4,%sp
#ifdef SGS_CMP_ORDER
	cmpa.l	%a0,&-1
#else
	cmpa.l	&-1,%a0
#endif
	beq.b	LOCAL_LABEL(3)
	mov.l	%a0,-(%sp)
	add.l	&12,%a0
	mov.l	%a0,_countbase
	mov.l	&etext,-(%sp)
	mov.l	&LOCAL_LABEL(endofstart),-(%sp)
	jsr	monitor
	lea	20(%sp),%sp
	jsr	main
	mov.l	%d0,(%sp)
	jsr	exit
_exit:	moveq	&1,%d0
	trap	&0

	GLOBAL_SYM (__stop_monitor)
__stop_monitor:
	clr.l	-(%sp)
	jsr	monitor
	add.w	&4,%sp
	rts

LOCAL_LABEL(errtxt):
	byte	'N,'o,' ,'s,'p,'a,'c,'e,' ,'f,'o,'r,' ,'m,'o,'n
	byte	'i,'t,'o,'r,' ,'b,'u,'f,'f,'e,'r,'\n
LOCAL_LABEL(errtxt_end):

	EVEN
LOCAL_LABEL(3):
	pea	LOCAL_LABEL(errtxt_end)-LOCAL_LABEL(errtxt)
	pea	LOCAL_LABEL(errtxt)(%pc)
	pea	2
	jsr	write
	bra.b	_exit
LOCAL_LABEL(endofstart):
	
	EVEN

	COMM	splimit%,4
	COMM	environ,4
	COMM	_countbase,4

	IDENT ("$Id: mot3300Mcrt0.S,v 1.1.1.2 1998/01/14 19:39:08 law Exp $")
	IDENT ("Contributed by Manfred Hollstein (manfred@lts.sel.alcatel.de)")
	IDENT ("Corrections by Philippe De Muyter (phdm@macqel.be)")
