/* The start module crt0.s for the SysV68 Motorola 3300 Delta Series.
   Copyright (C) 1996 Free Software Foundation, Inc.
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

	FILE ("crt0.s")
	TEXT
	GLOBAL_SYM (_start)
_start:	mov.l	%d0,splimit%
	subq.w	&8,%sp
	mov.l	8(%sp),(%sp)
	lea	12(%sp),%a0
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
	
	subq.w	&8,%sp
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

	jsr	main
	mov.l	%d0,(%sp)
	jsr	exit
	moveq.l	&1,%d0
	trap	&0
	nop

	GLOBAL_SYM (__stop_monitor)
__stop_monitor:
	rts
	EVEN

	COMM	splimit%,4
	COMM	environ,4

	IDENT ("$Id: mot3300-crt0.S,v 1.1.1.2 1998/01/14 19:39:06 law Exp $")
	IDENT ("Contributed by Manfred Hollstein (manfred@lts.sel.alcatel.de)")
	IDENT ("Corrections by Philippe De Muyter (phdm@macqel.be)")
