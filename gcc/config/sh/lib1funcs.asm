/* Copyright (C) 1994, 1995 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


!! libgcc1 routines for the Hitachi SH cpu.
!! Contributed by Steve Chamberlain.
!! sac@cygnus.com

!! ashiftrt_r4_x, ___ashrsi3, ___ashlsi3, ___lshrsi3 routines
!! recoded in assembly by Toshiyasu Morita
!! tm@netcom.com

#ifdef L_ashiftrt
	.global	___ashiftrt_r4_0
	.global	___ashiftrt_r4_1
	.global	___ashiftrt_r4_2
	.global	___ashiftrt_r4_3
	.global	___ashiftrt_r4_4
	.global	___ashiftrt_r4_5
	.global	___ashiftrt_r4_6
	.global	___ashiftrt_r4_7
	.global	___ashiftrt_r4_8
	.global	___ashiftrt_r4_9
	.global	___ashiftrt_r4_10
	.global	___ashiftrt_r4_11
	.global	___ashiftrt_r4_12
	.global	___ashiftrt_r4_13
	.global	___ashiftrt_r4_14
	.global	___ashiftrt_r4_15
	.global	___ashiftrt_r4_16
	.global	___ashiftrt_r4_17
	.global	___ashiftrt_r4_18
	.global	___ashiftrt_r4_19
	.global	___ashiftrt_r4_20
	.global	___ashiftrt_r4_21
	.global	___ashiftrt_r4_22
	.global	___ashiftrt_r4_23
	.global	___ashiftrt_r4_24
	.global	___ashiftrt_r4_25
	.global	___ashiftrt_r4_26
	.global	___ashiftrt_r4_27
	.global	___ashiftrt_r4_28
	.global	___ashiftrt_r4_29
	.global	___ashiftrt_r4_30
	.global	___ashiftrt_r4_31
	.global	___ashiftrt_r4_32

	.align	1
___ashiftrt_r4_32:
___ashiftrt_r4_31:
	rotcl	r4
	rts
	subc	r4,r4

___ashiftrt_r4_30:
	shar	r4
___ashiftrt_r4_29:
	shar	r4
___ashiftrt_r4_28:
	shar	r4
___ashiftrt_r4_27:
	shar	r4
___ashiftrt_r4_26:
	shar	r4
___ashiftrt_r4_25:
	shar	r4
___ashiftrt_r4_24:
	shlr16	r4
	shlr8	r4
	rts
	exts.b	r4,r4

___ashiftrt_r4_23:
	shar	r4
___ashiftrt_r4_22:
	shar	r4
___ashiftrt_r4_21:
	shar	r4
___ashiftrt_r4_20:
	shar	r4
___ashiftrt_r4_19:
	shar	r4
___ashiftrt_r4_18:
	shar	r4
___ashiftrt_r4_17:
	shar	r4
___ashiftrt_r4_16:
	shlr16	r4
	rts
	exts.w	r4,r4

___ashiftrt_r4_15:
	shar	r4
___ashiftrt_r4_14:
	shar	r4
___ashiftrt_r4_13:
	shar	r4
___ashiftrt_r4_12:
	shar	r4
___ashiftrt_r4_11:
	shar	r4
___ashiftrt_r4_10:
	shar	r4
___ashiftrt_r4_9:
	shar	r4
___ashiftrt_r4_8:
	shar	r4
___ashiftrt_r4_7:
	shar	r4
___ashiftrt_r4_6:
	shar	r4
___ashiftrt_r4_5:
	shar	r4
___ashiftrt_r4_4:
	shar	r4
___ashiftrt_r4_3:
	shar	r4
___ashiftrt_r4_2:
	shar	r4
___ashiftrt_r4_1:
	rts
	shar	r4

___ashiftrt_r4_0:
	rts
	nop
#endif

#ifdef L_ashiftrt_n

!
! ___ashrsi3
!
! Entry:
!
! r4: Value to shift
! r5: Shifts
!
! Exit:
!
! r0: Result
!
! Destroys:
!
! (none)
!

	.global	___ashrsi3
	.align	2
___ashrsi3:
	mov	#31,r0
	cmp/hi	r0,r5
	bt	L_ashrsi3_31
	mova	L_ashrsi3_table,r0
	mov.b	@(r0,r5),r5
	add	r5,r0		! Change to braf when gas is fixed
	jmp	@r0
	mov	r4,r0

L_ashrsi3_table:
	.byte		L_ashrsi3_0-L_ashrsi3_table
	.byte		L_ashrsi3_1-L_ashrsi3_table
	.byte		L_ashrsi3_2-L_ashrsi3_table
	.byte		L_ashrsi3_3-L_ashrsi3_table
	.byte		L_ashrsi3_4-L_ashrsi3_table
	.byte		L_ashrsi3_5-L_ashrsi3_table
	.byte		L_ashrsi3_6-L_ashrsi3_table
	.byte		L_ashrsi3_7-L_ashrsi3_table
	.byte		L_ashrsi3_8-L_ashrsi3_table
	.byte		L_ashrsi3_9-L_ashrsi3_table
	.byte		L_ashrsi3_10-L_ashrsi3_table
	.byte		L_ashrsi3_11-L_ashrsi3_table
	.byte		L_ashrsi3_12-L_ashrsi3_table
	.byte		L_ashrsi3_13-L_ashrsi3_table
	.byte		L_ashrsi3_14-L_ashrsi3_table
	.byte		L_ashrsi3_15-L_ashrsi3_table
	.byte		L_ashrsi3_16-L_ashrsi3_table
	.byte		L_ashrsi3_17-L_ashrsi3_table
	.byte		L_ashrsi3_18-L_ashrsi3_table
	.byte		L_ashrsi3_19-L_ashrsi3_table
	.byte		L_ashrsi3_20-L_ashrsi3_table
	.byte		L_ashrsi3_21-L_ashrsi3_table
	.byte		L_ashrsi3_22-L_ashrsi3_table
	.byte		L_ashrsi3_23-L_ashrsi3_table
	.byte		L_ashrsi3_24-L_ashrsi3_table
	.byte		L_ashrsi3_25-L_ashrsi3_table
	.byte		L_ashrsi3_26-L_ashrsi3_table
	.byte		L_ashrsi3_27-L_ashrsi3_table
	.byte		L_ashrsi3_28-L_ashrsi3_table
	.byte		L_ashrsi3_29-L_ashrsi3_table
	.byte		L_ashrsi3_30-L_ashrsi3_table
	.byte		L_ashrsi3_31-L_ashrsi3_table

L_ashrsi3_31:
	rotcl	r0
	rts
	subc	r0,r0

L_ashrsi3_30:
	shar	r0
L_ashrsi3_29:
	shar	r0
L_ashrsi3_28:
	shar	r0
L_ashrsi3_27:
	shar	r0
L_ashrsi3_26:
	shar	r0
L_ashrsi3_25:
	shar	r0
L_ashrsi3_24:
	shlr16	r0
	shlr8	r0
	rts
	exts.b	r0,r0

L_ashrsi3_23:
	shar	r0
L_ashrsi3_22:
	shar	r0
L_ashrsi3_21:
	shar	r0
L_ashrsi3_20:
	shar	r0
L_ashrsi3_19:
	shar	r0
L_ashrsi3_18:
	shar	r0
L_ashrsi3_17:
	shar	r0
L_ashrsi3_16:
	shlr16	r0
	rts
	exts.w	r0,r0

L_ashrsi3_15:
	shar	r0
L_ashrsi3_14:
	shar	r0
L_ashrsi3_13:
	shar	r0
L_ashrsi3_12:
	shar	r0
L_ashrsi3_11:
	shar	r0
L_ashrsi3_10:
	shar	r0
L_ashrsi3_9:
	shar	r0
L_ashrsi3_8:
	shar	r0
L_ashrsi3_7:
	shar	r0
L_ashrsi3_6:
	shar	r0
L_ashrsi3_5:
	shar	r0
L_ashrsi3_4:
	shar	r0
L_ashrsi3_3:
	shar	r0
L_ashrsi3_2:
	shar	r0
L_ashrsi3_1:
	rts
	shar	r0

L_ashrsi3_0:
	rts
	nop

#endif

#ifdef L_ashiftlt

!
! ___ashlsi3
!
! Entry:
!
! r4: Value to shift
! r5: Shifts
!
! Exit:
!
! r0: Result
!
! Destroys:
!
! (none)
!
	.global	___ashlsi3
	.align	2
___ashlsi3:
	mov	#31,r0
	cmp/hi	r0,r5
	bt	L_ashlsi3_32
	mova	L_ashlsi3_table,r0
	mov.b	@(r0,r5),r5
	add	r5,r0		! Change to braf when gas is fixed
	jmp	@r0
	mov	r4,r0

L_ashlsi3_table:
	.byte		L_ashlsi3_0-L_ashlsi3_table
	.byte		L_ashlsi3_1-L_ashlsi3_table
	.byte		L_ashlsi3_2-L_ashlsi3_table
	.byte		L_ashlsi3_3-L_ashlsi3_table
	.byte		L_ashlsi3_4-L_ashlsi3_table
	.byte		L_ashlsi3_5-L_ashlsi3_table
	.byte		L_ashlsi3_6-L_ashlsi3_table
	.byte		L_ashlsi3_7-L_ashlsi3_table
	.byte		L_ashlsi3_8-L_ashlsi3_table
	.byte		L_ashlsi3_9-L_ashlsi3_table
	.byte		L_ashlsi3_10-L_ashlsi3_table
	.byte		L_ashlsi3_11-L_ashlsi3_table
	.byte		L_ashlsi3_12-L_ashlsi3_table
	.byte		L_ashlsi3_13-L_ashlsi3_table
	.byte		L_ashlsi3_14-L_ashlsi3_table
	.byte		L_ashlsi3_15-L_ashlsi3_table
	.byte		L_ashlsi3_16-L_ashlsi3_table
	.byte		L_ashlsi3_17-L_ashlsi3_table
	.byte		L_ashlsi3_18-L_ashlsi3_table
	.byte		L_ashlsi3_19-L_ashlsi3_table
	.byte		L_ashlsi3_20-L_ashlsi3_table
	.byte		L_ashlsi3_21-L_ashlsi3_table
	.byte		L_ashlsi3_22-L_ashlsi3_table
	.byte		L_ashlsi3_23-L_ashlsi3_table
	.byte		L_ashlsi3_24-L_ashlsi3_table
	.byte		L_ashlsi3_25-L_ashlsi3_table
	.byte		L_ashlsi3_26-L_ashlsi3_table
	.byte		L_ashlsi3_27-L_ashlsi3_table
	.byte		L_ashlsi3_28-L_ashlsi3_table
	.byte		L_ashlsi3_29-L_ashlsi3_table
	.byte		L_ashlsi3_30-L_ashlsi3_table
	.byte		L_ashlsi3_31-L_ashlsi3_table

L_ashlsi3_6:
	shll2	r0
L_ashlsi3_4:
	shll2	r0
L_ashlsi3_2:
	rts
	shll2	r0

L_ashlsi3_7:
	shll2	r0
L_ashlsi3_5:
	shll2	r0
L_ashlsi3_3:
	shll2	r0
L_ashlsi3_1:
	rts
	shll	r0

L_ashlsi3_14:
	shll2	r0
L_ashlsi3_12:
	shll2	r0
L_ashlsi3_10:
	shll2	r0
L_ashlsi3_8:
	rts
	shll8	r0

L_ashlsi3_15:
	shll2	r0
L_ashlsi3_13:
	shll2	r0
L_ashlsi3_11:
	shll2	r0
L_ashlsi3_9:
	shll8	r0
	rts
	shll	r0

L_ashlsi3_22:
	shll2	r0
L_ashlsi3_20:
	shll2	r0
L_ashlsi3_18:
	shll2	r0
L_ashlsi3_16:
	rts
	shll16	r0

L_ashlsi3_23:
	shll2	r0
L_ashlsi3_21:
	shll2	r0
L_ashlsi3_19:
	shll2	r0
L_ashlsi3_17:
	shll16	r0
	rts
	shll	r0

L_ashlsi3_30:
	shll2	r0
L_ashlsi3_28:
	shll2	r0
L_ashlsi3_26:
	shll2	r0
L_ashlsi3_24:
	shll16	r0
	rts
	shll8	r0

L_ashlsi3_31:
	shll2	r0
L_ashlsi3_29:
	shll2	r0
L_ashlsi3_27:
	shll2	r0
L_ashlsi3_25:
	shll16	r0
	shll8	r0
	rts
	shll	r0

L_ashlsi3_32:
	rts
	mov	#0,r0

L_ashlsi3_0:
	rts
	nop

#endif

#ifdef L_lshiftrt

!
! ___lshrsi3
!
! Entry:
!
! r4: Value to shift
! r5: Shifts
!
! Exit:
!
! r0: Result
!
! Destroys:
!
! (none)
!
	.global	___lshrsi3
	.align	2
___lshrsi3:
	mov	#31,r0
	cmp/hi	r0,r5
	bt	L_lshrsi3_32
	mova	L_lshrsi3_table,r0
	mov.b	@(r0,r5),r5
	add	r5,r0		! Change to braf when gas is fixed
	jmp	@r0
	mov	r4,r0

L_lshrsi3_table:
	.byte		L_lshrsi3_0-L_lshrsi3_table
	.byte		L_lshrsi3_1-L_lshrsi3_table
	.byte		L_lshrsi3_2-L_lshrsi3_table
	.byte		L_lshrsi3_3-L_lshrsi3_table
	.byte		L_lshrsi3_4-L_lshrsi3_table
	.byte		L_lshrsi3_5-L_lshrsi3_table
	.byte		L_lshrsi3_6-L_lshrsi3_table
	.byte		L_lshrsi3_7-L_lshrsi3_table
	.byte		L_lshrsi3_8-L_lshrsi3_table
	.byte		L_lshrsi3_9-L_lshrsi3_table
	.byte		L_lshrsi3_10-L_lshrsi3_table
	.byte		L_lshrsi3_11-L_lshrsi3_table
	.byte		L_lshrsi3_12-L_lshrsi3_table
	.byte		L_lshrsi3_13-L_lshrsi3_table
	.byte		L_lshrsi3_14-L_lshrsi3_table
	.byte		L_lshrsi3_15-L_lshrsi3_table
	.byte		L_lshrsi3_16-L_lshrsi3_table
	.byte		L_lshrsi3_17-L_lshrsi3_table
	.byte		L_lshrsi3_18-L_lshrsi3_table
	.byte		L_lshrsi3_19-L_lshrsi3_table
	.byte		L_lshrsi3_20-L_lshrsi3_table
	.byte		L_lshrsi3_21-L_lshrsi3_table
	.byte		L_lshrsi3_22-L_lshrsi3_table
	.byte		L_lshrsi3_23-L_lshrsi3_table
	.byte		L_lshrsi3_24-L_lshrsi3_table
	.byte		L_lshrsi3_25-L_lshrsi3_table
	.byte		L_lshrsi3_26-L_lshrsi3_table
	.byte		L_lshrsi3_27-L_lshrsi3_table
	.byte		L_lshrsi3_28-L_lshrsi3_table
	.byte		L_lshrsi3_29-L_lshrsi3_table
	.byte		L_lshrsi3_30-L_lshrsi3_table
	.byte		L_lshrsi3_31-L_lshrsi3_table

L_lshrsi3_6:
	shlr2	r0
L_lshrsi3_4:
	shlr2	r0
L_lshrsi3_2:
	rts
	shlr2	r0

L_lshrsi3_7:
	shlr2	r0
L_lshrsi3_5:
	shlr2	r0
L_lshrsi3_3:
	shlr2	r0
L_lshrsi3_1:
	rts
	shlr	r0

L_lshrsi3_14:
	shlr2	r0
L_lshrsi3_12:
	shlr2	r0
L_lshrsi3_10:
	shlr2	r0
L_lshrsi3_8:
	rts
	shlr8	r0

L_lshrsi3_15:
	shlr2	r0
L_lshrsi3_13:
	shlr2	r0
L_lshrsi3_11:
	shlr2	r0
L_lshrsi3_9:
	shlr8	r0
	rts
	shlr	r0

L_lshrsi3_22:
	shlr2	r0
L_lshrsi3_20:
	shlr2	r0
L_lshrsi3_18:
	shlr2	r0
L_lshrsi3_16:
	rts
	shlr16	r0

L_lshrsi3_23:
	shlr2	r0
L_lshrsi3_21:
	shlr2	r0
L_lshrsi3_19:
	shlr2	r0
L_lshrsi3_17:
	shlr16	r0
	rts
	shlr	r0

L_lshrsi3_30:
	shlr2	r0
L_lshrsi3_28:
	shlr2	r0
L_lshrsi3_26:
	shlr2	r0
L_lshrsi3_24:
	shlr16	r0
	rts
	shlr8	r0

L_lshrsi3_31:
	shlr2	r0
L_lshrsi3_29:
	shlr2	r0
L_lshrsi3_27:
	shlr2	r0
L_lshrsi3_25:
	shlr16	r0
	shlr8	r0
	rts
	shlr	r0

L_lshrsi3_32:
	rts
	mov	#0,r0

L_lshrsi3_0:
	rts
	nop

#endif

#ifdef L_movstr
	.text
! done all the large groups, do the remainder

! jump to movstr+
done:
	add	#64,r5
	mova	___movstrSI0,r0
	shll2	r6
	add	r6,r0
	jmp	@r0
	add	#64,r4
	.align	4
	.global	___movstrSI64
___movstrSI64:
	mov.l	@(60,r5),r0
	mov.l	r0,@(60,r4)
	.global	___movstrSI60
___movstrSI60:
	mov.l	@(56,r5),r0
	mov.l	r0,@(56,r4)
	.global	___movstrSI56
___movstrSI56:
	mov.l	@(52,r5),r0
	mov.l	r0,@(52,r4)
	.global	___movstrSI52
___movstrSI52:
	mov.l	@(48,r5),r0
	mov.l	r0,@(48,r4)
	.global	___movstrSI48
___movstrSI48:
	mov.l	@(44,r5),r0
	mov.l	r0,@(44,r4)
	.global	___movstrSI44
___movstrSI44:
	mov.l	@(40,r5),r0
	mov.l	r0,@(40,r4)
	.global	___movstrSI40
___movstrSI40:
	mov.l	@(36,r5),r0
	mov.l	r0,@(36,r4)
	.global	___movstrSI36
___movstrSI36:
	mov.l	@(32,r5),r0
	mov.l	r0,@(32,r4)
	.global	___movstrSI32
___movstrSI32:
	mov.l	@(28,r5),r0
	mov.l	r0,@(28,r4)
	.global	___movstrSI28
___movstrSI28:
	mov.l	@(24,r5),r0
	mov.l	r0,@(24,r4)
	.global	___movstrSI24
___movstrSI24:
	mov.l	@(20,r5),r0
	mov.l	r0,@(20,r4)
	.global	___movstrSI20
___movstrSI20:
	mov.l	@(16,r5),r0
	mov.l	r0,@(16,r4)
	.global	___movstrSI16
___movstrSI16:
	mov.l	@(12,r5),r0
	mov.l	r0,@(12,r4)
	.global	___movstrSI12
___movstrSI12:
	mov.l	@(8,r5),r0
	mov.l	r0,@(8,r4)
	.global	___movstrSI8
___movstrSI8:
	mov.l	@(4,r5),r0
	mov.l	r0,@(4,r4)
	.global	___movstrSI4
___movstrSI4:
	mov.l	@(0,r5),r0
	mov.l	r0,@(0,r4)
___movstrSI0:
	rts
	or	r0,r0,r0

	.align	4

	.global	___movstr
___movstr:
	mov.l	@(60,r5),r0
	mov.l	r0,@(60,r4)

	mov.l	@(56,r5),r0
	mov.l	r0,@(56,r4)

	mov.l	@(52,r5),r0
	mov.l	r0,@(52,r4)

	mov.l	@(48,r5),r0
	mov.l	r0,@(48,r4)

	mov.l	@(44,r5),r0
	mov.l	r0,@(44,r4)

	mov.l	@(40,r5),r0
	mov.l	r0,@(40,r4)

	mov.l	@(36,r5),r0
	mov.l	r0,@(36,r4)

	mov.l	@(32,r5),r0
	mov.l	r0,@(32,r4)

	mov.l	@(28,r5),r0
	mov.l	r0,@(28,r4)

	mov.l	@(24,r5),r0
	mov.l	r0,@(24,r4)

	mov.l	@(20,r5),r0
	mov.l	r0,@(20,r4)

	mov.l	@(16,r5),r0
	mov.l	r0,@(16,r4)

	mov.l	@(12,r5),r0
	mov.l	r0,@(12,r4)

	mov.l	@(8,r5),r0
	mov.l	r0,@(8,r4)

	mov.l	@(4,r5),r0
	mov.l	r0,@(4,r4)

	mov.l	@(0,r5),r0
	mov.l	r0,@(0,r4)

	add	#-16,r6
	cmp/pl	r6
	bf	done

	add	#64,r5
	bra	___movstr
	add	#64,r4
#endif

#ifdef L_mulsi3


	.global	___mulsi3

! r4 =       aabb
! r5 =       ccdd
! r0 = aabb*ccdd  via partial products
!
! if aa == 0 and cc = 0
! r0 = bb*dd
!
! else
! aa = bb*dd + (aa*dd*65536) + (cc*bb*65536)
!

___mulsi3:
	mulu    r4,r5		! multiply the lsws  macl=bb*dd
	mov     r5,r3		! r3 = ccdd
	swap.w  r4,r2		! r2 = bbaa
	xtrct   r2,r3		! r3 = aacc
	tst  	r3,r3		! msws zero ?
	bf      hiset
	rts			! yes - then we have the answer
	sts     macl,r0

hiset:	sts	macl,r0		! r0 = bb*dd
	mulu	r2,r5		| brewing macl = aa*dd
	sts	macl,r1
	mulu	r3,r4		| brewing macl = cc*bb
	sts	macl,r2
	add	r1,r2
	shll16	r2
	rts
	add	r2,r0


#endif
#ifdef L_sdivsi3
	.title "SH DIVIDE"
!! 4 byte integer Divide code for the Hitachi SH
!!
!! Steve Chamberlain
!! sac@cygnus.com
!!
!!

!! args in r4 and r5, result in r0 clobber r1,r2,r3

	.global	___sdivsi3
___sdivsi3:
	mov	r4,r1
	mov	r5,r0

	tst	r0,r0
	bt	div0
	mov	#0,r2
	div0s	r2,r1
	subc	r3,r3
	subc	r2,r1
	div0s	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	div1	r0,r3
	rotcl	r1
	addc	r2,r1
	rts
	mov	r1,r0


div0:	rts
	mov	#0,r0

#endif
#ifdef L_udivsi3

	.title "SH DIVIDE"
!! 4 byte integer Divide code for the Hitachi SH
!!
!! Steve Chamberlain
!! sac@cygnus.com
!!
!!

!! args in r4 and r5, result in r0, clobbers r4, pr, and t bit
	.global	___udivsi3

___udivsi3:
longway:
	mov	#0,r0
	div0u
	! get one bit from the msb of the numerator into the T
	! bit and divide it by whats in r5.  Put the answer bit
	! into the T bit so it can come out again at the bottom

	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0

	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
shortway:
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0

vshortway:
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4 ; div1 r5,r0
	rotcl	r4
ret:	rts
	mov	r4,r0

#endif
