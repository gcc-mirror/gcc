/* Copyright (C) 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


!! libgcc1 routines for the Hitachi SH cpu.
!! Contributed by Steve Chamberlain.
!! sac@cygnus.com


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

___ashiftrt_r4_32:
	shar	r4
___ashiftrt_r4_31:
	shar	r4
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
	shar	r4
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
	shar	r4
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
	or	r0,r0
	rts
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
	rts			! yes - then weve got the answer
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

!! args in r4 and r5, result in r0, clobbers r4,r6, pr and t bit
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

