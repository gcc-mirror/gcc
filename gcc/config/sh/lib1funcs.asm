/* Copyright (C) 1994, 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

!! libgcc routines for the Renesas / SuperH SH CPUs.
!! Contributed by Steve Chamberlain.
!! sac@cygnus.com

!! ashiftrt_r4_x, ___ashrsi3, ___ashlsi3, ___lshrsi3 routines
!! recoded in assembly by Toshiyasu Morita
!! tm@netcom.com

/* SH2 optimizations for ___ashrsi3, ___ashlsi3, ___lshrsi3 and
   ELF local label prefixes by J"orn Rennecke
   amylaar@cygnus.com  */

#ifdef __ELF__
#define LOCAL(X)	.L_##X
#define FUNC(X)		.type X,@function
#define ENDFUNC0(X)	.Lfe_##X: .size X,.Lfe_##X-X
#define ENDFUNC(X)	ENDFUNC0(X)
#else
#define LOCAL(X)	L_##X
#define FUNC(X)
#define ENDFUNC(X)
#endif

#define	CONCAT(A,B)	A##B
#define	GLOBAL0(U,X)	CONCAT(U,__##X)
#define	GLOBAL(X)	GLOBAL0(__USER_LABEL_PREFIX__,X)

#if defined __SH5__ && ! defined __SH4_NOFPU__ && ! defined (__LITTLE_ENDIAN__)
#define FMOVD_WORKS
#endif

#if ! __SH5__
#ifdef L_ashiftrt
	.global	GLOBAL(ashiftrt_r4_0)
	.global	GLOBAL(ashiftrt_r4_1)
	.global	GLOBAL(ashiftrt_r4_2)
	.global	GLOBAL(ashiftrt_r4_3)
	.global	GLOBAL(ashiftrt_r4_4)
	.global	GLOBAL(ashiftrt_r4_5)
	.global	GLOBAL(ashiftrt_r4_6)
	.global	GLOBAL(ashiftrt_r4_7)
	.global	GLOBAL(ashiftrt_r4_8)
	.global	GLOBAL(ashiftrt_r4_9)
	.global	GLOBAL(ashiftrt_r4_10)
	.global	GLOBAL(ashiftrt_r4_11)
	.global	GLOBAL(ashiftrt_r4_12)
	.global	GLOBAL(ashiftrt_r4_13)
	.global	GLOBAL(ashiftrt_r4_14)
	.global	GLOBAL(ashiftrt_r4_15)
	.global	GLOBAL(ashiftrt_r4_16)
	.global	GLOBAL(ashiftrt_r4_17)
	.global	GLOBAL(ashiftrt_r4_18)
	.global	GLOBAL(ashiftrt_r4_19)
	.global	GLOBAL(ashiftrt_r4_20)
	.global	GLOBAL(ashiftrt_r4_21)
	.global	GLOBAL(ashiftrt_r4_22)
	.global	GLOBAL(ashiftrt_r4_23)
	.global	GLOBAL(ashiftrt_r4_24)
	.global	GLOBAL(ashiftrt_r4_25)
	.global	GLOBAL(ashiftrt_r4_26)
	.global	GLOBAL(ashiftrt_r4_27)
	.global	GLOBAL(ashiftrt_r4_28)
	.global	GLOBAL(ashiftrt_r4_29)
	.global	GLOBAL(ashiftrt_r4_30)
	.global	GLOBAL(ashiftrt_r4_31)
	.global	GLOBAL(ashiftrt_r4_32)

	FUNC(GLOBAL(ashiftrt_r4_0))
	FUNC(GLOBAL(ashiftrt_r4_1))
	FUNC(GLOBAL(ashiftrt_r4_2))
	FUNC(GLOBAL(ashiftrt_r4_3))
	FUNC(GLOBAL(ashiftrt_r4_4))
	FUNC(GLOBAL(ashiftrt_r4_5))
	FUNC(GLOBAL(ashiftrt_r4_6))
	FUNC(GLOBAL(ashiftrt_r4_7))
	FUNC(GLOBAL(ashiftrt_r4_8))
	FUNC(GLOBAL(ashiftrt_r4_9))
	FUNC(GLOBAL(ashiftrt_r4_10))
	FUNC(GLOBAL(ashiftrt_r4_11))
	FUNC(GLOBAL(ashiftrt_r4_12))
	FUNC(GLOBAL(ashiftrt_r4_13))
	FUNC(GLOBAL(ashiftrt_r4_14))
	FUNC(GLOBAL(ashiftrt_r4_15))
	FUNC(GLOBAL(ashiftrt_r4_16))
	FUNC(GLOBAL(ashiftrt_r4_17))
	FUNC(GLOBAL(ashiftrt_r4_18))
	FUNC(GLOBAL(ashiftrt_r4_19))
	FUNC(GLOBAL(ashiftrt_r4_20))
	FUNC(GLOBAL(ashiftrt_r4_21))
	FUNC(GLOBAL(ashiftrt_r4_22))
	FUNC(GLOBAL(ashiftrt_r4_23))
	FUNC(GLOBAL(ashiftrt_r4_24))
	FUNC(GLOBAL(ashiftrt_r4_25))
	FUNC(GLOBAL(ashiftrt_r4_26))
	FUNC(GLOBAL(ashiftrt_r4_27))
	FUNC(GLOBAL(ashiftrt_r4_28))
	FUNC(GLOBAL(ashiftrt_r4_29))
	FUNC(GLOBAL(ashiftrt_r4_30))
	FUNC(GLOBAL(ashiftrt_r4_31))
	FUNC(GLOBAL(ashiftrt_r4_32))

	.align	1
GLOBAL(ashiftrt_r4_32):
GLOBAL(ashiftrt_r4_31):
	rotcl	r4
	rts
	subc	r4,r4

GLOBAL(ashiftrt_r4_30):
	shar	r4
GLOBAL(ashiftrt_r4_29):
	shar	r4
GLOBAL(ashiftrt_r4_28):
	shar	r4
GLOBAL(ashiftrt_r4_27):
	shar	r4
GLOBAL(ashiftrt_r4_26):
	shar	r4
GLOBAL(ashiftrt_r4_25):
	shar	r4
GLOBAL(ashiftrt_r4_24):
	shlr16	r4
	shlr8	r4
	rts
	exts.b	r4,r4

GLOBAL(ashiftrt_r4_23):
	shar	r4
GLOBAL(ashiftrt_r4_22):
	shar	r4
GLOBAL(ashiftrt_r4_21):
	shar	r4
GLOBAL(ashiftrt_r4_20):
	shar	r4
GLOBAL(ashiftrt_r4_19):
	shar	r4
GLOBAL(ashiftrt_r4_18):
	shar	r4
GLOBAL(ashiftrt_r4_17):
	shar	r4
GLOBAL(ashiftrt_r4_16):
	shlr16	r4
	rts
	exts.w	r4,r4

GLOBAL(ashiftrt_r4_15):
	shar	r4
GLOBAL(ashiftrt_r4_14):
	shar	r4
GLOBAL(ashiftrt_r4_13):
	shar	r4
GLOBAL(ashiftrt_r4_12):
	shar	r4
GLOBAL(ashiftrt_r4_11):
	shar	r4
GLOBAL(ashiftrt_r4_10):
	shar	r4
GLOBAL(ashiftrt_r4_9):
	shar	r4
GLOBAL(ashiftrt_r4_8):
	shar	r4
GLOBAL(ashiftrt_r4_7):
	shar	r4
GLOBAL(ashiftrt_r4_6):
	shar	r4
GLOBAL(ashiftrt_r4_5):
	shar	r4
GLOBAL(ashiftrt_r4_4):
	shar	r4
GLOBAL(ashiftrt_r4_3):
	shar	r4
GLOBAL(ashiftrt_r4_2):
	shar	r4
GLOBAL(ashiftrt_r4_1):
	rts
	shar	r4

GLOBAL(ashiftrt_r4_0):
	rts
	nop

	ENDFUNC(GLOBAL(ashiftrt_r4_0))
	ENDFUNC(GLOBAL(ashiftrt_r4_1))
	ENDFUNC(GLOBAL(ashiftrt_r4_2))
	ENDFUNC(GLOBAL(ashiftrt_r4_3))
	ENDFUNC(GLOBAL(ashiftrt_r4_4))
	ENDFUNC(GLOBAL(ashiftrt_r4_5))
	ENDFUNC(GLOBAL(ashiftrt_r4_6))
	ENDFUNC(GLOBAL(ashiftrt_r4_7))
	ENDFUNC(GLOBAL(ashiftrt_r4_8))
	ENDFUNC(GLOBAL(ashiftrt_r4_9))
	ENDFUNC(GLOBAL(ashiftrt_r4_10))
	ENDFUNC(GLOBAL(ashiftrt_r4_11))
	ENDFUNC(GLOBAL(ashiftrt_r4_12))
	ENDFUNC(GLOBAL(ashiftrt_r4_13))
	ENDFUNC(GLOBAL(ashiftrt_r4_14))
	ENDFUNC(GLOBAL(ashiftrt_r4_15))
	ENDFUNC(GLOBAL(ashiftrt_r4_16))
	ENDFUNC(GLOBAL(ashiftrt_r4_17))
	ENDFUNC(GLOBAL(ashiftrt_r4_18))
	ENDFUNC(GLOBAL(ashiftrt_r4_19))
	ENDFUNC(GLOBAL(ashiftrt_r4_20))
	ENDFUNC(GLOBAL(ashiftrt_r4_21))
	ENDFUNC(GLOBAL(ashiftrt_r4_22))
	ENDFUNC(GLOBAL(ashiftrt_r4_23))
	ENDFUNC(GLOBAL(ashiftrt_r4_24))
	ENDFUNC(GLOBAL(ashiftrt_r4_25))
	ENDFUNC(GLOBAL(ashiftrt_r4_26))
	ENDFUNC(GLOBAL(ashiftrt_r4_27))
	ENDFUNC(GLOBAL(ashiftrt_r4_28))
	ENDFUNC(GLOBAL(ashiftrt_r4_29))
	ENDFUNC(GLOBAL(ashiftrt_r4_30))
	ENDFUNC(GLOBAL(ashiftrt_r4_31))
	ENDFUNC(GLOBAL(ashiftrt_r4_32))
#endif

#ifdef L_ashiftrt_n

!
! GLOBAL(ashrsi3)
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

	.global	GLOBAL(ashrsi3)
	FUNC(GLOBAL(ashrsi3))
	.align	2
GLOBAL(ashrsi3):
	mov	#31,r0
	and	r0,r5
	mova	LOCAL(ashrsi3_table),r0
	mov.b	@(r0,r5),r5
#ifdef __sh1__
	add	r5,r0
	jmp	@r0
#else
	braf	r5
#endif
	mov	r4,r0

	.align	2
LOCAL(ashrsi3_table):
	.byte		LOCAL(ashrsi3_0)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_1)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_2)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_3)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_4)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_5)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_6)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_7)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_8)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_9)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_10)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_11)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_12)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_13)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_14)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_15)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_16)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_17)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_18)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_19)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_20)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_21)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_22)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_23)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_24)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_25)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_26)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_27)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_28)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_29)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_30)-LOCAL(ashrsi3_table)
	.byte		LOCAL(ashrsi3_31)-LOCAL(ashrsi3_table)

LOCAL(ashrsi3_31):
	rotcl	r0
	rts
	subc	r0,r0

LOCAL(ashrsi3_30):
	shar	r0
LOCAL(ashrsi3_29):
	shar	r0
LOCAL(ashrsi3_28):
	shar	r0
LOCAL(ashrsi3_27):
	shar	r0
LOCAL(ashrsi3_26):
	shar	r0
LOCAL(ashrsi3_25):
	shar	r0
LOCAL(ashrsi3_24):
	shlr16	r0
	shlr8	r0
	rts
	exts.b	r0,r0

LOCAL(ashrsi3_23):
	shar	r0
LOCAL(ashrsi3_22):
	shar	r0
LOCAL(ashrsi3_21):
	shar	r0
LOCAL(ashrsi3_20):
	shar	r0
LOCAL(ashrsi3_19):
	shar	r0
LOCAL(ashrsi3_18):
	shar	r0
LOCAL(ashrsi3_17):
	shar	r0
LOCAL(ashrsi3_16):
	shlr16	r0
	rts
	exts.w	r0,r0

LOCAL(ashrsi3_15):
	shar	r0
LOCAL(ashrsi3_14):
	shar	r0
LOCAL(ashrsi3_13):
	shar	r0
LOCAL(ashrsi3_12):
	shar	r0
LOCAL(ashrsi3_11):
	shar	r0
LOCAL(ashrsi3_10):
	shar	r0
LOCAL(ashrsi3_9):
	shar	r0
LOCAL(ashrsi3_8):
	shar	r0
LOCAL(ashrsi3_7):
	shar	r0
LOCAL(ashrsi3_6):
	shar	r0
LOCAL(ashrsi3_5):
	shar	r0
LOCAL(ashrsi3_4):
	shar	r0
LOCAL(ashrsi3_3):
	shar	r0
LOCAL(ashrsi3_2):
	shar	r0
LOCAL(ashrsi3_1):
	rts
	shar	r0

LOCAL(ashrsi3_0):
	rts
	nop

	ENDFUNC(GLOBAL(ashrsi3))
#endif

#ifdef L_ashiftlt

!
! GLOBAL(ashlsi3)
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
	.global	GLOBAL(ashlsi3)
	FUNC(GLOBAL(ashlsi3))
	.align	2
GLOBAL(ashlsi3):
	mov	#31,r0
	and	r0,r5
	mova	LOCAL(ashlsi3_table),r0
	mov.b	@(r0,r5),r5
#ifdef __sh1__
	add	r5,r0
	jmp	@r0
#else
	braf	r5
#endif
	mov	r4,r0

	.align	2
LOCAL(ashlsi3_table):
	.byte		LOCAL(ashlsi3_0)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_1)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_2)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_3)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_4)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_5)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_6)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_7)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_8)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_9)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_10)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_11)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_12)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_13)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_14)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_15)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_16)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_17)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_18)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_19)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_20)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_21)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_22)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_23)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_24)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_25)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_26)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_27)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_28)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_29)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_30)-LOCAL(ashlsi3_table)
	.byte		LOCAL(ashlsi3_31)-LOCAL(ashlsi3_table)

LOCAL(ashlsi3_6):
	shll2	r0
LOCAL(ashlsi3_4):
	shll2	r0
LOCAL(ashlsi3_2):
	rts
	shll2	r0

LOCAL(ashlsi3_7):
	shll2	r0
LOCAL(ashlsi3_5):
	shll2	r0
LOCAL(ashlsi3_3):
	shll2	r0
LOCAL(ashlsi3_1):
	rts
	shll	r0

LOCAL(ashlsi3_14):
	shll2	r0
LOCAL(ashlsi3_12):
	shll2	r0
LOCAL(ashlsi3_10):
	shll2	r0
LOCAL(ashlsi3_8):
	rts
	shll8	r0

LOCAL(ashlsi3_15):
	shll2	r0
LOCAL(ashlsi3_13):
	shll2	r0
LOCAL(ashlsi3_11):
	shll2	r0
LOCAL(ashlsi3_9):
	shll8	r0
	rts
	shll	r0

LOCAL(ashlsi3_22):
	shll2	r0
LOCAL(ashlsi3_20):
	shll2	r0
LOCAL(ashlsi3_18):
	shll2	r0
LOCAL(ashlsi3_16):
	rts
	shll16	r0

LOCAL(ashlsi3_23):
	shll2	r0
LOCAL(ashlsi3_21):
	shll2	r0
LOCAL(ashlsi3_19):
	shll2	r0
LOCAL(ashlsi3_17):
	shll16	r0
	rts
	shll	r0

LOCAL(ashlsi3_30):
	shll2	r0
LOCAL(ashlsi3_28):
	shll2	r0
LOCAL(ashlsi3_26):
	shll2	r0
LOCAL(ashlsi3_24):
	shll16	r0
	rts
	shll8	r0

LOCAL(ashlsi3_31):
	shll2	r0
LOCAL(ashlsi3_29):
	shll2	r0
LOCAL(ashlsi3_27):
	shll2	r0
LOCAL(ashlsi3_25):
	shll16	r0
	shll8	r0
	rts
	shll	r0

LOCAL(ashlsi3_0):
	rts
	nop

	ENDFUNC(GLOBAL(ashlsi3))
#endif

#ifdef L_lshiftrt

!
! GLOBAL(lshrsi3)
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
	.global	GLOBAL(lshrsi3)
	FUNC(GLOBAL(lshrsi3))
	.align	2
GLOBAL(lshrsi3):
	mov	#31,r0
	and	r0,r5
	mova	LOCAL(lshrsi3_table),r0
	mov.b	@(r0,r5),r5
#ifdef __sh1__
	add	r5,r0
	jmp	@r0
#else
	braf	r5
#endif
	mov	r4,r0

	.align	2
LOCAL(lshrsi3_table):
	.byte		LOCAL(lshrsi3_0)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_1)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_2)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_3)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_4)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_5)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_6)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_7)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_8)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_9)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_10)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_11)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_12)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_13)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_14)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_15)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_16)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_17)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_18)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_19)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_20)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_21)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_22)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_23)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_24)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_25)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_26)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_27)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_28)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_29)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_30)-LOCAL(lshrsi3_table)
	.byte		LOCAL(lshrsi3_31)-LOCAL(lshrsi3_table)

LOCAL(lshrsi3_6):
	shlr2	r0
LOCAL(lshrsi3_4):
	shlr2	r0
LOCAL(lshrsi3_2):
	rts
	shlr2	r0

LOCAL(lshrsi3_7):
	shlr2	r0
LOCAL(lshrsi3_5):
	shlr2	r0
LOCAL(lshrsi3_3):
	shlr2	r0
LOCAL(lshrsi3_1):
	rts
	shlr	r0

LOCAL(lshrsi3_14):
	shlr2	r0
LOCAL(lshrsi3_12):
	shlr2	r0
LOCAL(lshrsi3_10):
	shlr2	r0
LOCAL(lshrsi3_8):
	rts
	shlr8	r0

LOCAL(lshrsi3_15):
	shlr2	r0
LOCAL(lshrsi3_13):
	shlr2	r0
LOCAL(lshrsi3_11):
	shlr2	r0
LOCAL(lshrsi3_9):
	shlr8	r0
	rts
	shlr	r0

LOCAL(lshrsi3_22):
	shlr2	r0
LOCAL(lshrsi3_20):
	shlr2	r0
LOCAL(lshrsi3_18):
	shlr2	r0
LOCAL(lshrsi3_16):
	rts
	shlr16	r0

LOCAL(lshrsi3_23):
	shlr2	r0
LOCAL(lshrsi3_21):
	shlr2	r0
LOCAL(lshrsi3_19):
	shlr2	r0
LOCAL(lshrsi3_17):
	shlr16	r0
	rts
	shlr	r0

LOCAL(lshrsi3_30):
	shlr2	r0
LOCAL(lshrsi3_28):
	shlr2	r0
LOCAL(lshrsi3_26):
	shlr2	r0
LOCAL(lshrsi3_24):
	shlr16	r0
	rts
	shlr8	r0

LOCAL(lshrsi3_31):
	shlr2	r0
LOCAL(lshrsi3_29):
	shlr2	r0
LOCAL(lshrsi3_27):
	shlr2	r0
LOCAL(lshrsi3_25):
	shlr16	r0
	shlr8	r0
	rts
	shlr	r0

LOCAL(lshrsi3_0):
	rts
	nop

	ENDFUNC(GLOBAL(lshrsi3))
#endif

#ifdef L_movstr
	.text
! done all the large groups, do the remainder

! jump to movstr+
done:
	add	#64,r5
	mova	GLOBAL(movstrSI0),r0
	shll2	r6
	add	r6,r0
	jmp	@r0
	add	#64,r4
	.align	4
	.global	GLOBAL(movstrSI64)
	FUNC(GLOBAL(movstrSI64))
GLOBAL(movstrSI64):
	mov.l	@(60,r5),r0
	mov.l	r0,@(60,r4)
	.global	GLOBAL(movstrSI60)
	FUNC(GLOBAL(movstrSI60))
GLOBAL(movstrSI60):
	mov.l	@(56,r5),r0
	mov.l	r0,@(56,r4)
	.global	GLOBAL(movstrSI56)
	FUNC(GLOBAL(movstrSI56))
GLOBAL(movstrSI56):
	mov.l	@(52,r5),r0
	mov.l	r0,@(52,r4)
	.global	GLOBAL(movstrSI52)
	FUNC(GLOBAL(movstrSI52))
GLOBAL(movstrSI52):
	mov.l	@(48,r5),r0
	mov.l	r0,@(48,r4)
	.global	GLOBAL(movstrSI48)
	FUNC(GLOBAL(movstrSI48))
GLOBAL(movstrSI48):
	mov.l	@(44,r5),r0
	mov.l	r0,@(44,r4)
	.global	GLOBAL(movstrSI44)
	FUNC(GLOBAL(movstrSI44))
GLOBAL(movstrSI44):
	mov.l	@(40,r5),r0
	mov.l	r0,@(40,r4)
	.global	GLOBAL(movstrSI40)
	FUNC(GLOBAL(movstrSI40))
GLOBAL(movstrSI40):
	mov.l	@(36,r5),r0
	mov.l	r0,@(36,r4)
	.global	GLOBAL(movstrSI36)
	FUNC(GLOBAL(movstrSI36))
GLOBAL(movstrSI36):
	mov.l	@(32,r5),r0
	mov.l	r0,@(32,r4)
	.global	GLOBAL(movstrSI32)
	FUNC(GLOBAL(movstrSI32))
GLOBAL(movstrSI32):
	mov.l	@(28,r5),r0
	mov.l	r0,@(28,r4)
	.global	GLOBAL(movstrSI28)
	FUNC(GLOBAL(movstrSI28))
GLOBAL(movstrSI28):
	mov.l	@(24,r5),r0
	mov.l	r0,@(24,r4)
	.global	GLOBAL(movstrSI24)
	FUNC(GLOBAL(movstrSI24))
GLOBAL(movstrSI24):
	mov.l	@(20,r5),r0
	mov.l	r0,@(20,r4)
	.global	GLOBAL(movstrSI20)
	FUNC(GLOBAL(movstrSI20))
GLOBAL(movstrSI20):
	mov.l	@(16,r5),r0
	mov.l	r0,@(16,r4)
	.global	GLOBAL(movstrSI16)
	FUNC(GLOBAL(movstrSI16))
GLOBAL(movstrSI16):
	mov.l	@(12,r5),r0
	mov.l	r0,@(12,r4)
	.global	GLOBAL(movstrSI12)
	FUNC(GLOBAL(movstrSI12))
GLOBAL(movstrSI12):
	mov.l	@(8,r5),r0
	mov.l	r0,@(8,r4)
	.global	GLOBAL(movstrSI8)
	FUNC(GLOBAL(movstrSI8))
GLOBAL(movstrSI8):
	mov.l	@(4,r5),r0
	mov.l	r0,@(4,r4)
	.global	GLOBAL(movstrSI4)
	FUNC(GLOBAL(movstrSI4))
GLOBAL(movstrSI4):
	mov.l	@(0,r5),r0
	mov.l	r0,@(0,r4)
	.global	GLOBAL(movstrSI0)
	FUNC(GLOBAL(movstrSI0))
GLOBAL(movstrSI0):
	rts
	nop

	ENDFUNC(GLOBAL(movstrSI64))
	ENDFUNC(GLOBAL(movstrSI60))
	ENDFUNC(GLOBAL(movstrSI56))
	ENDFUNC(GLOBAL(movstrSI52))
	ENDFUNC(GLOBAL(movstrSI48))
	ENDFUNC(GLOBAL(movstrSI44))
	ENDFUNC(GLOBAL(movstrSI40))
	ENDFUNC(GLOBAL(movstrSI36))
	ENDFUNC(GLOBAL(movstrSI32))
	ENDFUNC(GLOBAL(movstrSI28))
	ENDFUNC(GLOBAL(movstrSI24))
	ENDFUNC(GLOBAL(movstrSI20))
	ENDFUNC(GLOBAL(movstrSI16))
	ENDFUNC(GLOBAL(movstrSI12))
	ENDFUNC(GLOBAL(movstrSI8))
	ENDFUNC(GLOBAL(movstrSI4))
	ENDFUNC(GLOBAL(movstrSI0))

	.align	4

	.global	GLOBAL(movstr)
	FUNC(GLOBAL(movstr))
GLOBAL(movstr):
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
	bra	GLOBAL(movstr)
	add	#64,r4

	FUNC(GLOBAL(movstr))
#endif

#ifdef L_movstr_i4
	.text
	.global	GLOBAL(movstr_i4_even)
	.global	GLOBAL(movstr_i4_odd)
	.global	GLOBAL(movstrSI12_i4)

	FUNC(GLOBAL(movstr_i4_even))
	FUNC(GLOBAL(movstr_i4_odd))
	FUNC(GLOBAL(movstrSI12_i4))

	.p2align	5
L_movstr_2mod4_end:
	mov.l	r0,@(16,r4)
	rts
	mov.l	r1,@(20,r4)

	.p2align	2

GLOBAL(movstr_i4_even):
	mov.l	@r5+,r0
	bra	L_movstr_start_even
	mov.l	@r5+,r1

GLOBAL(movstr_i4_odd):
	mov.l	@r5+,r1
	add	#-4,r4
	mov.l	@r5+,r2
	mov.l	@r5+,r3
	mov.l	r1,@(4,r4)
	mov.l	r2,@(8,r4)

L_movstr_loop:
	mov.l	r3,@(12,r4)
	dt	r6
	mov.l	@r5+,r0
	bt/s	L_movstr_2mod4_end
	mov.l	@r5+,r1
	add	#16,r4
L_movstr_start_even:
	mov.l	@r5+,r2
	mov.l	@r5+,r3
	mov.l	r0,@r4
	dt	r6
	mov.l	r1,@(4,r4)
	bf/s	L_movstr_loop
	mov.l	r2,@(8,r4)
	rts
	mov.l	r3,@(12,r4)

	ENDFUNC(GLOBAL(movstr_i4_even))
	ENDFUNC(GLOBAL(movstr_i4_odd))

	.p2align	4
GLOBAL(movstrSI12_i4):
	mov.l	@r5,r0
	mov.l	@(4,r5),r1
	mov.l	@(8,r5),r2
	mov.l	r0,@r4
	mov.l	r1,@(4,r4)
	rts
	mov.l	r2,@(8,r4)

	ENDFUNC(GLOBAL(movstrSI12_i4))
#endif

#ifdef L_mulsi3


	.global	GLOBAL(mulsi3)
	FUNC(GLOBAL(mulsi3))

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

GLOBAL(mulsi3):
	mulu.w  r4,r5		! multiply the lsws  macl=bb*dd
	mov     r5,r3		! r3 = ccdd
	swap.w  r4,r2		! r2 = bbaa
	xtrct   r2,r3		! r3 = aacc
	tst  	r3,r3		! msws zero ?
	bf      hiset
	rts			! yes - then we have the answer
	sts     macl,r0

hiset:	sts	macl,r0		! r0 = bb*dd
	mulu.w	r2,r5		! brewing macl = aa*dd
	sts	macl,r1
	mulu.w	r3,r4		! brewing macl = cc*bb
	sts	macl,r2
	add	r1,r2
	shll16	r2
	rts
	add	r2,r0

	FUNC(GLOBAL(mulsi3))
#endif
#endif /* ! __SH5__ */
#ifdef L_sdivsi3_i4
	.title "SH DIVIDE"
!! 4 byte integer Divide code for the Renesas SH
#ifdef __SH4__
!! args in r4 and r5, result in fpul, clobber dr0, dr2

	.global	GLOBAL(sdivsi3_i4)
	FUNC(GLOBAL(sdivsi3_i4))
GLOBAL(sdivsi3_i4):
	lds r4,fpul
	float fpul,dr0
	lds r5,fpul
	float fpul,dr2
	fdiv dr2,dr0
	rts
	ftrc dr0,fpul

	ENDFUNC(GLOBAL(sdivsi3_i4))
#elif defined(__SH4_SINGLE__) || defined(__SH4_SINGLE_ONLY__) || (defined (__SH5__) && ! defined __SH4_NOFPU__)
!! args in r4 and r5, result in fpul, clobber r2, dr0, dr2

#if ! __SH5__ || __SH5__ == 32
#if __SH5__
	.mode	SHcompact
#endif
	.global	GLOBAL(sdivsi3_i4)
	FUNC(GLOBAL(sdivsi3_i4))
GLOBAL(sdivsi3_i4):
	sts.l fpscr,@-r15
	mov #8,r2
	swap.w r2,r2
	lds r2,fpscr
	lds r4,fpul
	float fpul,dr0
	lds r5,fpul
	float fpul,dr2
	fdiv dr2,dr0
	ftrc dr0,fpul
	rts
	lds.l @r15+,fpscr

	ENDFUNC(GLOBAL(sdivsi3_i4))
#endif /* ! __SH5__ || __SH5__ == 32 */
#endif /* ! __SH4__ */
#endif

#ifdef L_sdivsi3
/* __SH4_SINGLE_ONLY__ keeps this part for link compatibility with
   sh2e/sh3e code.  */
#if (! defined(__SH4__) && ! defined (__SH4_SINGLE__)) || defined (__linux__)
!!
!! Steve Chamberlain
!! sac@cygnus.com
!!
!!

!! args in r4 and r5, result in r0 clobber r1, r2, r3, and t bit

	.global	GLOBAL(sdivsi3)
	FUNC(GLOBAL(sdivsi3))
#if __SHMEDIA__
#if __SH5__ == 32
	.section	.text..SHmedia32,"ax"
#else
	.text
#endif
	.align	2
#if 0
/* The assembly code that follows is a hand-optimized version of the C
   code that follows.  Note that the registers that are modified are
   exactly those listed as clobbered in the patterns divsi3_i1 and
   divsi3_i1_media.
	
int __sdivsi3 (i, j)
     int i, j;
{
  register unsigned long long r18 asm ("r18");
  register unsigned long long r19 asm ("r19");
  register unsigned long long r0 asm ("r0") = 0;
  register unsigned long long r1 asm ("r1") = 1;
  register int r2 asm ("r2") = i >> 31;
  register int r3 asm ("r3") = j >> 31;

  r2 = r2 ? r2 : r1;
  r3 = r3 ? r3 : r1;
  r18 = i * r2;
  r19 = j * r3;
  r2 *= r3;
  
  r19 <<= 31;
  r1 <<= 31;
  do
    if (r18 >= r19)
      r0 |= r1, r18 -= r19;
  while (r19 >>= 1, r1 >>= 1);

  return r2 * (int)r0;
}
*/
GLOBAL(sdivsi3):
	pt/l	LOCAL(sdivsi3_dontadd), tr2
	pt/l	LOCAL(sdivsi3_loop), tr1
	ptabs/l	r18, tr0
	movi	0, r0
	movi	1, r1
	shari.l	r4, 31, r2
	shari.l	r5, 31, r3
	cmveq	r2, r1, r2
	cmveq	r3, r1, r3
	muls.l	r4, r2, r18
	muls.l	r5, r3, r19
	muls.l	r2, r3, r2
	shlli	r19, 31, r19
	shlli	r1, 31, r1
LOCAL(sdivsi3_loop):
	bgtu	r19, r18, tr2
	or	r0, r1, r0
	sub	r18, r19, r18
LOCAL(sdivsi3_dontadd):
	shlri	r1, 1, r1
	shlri	r19, 1, r19
	bnei	r1, 0, tr1
	muls.l	r0, r2, r0
	add.l	r0, r63, r0
	blink	tr0, r63
#else /* ! 0 */
 // inputs: r4,r5
 // clobbered: r1,r2,r3,r18,r19,r20,r21,r25,tr0
 // result in r0
GLOBAL(sdivsi3):
 // can create absolute value without extra latency,
 // but dependent on proper sign extension of inputs:
 // shari.l r5,31,r2
 // xor r5,r2,r20
 // sub r20,r2,r20 // r20 is now absolute value of r5, zero-extended.
 shari.l r5,31,r2
 ori r2,1,r2
 muls.l r5,r2,r20 // r20 is now absolute value of r5, zero-extended.
 movi 0xffffffffffffbb0c,r19 // shift count eqiv 76
 shari.l r4,31,r3
 nsb r20,r0
 shlld r20,r0,r25
 shlri r25,48,r25
 sub r19,r25,r1
 mmulfx.w r1,r1,r2
 mshflo.w r1,r63,r1
 // If r4 was to be used in-place instead of r21, could use this sequence
 // to compute absolute:
 // sub r63,r4,r19 // compute absolute value of r4
 // shlri r4,32,r3 // into lower 32 bit of r4, keeping
 // mcmv r19,r3,r4 // the sign in the upper 32 bits intact.
 ori r3,1,r3
 mmulfx.w r25,r2,r2
 sub r19,r0,r0
 muls.l r4,r3,r21
 msub.w r1,r2,r2
 addi r2,-2,r1
 mulu.l r21,r1,r19
 mmulfx.w r2,r2,r2
 shlli r1,15,r1
 shlrd r19,r0,r19
 mulu.l r19,r20,r3
 mmacnfx.wl r25,r2,r1
 ptabs r18,tr0
 sub r21,r3,r25

 mulu.l r25,r1,r2
 addi r0,14,r0
 xor r4,r5,r18
 shlrd r2,r0,r2
 mulu.l r2,r20,r3
 add r19,r2,r19
 shari.l r18,31,r18
 sub r25,r3,r25

 mulu.l r25,r1,r2
 sub r25,r20,r25
 add r19,r18,r19
 shlrd r2,r0,r2
 mulu.l r2,r20,r3
 addi r25,1,r25
 add r19,r2,r19

 cmpgt r25,r3,r25
 add.l r19,r25,r0
 xor r0,r18,r0
 blink tr0,r63
#endif
#elif defined __SHMEDIA__
/* m5compact-nofpu */
 // clobbered: r18,r19,r20,r21,r25,tr0,tr1,tr2
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
GLOBAL(sdivsi3):
	pt/l LOCAL(sdivsi3_dontsub), tr0
	pt/l LOCAL(sdivsi3_loop), tr1
	ptabs/l r18,tr2
	shari.l r4,31,r18
	shari.l r5,31,r19
	xor r4,r18,r20
	xor r5,r19,r21
	sub.l r20,r18,r20
	sub.l r21,r19,r21
	xor r18,r19,r19
	shlli r21,32,r25
	addi r25,-1,r21
	addz.l r20,r63,r20
LOCAL(sdivsi3_loop):
	shlli r20,1,r20
	bgeu/u r21,r20,tr0
	sub r20,r21,r20
LOCAL(sdivsi3_dontsub):
	addi.l r25,-1,r25
	bnei r25,-32,tr1
	xor r20,r19,r20
	sub.l r20,r19,r0
	blink tr2,r63
#else /* ! __SHMEDIA__ */
GLOBAL(sdivsi3):
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

	ENDFUNC(GLOBAL(sdivsi3))
#endif /* ! __SHMEDIA__ */
#endif /* ! __SH4__ */
#endif
#ifdef L_udivsi3_i4

	.title "SH DIVIDE"
!! 4 byte integer Divide code for the Renesas SH
#ifdef __SH4__
!! args in r4 and r5, result in fpul, clobber r0, r1, r4, r5, dr0, dr2, dr4,
!! and t bit

	.global	GLOBAL(udivsi3_i4)
	FUNC(GLOBAL(udivsi3_i4))
GLOBAL(udivsi3_i4):
	mov #1,r1
	cmp/hi r1,r5
	bf trivial
	rotr r1
	xor r1,r4
	lds r4,fpul
	mova L1,r0
#ifdef FMOVD_WORKS
	fmov.d @r0+,dr4
#else
#ifdef __LITTLE_ENDIAN__
	fmov.s @r0+,fr5
	fmov.s @r0,fr4
#else
	fmov.s @r0+,fr4
	fmov.s @r0,fr5
#endif
#endif
	float fpul,dr0
	xor r1,r5
	lds r5,fpul
	float fpul,dr2
	fadd dr4,dr0
	fadd dr4,dr2
	fdiv dr2,dr0
	rts
	ftrc dr0,fpul

trivial:
	rts
	lds r4,fpul

	.align 2
#ifdef FMOVD_WORKS
	.align 3	! make double below 8 byte aligned.
#endif
L1:
	.double 2147483648

	ENDFUNC(GLOBAL(udivsi3_i4))
#elif defined (__SH5__) && ! defined (__SH4_NOFPU__)
#if ! __SH5__ || __SH5__ == 32
!! args in r4 and r5, result in fpul, clobber r20, r21, dr0, fr33
	.mode	SHmedia
	.global	GLOBAL(udivsi3_i4)
	FUNC(GLOBAL(udivsi3_i4))
GLOBAL(udivsi3_i4):
	addz.l	r4,r63,r20
	addz.l	r5,r63,r21
	fmov.qd	r20,dr0
	fmov.qd	r21,dr32
	ptabs	r18,tr0
	float.qd dr0,dr0
	float.qd dr32,dr32
	fdiv.d	dr0,dr32,dr0
	ftrc.dq dr0,dr32
	fmov.s fr33,fr32
	blink tr0,r63

	ENDFUNC(GLOBAL(udivsi3_i4))
#endif /* ! __SH5__ || __SH5__ == 32 */
#elif defined(__SH4_SINGLE__) || defined(__SH4_SINGLE_ONLY__)
!! args in r4 and r5, result in fpul, clobber r0, r1, r4, r5, dr0, dr2, dr4

	.global	GLOBAL(udivsi3_i4)
GLOBAL(udivsi3_i4):
	mov #1,r1
	cmp/hi r1,r5
	bf trivial
	sts.l fpscr,@-r15
	mova L1,r0
	lds.l @r0+,fpscr
	rotr r1
	xor r1,r4
	lds r4,fpul
#ifdef FMOVD_WORKS
	fmov.d @r0+,dr4
#else
#ifdef __LITTLE_ENDIAN__
	fmov.s @r0+,fr5
	fmov.s @r0,fr4
#else
	fmov.s @r0+,fr4
	fmov.s @r0,fr5
#endif
#endif
	float fpul,dr0
	xor r1,r5
	lds r5,fpul
	float fpul,dr2
	fadd dr4,dr0
	fadd dr4,dr2
	fdiv dr2,dr0
	ftrc dr0,fpul
	rts
	lds.l @r15+,fpscr

#ifdef FMOVD_WORKS
	.align 3	! make double below 8 byte aligned.
#endif
trivial:
	rts
	lds r4,fpul

	.align 2
L1:
#ifndef FMOVD_WORKS
	.long 0x80000
#else
	.long 0x180000
#endif
	.double 2147483648

	ENDFUNC(GLOBAL(udivsi3_i4))
#endif /* ! __SH4__ */
#endif

#ifdef L_udivsi3
/* __SH4_SINGLE_ONLY__ keeps this part for link compatibility with
   sh2e/sh3e code.  */
#if (! defined(__SH4__) && ! defined (__SH4_SINGLE__)) || defined (__linux__)

!! args in r4 and r5, result in r0, clobbers r4, pr, and t bit
	.global	GLOBAL(udivsi3)
	FUNC(GLOBAL(udivsi3))

#if __SHMEDIA__
#if __SH5__ == 32
	.section	.text..SHmedia32,"ax"
#else
	.text
#endif
	.align	2
#if 0
/* The assembly code that follows is a hand-optimized version of the C
   code that follows.  Note that the registers that are modified are
   exactly those listed as clobbered in the patterns udivsi3_i1 and
   udivsi3_i1_media.
	
unsigned 
__udivsi3 (i, j)
    unsigned i, j; 
{
  register unsigned long long r0 asm ("r0") = 0;
  register unsigned long long r18 asm ("r18") = 1;
  register unsigned long long r4 asm ("r4") = i;
  register unsigned long long r19 asm ("r19") = j;

  r19 <<= 31;
  r18 <<= 31;
  do
    if (r4 >= r19)
      r0 |= r18, r4 -= r19;
  while (r19 >>= 1, r18 >>= 1);

  return r0;
}
*/
GLOBAL(udivsi3):
	pt/l	LOCAL(udivsi3_dontadd), tr2
	pt/l	LOCAL(udivsi3_loop), tr1
	ptabs/l	r18, tr0
	movi	0, r0
	movi	1, r18
	addz.l	r5, r63, r19
	addz.l	r4, r63, r4
	shlli	r19, 31, r19
	shlli	r18, 31, r18
LOCAL(udivsi3_loop):
	bgtu	r19, r4, tr2
	or	r0, r18, r0
	sub	r4, r19, r4
LOCAL(udivsi3_dontadd):
	shlri	r18, 1, r18
	shlri	r19, 1, r19
	bnei	r18, 0, tr1
	blink	tr0, r63
#else
GLOBAL(udivsi3):
 // inputs: r4,r5
 // clobbered: r18,r19,r20,r21,r22,r25,tr0
 // result in r0.
 addz.l r5,r63,r22
 nsb r22,r0
 shlld r22,r0,r25
 shlri r25,48,r25
 movi 0xffffffffffffbb0c,r20 // shift count eqiv 76
 sub r20,r25,r21
 mmulfx.w r21,r21,r19
 mshflo.w r21,r63,r21
 ptabs r18,tr0
 mmulfx.w r25,r19,r19
 sub r20,r0,r0
 /* bubble */
 msub.w r21,r19,r19
 addi r19,-2,r21 /* It would be nice for scheduling to do this add to r21
		    before the msub.w, but we need a different value for
		    r19 to keep errors under control.  */
 mulu.l r4,r21,r18
 mmulfx.w r19,r19,r19
 shlli r21,15,r21
 shlrd r18,r0,r18
 mulu.l r18,r22,r20
 mmacnfx.wl r25,r19,r21
 /* bubble */
 sub r4,r20,r25

 mulu.l r25,r21,r19
 addi r0,14,r0
 /* bubble */
 shlrd r19,r0,r19
 mulu.l r19,r22,r20
 add r18,r19,r18
 /* bubble */
 sub.l r25,r20,r25

 mulu.l r25,r21,r19
 addz.l r25,r63,r25
 sub r25,r22,r25
 shlrd r19,r0,r19
 mulu.l r19,r22,r20
 addi r25,1,r25
 add r18,r19,r18

 cmpgt r25,r20,r25
 add.l r18,r25,r0
 blink tr0,r63
#endif
#elif defined (__SHMEDIA__)
/* m5compact-nofpu - more emphasis on code size than on speed, but don't
   ignore speed altogether - div1 needs 9 cycles, subc 7 and rotcl 4.
   So use a short shmedia loop.  */
 // clobbered: r20,r21,r25,tr0,tr1,tr2
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
GLOBAL(udivsi3):
 pt/l LOCAL(udivsi3_dontsub), tr0
 pt/l LOCAL(udivsi3_loop), tr1
 ptabs/l r18,tr2
 shlli r5,32,r25
 addi r25,-1,r21
 addz.l r4,r63,r20
LOCAL(udivsi3_loop):
 shlli r20,1,r20
 bgeu/u r21,r20,tr0
 sub r20,r21,r20
LOCAL(udivsi3_dontsub):
 addi.l r25,-1,r25
 bnei r25,-32,tr1
 add.l r20,r63,r0
 blink tr2,r63
#else /* ! defined (__SHMEDIA__) */
LOCAL(div8):
 div1 r5,r4
LOCAL(div7):
 div1 r5,r4; div1 r5,r4; div1 r5,r4
 div1 r5,r4; div1 r5,r4; div1 r5,r4; rts; div1 r5,r4

LOCAL(divx4):
 div1 r5,r4; rotcl r0
 div1 r5,r4; rotcl r0
 div1 r5,r4; rotcl r0
 rts; div1 r5,r4

GLOBAL(udivsi3):
 sts.l pr,@-r15
 extu.w r5,r0
 cmp/eq r5,r0
#ifdef __sh1__
 bf LOCAL(large_divisor)
#else
 bf/s LOCAL(large_divisor)
#endif
 div0u
 swap.w r4,r0
 shlr16 r4
 bsr LOCAL(div8)
 shll16 r5
 bsr LOCAL(div7)
 div1 r5,r4
 xtrct r4,r0
 xtrct r0,r4
 bsr LOCAL(div8)
 swap.w r4,r4
 bsr LOCAL(div7)
 div1 r5,r4
 lds.l @r15+,pr
 xtrct r4,r0
 swap.w r0,r0
 rotcl r0
 rts
 shlr16 r5

LOCAL(large_divisor):
#ifdef __sh1__
 div0u
#endif
 mov #0,r0
 xtrct r4,r0
 xtrct r0,r4
 bsr LOCAL(divx4)
 rotcl r0
 bsr LOCAL(divx4)
 rotcl r0
 bsr LOCAL(divx4)
 rotcl r0
 bsr LOCAL(divx4)
 rotcl r0
 lds.l @r15+,pr
 rts
 rotcl r0

	ENDFUNC(GLOBAL(udivsi3))
#endif /* ! __SHMEDIA__ */
#endif /* __SH4__ */
#endif /* L_udivsi3 */

#ifdef L_udivdi3
#ifdef __SHMEDIA__
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
	.global	GLOBAL(udivdi3)
	FUNC(GLOBAL(udivdi3))
GLOBAL(udivdi3):
	shlri r3,1,r4
	nsb r4,r22
	shlld r3,r22,r6
	shlri r6,49,r5
	movi 0xffffffffffffbaf1,r21 /* .l shift count 17.  */
	sub r21,r5,r1
	mmulfx.w r1,r1,r4
	mshflo.w r1,r63,r1
	sub r63,r22,r20 // r63 == 64 % 64
	mmulfx.w r5,r4,r4
	pta LOCAL(large_divisor),tr0
	addi r20,32,r9
	msub.w r1,r4,r1
	madd.w r1,r1,r1
	mmulfx.w r1,r1,r4
	shlri r6,32,r7
	bgt/u r9,r63,tr0 // large_divisor
	mmulfx.w r5,r4,r4
	shlri r2,32+14,r19
	addi r22,-31,r0
	msub.w r1,r4,r1

	mulu.l r1,r7,r4
	addi r1,-3,r5
	mulu.l r5,r19,r5
	sub r63,r4,r4 // Negate to make sure r1 ends up <= 1/r2
	shlri r4,2,r4 /* chop off leading %0000000000000000 001.00000000000 - or, as
	                 the case may be, %0000000000000000 000.11111111111, still */
	muls.l r1,r4,r4 /* leaving at least one sign bit.  */
	mulu.l r5,r3,r8
	mshalds.l r1,r21,r1
	shari r4,26,r4
	shlld r8,r0,r8
	add r1,r4,r1 // 31 bit unsigned reciprocal now in r1 (msb equiv. 0.5)
	sub r2,r8,r2
	/* Can do second step of 64 : 32 div now, using r1 and the rest in r2.  */

	shlri r2,22,r21
	mulu.l r21,r1,r21
	shlld r5,r0,r8
	addi r20,30-22,r0
	shlrd r21,r0,r21
	mulu.l r21,r3,r5
	add r8,r21,r8
	mcmpgt.l r21,r63,r21 // See Note 1
	addi r20,30,r0
	mshfhi.l r63,r21,r21
	sub r2,r5,r2
	andc r2,r21,r2

	/* small divisor: need a third divide step */
	mulu.l r2,r1,r7
	ptabs r18,tr0
	addi r2,1,r2
	shlrd r7,r0,r7
	mulu.l r7,r3,r5
	add r8,r7,r8
	sub r2,r3,r2
	cmpgt r2,r5,r5
	add r8,r5,r2
	/* could test r3 here to check for divide by zero.  */
	blink tr0,r63

LOCAL(large_divisor):
	mmulfx.w r5,r4,r4
	shlrd r2,r9,r25
	shlri r25,32,r8
	msub.w r1,r4,r1

	mulu.l r1,r7,r4
	addi r1,-3,r5
	mulu.l r5,r8,r5
	sub r63,r4,r4 // Negate to make sure r1 ends up <= 1/r2
	shlri r4,2,r4 /* chop off leading %0000000000000000 001.00000000000 - or, as
	                 the case may be, %0000000000000000 000.11111111111, still */
	muls.l r1,r4,r4 /* leaving at least one sign bit.  */
	shlri r5,14-1,r8
	mulu.l r8,r7,r5
	mshalds.l r1,r21,r1
	shari r4,26,r4
	add r1,r4,r1 // 31 bit unsigned reciprocal now in r1 (msb equiv. 0.5)
	sub r25,r5,r25
	/* Can do second step of 64 : 32 div now, using r1 and the rest in r25.  */

	shlri r25,22,r21
	mulu.l r21,r1,r21
	pta LOCAL(no_lo_adj),tr0
	addi r22,32,r0
	shlri r21,40,r21
	mulu.l r21,r7,r5
	add r8,r21,r8
	shlld r2,r0,r2
	sub r25,r5,r25
	bgtu/u r7,r25,tr0 // no_lo_adj
	addi r8,1,r8
	sub r25,r7,r25
LOCAL(no_lo_adj):
	mextr4 r2,r25,r2

	/* large_divisor: only needs a few adjustments.  */
	mulu.l r8,r6,r5
	ptabs r18,tr0
	/* bubble */
	cmpgtu r5,r2,r5
	sub r8,r5,r2
	blink tr0,r63
	ENDFUNC(GLOBAL(udivdi3))
/* Note 1: To shift the result of the second divide stage so that the result
   always fits into 32 bits, yet we still reduce the rest sufficiently
   would require a lot of instructions to do the shifts just right.  Using
   the full 64 bit shift result to multiply with the divisor would require
   four extra instructions for the upper 32 bits (shift / mulu / shift / sub).
   Fortunately, if the upper 32 bits of the shift result are nonzero, we
   know that the rest after taking this partial result into account will
   fit into 32 bits.  So we just clear the upper 32 bits of the rest if the
   upper 32 bits of the partial result are nonzero.  */
#endif /* __SHMEDIA__ */
#endif /* L_udivdi3 */

#ifdef L_divdi3
#ifdef __SHMEDIA__
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
	.global	GLOBAL(divdi3)
	FUNC(GLOBAL(divdi3))
GLOBAL(divdi3):
	pta GLOBAL(udivdi3),tr0
	shari r2,63,r22
	shari r3,63,r23
	xor r2,r22,r2
	xor r3,r23,r3
	sub r2,r22,r2
	sub r3,r23,r3
	beq/u r22,r23,tr0
	ptabs r18,tr1
	blink tr0,r18
	sub r63,r2,r2
	blink tr1,r63
	ENDFUNC(GLOBAL(divdi3))
#endif /* __SHMEDIA__ */
#endif /* L_divdi3 */

#ifdef L_umoddi3
#ifdef __SHMEDIA__
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
	.global	GLOBAL(umoddi3)
	FUNC(GLOBAL(umoddi3))
GLOBAL(umoddi3):
	shlri r3,1,r4
	nsb r4,r22
	shlld r3,r22,r6
	shlri r6,49,r5
	movi 0xffffffffffffbaf1,r21 /* .l shift count 17.  */
	sub r21,r5,r1
	mmulfx.w r1,r1,r4
	mshflo.w r1,r63,r1
	sub r63,r22,r20 // r63 == 64 % 64
	mmulfx.w r5,r4,r4
	pta LOCAL(large_divisor),tr0
	addi r20,32,r9
	msub.w r1,r4,r1
	madd.w r1,r1,r1
	mmulfx.w r1,r1,r4
	shlri r6,32,r7
	bgt/u r9,r63,tr0 // large_divisor
	mmulfx.w r5,r4,r4
	shlri r2,32+14,r19
	addi r22,-31,r0
	msub.w r1,r4,r1

	mulu.l r1,r7,r4
	addi r1,-3,r5
	mulu.l r5,r19,r5
	sub r63,r4,r4 // Negate to make sure r1 ends up <= 1/r2
	shlri r4,2,r4 /* chop off leading %0000000000000000 001.00000000000 - or, as
	                 the case may be, %0000000000000000 000.11111111111, still */
	muls.l r1,r4,r4 /* leaving at least one sign bit.  */
	mulu.l r5,r3,r5
	mshalds.l r1,r21,r1
	shari r4,26,r4
	shlld r5,r0,r5
	add r1,r4,r1 // 31 bit unsigned reciprocal now in r1 (msb equiv. 0.5)
	sub r2,r5,r2
	/* Can do second step of 64 : 32 div now, using r1 and the rest in r2.  */

	shlri r2,22,r21
	mulu.l r21,r1,r21
	addi r20,30-22,r0
	/* bubble */ /* could test r3 here to check for divide by zero.  */
	shlrd r21,r0,r21
	mulu.l r21,r3,r5
	mcmpgt.l r21,r63,r21 // See Note 1
	addi r20,30,r0
	mshfhi.l r63,r21,r21
	sub r2,r5,r2
	andc r2,r21,r2

	/* small divisor: need a third divide step */
	mulu.l r2,r1,r7
	ptabs r18,tr0
	sub r2,r3,r8 /* re-use r8 here for rest - r3 */
	shlrd r7,r0,r7
	mulu.l r7,r3,r5
	/* bubble */
	addi r8,1,r7
	cmpgt r7,r5,r7
	cmvne r7,r8,r2
	sub r2,r5,r2
	blink tr0,r63

LOCAL(large_divisor):
	mmulfx.w r5,r4,r4
	shlrd r2,r9,r25
	shlri r25,32,r8
	msub.w r1,r4,r1

	mulu.l r1,r7,r4
	addi r1,-3,r5
	mulu.l r5,r8,r5
	sub r63,r4,r4 // Negate to make sure r1 ends up <= 1/r2
	shlri r4,2,r4 /* chop off leading %0000000000000000 001.00000000000 - or, as
	                 the case may be, %0000000000000000 000.11111111111, still */
	muls.l r1,r4,r4 /* leaving at least one sign bit.  */
	shlri r5,14-1,r8
	mulu.l r8,r7,r5
	mshalds.l r1,r21,r1
	shari r4,26,r4
	add r1,r4,r1 // 31 bit unsigned reciprocal now in r1 (msb equiv. 0.5)
	sub r25,r5,r25
	/* Can do second step of 64 : 32 div now, using r1 and the rest in r25.  */

	shlri r25,22,r21
	mulu.l r21,r1,r21
	pta LOCAL(no_lo_adj),tr0
	addi r22,32,r0
	shlri r21,40,r21
	mulu.l r21,r7,r5
	add r8,r21,r8
	shlld r2,r0,r2
	sub r25,r5,r25
	bgtu/u r7,r25,tr0 // no_lo_adj
	addi r8,1,r8
	sub r25,r7,r25
LOCAL(no_lo_adj):
	mextr4 r2,r25,r2

	/* large_divisor: only needs a few adjustments.  */
	mulu.l r8,r6,r5
	ptabs r18,tr0
	add r2,r6,r7
	cmpgtu r5,r2,r8
	cmvne r8,r7,r2
	sub r2,r5,r2
	shlrd r2,r22,r2
	blink tr0,r63
	ENDFUNC(GLOBAL(umoddi3))
/* Note 1: To shift the result of the second divide stage so that the result
   always fits into 32 bits, yet we still reduce the rest sufficiently
   would require a lot of instructions to do the shifts just right.  Using
   the full 64 bit shift result to multiply with the divisor would require
   four extra instructions for the upper 32 bits (shift / mulu / shift / sub).
   Fortunately, if the upper 32 bits of the shift result are nonzero, we
   know that the rest after taking this partial result into account will
   fit into 32 bits.  So we just clear the upper 32 bits of the rest if the
   upper 32 bits of the partial result are nonzero.  */
#endif /* __SHMEDIA__ */
#endif /* L_umoddi3 */

#ifdef L_moddi3
#ifdef __SHMEDIA__
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
	.global	GLOBAL(moddi3)
	FUNC(GLOBAL(moddi3))
GLOBAL(moddi3):
	pta GLOBAL(umoddi3),tr0
	shari r2,63,r22
	shari r3,63,r23
	xor r2,r22,r2
	xor r3,r23,r3
	sub r2,r22,r2
	sub r3,r23,r3
	beq/u r22,r63,tr0
	ptabs r18,tr1
	blink tr0,r18
	sub r63,r2,r2
	blink tr1,r63
	ENDFUNC(GLOBAL(moddi3))
#endif /* __SHMEDIA__ */
#endif /* L_moddi3 */

#ifdef L_set_fpscr
#if defined (__SH2E__) || defined (__SH3E__) || defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__) || __SH5__ == 32
#ifdef __SH5__
	.mode	SHcompact
#endif
	.global GLOBAL(set_fpscr)
	FUNC(GLOBAL(set_fpscr))
GLOBAL(set_fpscr):
	lds r4,fpscr
#ifdef __PIC__
	mov.l	r12,@-r15
	mova	LOCAL(set_fpscr_L0),r0
	mov.l	LOCAL(set_fpscr_L0),r12
	add	r0,r12
	mov.l	LOCAL(set_fpscr_L1),r0
	mov.l	@(r0,r12),r1
	mov.l	@r15+,r12
#else
	mov.l LOCAL(set_fpscr_L1),r1
#endif
	swap.w r4,r0
	or #24,r0
#ifndef FMOVD_WORKS
	xor #16,r0
#endif
#if defined(__SH4__)
	swap.w r0,r3
	mov.l r3,@(4,r1)
#else /* defined (__SH2E__) || defined(__SH3E__) || defined(__SH4_SINGLE*__) */
	swap.w r0,r2
	mov.l r2,@r1
#endif
#ifndef FMOVD_WORKS
	xor #8,r0
#else
	xor #24,r0
#endif
#if defined(__SH4__)
	swap.w r0,r2
	rts
	mov.l r2,@r1
#else /* defined(__SH2E__) || defined(__SH3E__) || defined(__SH4_SINGLE*__) */
	swap.w r0,r3
	rts
	mov.l r3,@(4,r1)
#endif
	.align 2
#ifdef __PIC__
LOCAL(set_fpscr_L0):
	.long _GLOBAL_OFFSET_TABLE_
LOCAL(set_fpscr_L1):
	.long GLOBAL(fpscr_values@GOT)
#else
LOCAL(set_fpscr_L1):
	.long GLOBAL(fpscr_values)
#endif

	ENDFUNC(GLOBAL(set_fpscr))
#ifndef NO_FPSCR_VALUES
#ifdef __ELF__
        .comm   GLOBAL(fpscr_values),8,4
#else
        .comm   GLOBAL(fpscr_values),8
#endif /* ELF */
#endif /* NO_FPSCR_VALUES */
#endif /* SH2E / SH3E / SH4 */
#endif /* L_set_fpscr */
#ifdef L_ic_invalidate
#if __SH5__ == 32
	.mode	SHmedia
	.section	.text..SHmedia32,"ax"
	.align	2
	.global	GLOBAL(init_trampoline)
	FUNC(GLOBAL(init_trampoline))
GLOBAL(init_trampoline):
	st.l	r0,8,r2
#ifdef __LITTLE_ENDIAN__
	movi	9,r20
	shori	0x402b,r20
	shori	0xd101,r20
	shori	0xd002,r20
#else
	movi	0xffffffffffffd002,r20
	shori	0xd101,r20
	shori	0x402b,r20
	shori	9,r20
#endif
	st.q	r0,0,r20
	st.l	r0,12,r3
	.global	GLOBAL(ic_invalidate)
	FUNC(GLOBAL(ic_invalidate))
GLOBAL(ic_invalidate):
	ocbwb	r0,0
	synco
	icbi	r0, 0
	ptabs	r18, tr0
	synci
	blink	tr0, r63

	ENDFUNC(GLOBAL(ic_invalidate))
	ENDFUNC(GLOBAL(init_trampoline))
#elif defined(__SH4_SINGLE__) || defined(__SH4__) || defined(__SH4_SINGLE_ONLY__)
	.global GLOBAL(ic_invalidate)
	FUNC(GLOBAL(ic_invalidate))
GLOBAL(ic_invalidate):
	ocbwb	@r4
	mova	0f,r0
	mov.w	1f,r1
/* Compute how many cache lines 0f is away from r4.  */
	sub	r0,r4
	and	r1,r4
/* Prepare to branch to 0f plus the cache-line offset.  */
	add	# 0f - 1f,r4
	braf	r4
	nop
1:
	.short	0x1fe0
	.p2align 5
/* This must be aligned to the beginning of a cache line.  */
0:
	.rept	256 /* There are 256 cache lines of 32 bytes.  */
	rts
	.rept	15
	nop
	.endr
	.endr

	ENDFUNC(GLOBAL(ic_invalidate))
#endif /* SH4 */
#endif /* L_ic_invalidate */

#if defined (__SH5__) && __SH5__ == 32
#ifdef L_shcompact_call_trampoline
	.section	.rodata
	.align	1
LOCAL(ct_main_table):
.word	LOCAL(ct_r2_fp) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r2_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r2_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r3_fp) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r3_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r3_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r4_fp) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r4_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r4_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r5_fp) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r5_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r5_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r6_fph) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r6_fpl) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r6_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r6_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r7_fph) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r7_fpl) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r7_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r7_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r8_fph) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r8_fpl) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r8_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r8_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r9_fph) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r9_fpl) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r9_ld) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r9_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_pop_seq) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_pop_seq) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_r9_pop) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_ret_wide) - datalabel LOCAL(ct_main_label)
.word	LOCAL(ct_call_func) - datalabel LOCAL(ct_main_label)
	.mode	SHmedia
	.section	.text..SHmedia32, "ax"
	.align	2
	
     /* This function loads 64-bit general-purpose registers from the
	stack, from a memory address contained in them or from an FP
	register, according to a cookie passed in r1.  Its execution
	time is linear on the number of registers that actually have
	to be copied.  See sh.h for details on the actual bit pattern.

	The function to be called is passed in r0.  If a 32-bit return
	value is expected, the actual function will be tail-called,
	otherwise the return address will be stored in r10 (that the
	caller should expect to be clobbered) and the return value
	will be expanded into r2/r3 upon return.  */
	
	.global	GLOBAL(GCC_shcompact_call_trampoline)
	FUNC(GLOBAL(GCC_shcompact_call_trampoline))
GLOBAL(GCC_shcompact_call_trampoline):
	ptabs/l	r0, tr0	/* Prepare to call the actual function.  */
	movi	((datalabel LOCAL(ct_main_table) - 31 * 2) >> 16) & 65535, r0
	pt/l	LOCAL(ct_loop), tr1
	addz.l	r1, r63, r1
	shori	((datalabel LOCAL(ct_main_table) - 31 * 2)) & 65535, r0
LOCAL(ct_loop):
	nsb	r1, r28
	shlli	r28, 1, r29
	ldx.w	r0, r29, r30
LOCAL(ct_main_label):
	ptrel/l	r30, tr2
	blink	tr2, r63
LOCAL(ct_r2_fp):	/* Copy r2 from an FP register.  */
	/* It must be dr0, so just do it.  */
	fmov.dq	dr0, r2
	movi	7, r30
	shlli	r30, 29, r31
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r3_fp):	/* Copy r3 from an FP register.  */
	/* It is either dr0 or dr2.  */
	movi	7, r30
	shlri	r1, 26, r32
	shlli	r30, 26, r31
	andc	r1, r31, r1
	fmov.dq	dr0, r3
	beqi/l	r32, 4, tr1
	fmov.dq	dr2, r3
	blink	tr1, r63
LOCAL(ct_r4_fp):	/* Copy r4 from an FP register.  */
	shlri	r1, 23 - 3, r34
	andi	r34, 3 << 3, r33
	addi	r33, LOCAL(ct_r4_fp_copy) - datalabel LOCAL(ct_r4_fp_base), r32
LOCAL(ct_r4_fp_base):
	ptrel/l	r32, tr2
	movi	7, r30
	shlli	r30, 23, r31
	andc	r1, r31, r1
	blink	tr2, r63
LOCAL(ct_r4_fp_copy):
	fmov.dq	dr0, r4
	blink	tr1, r63
	fmov.dq	dr2, r4
	blink	tr1, r63
	fmov.dq	dr4, r4
	blink	tr1, r63
LOCAL(ct_r5_fp):	/* Copy r5 from an FP register.  */
	shlri	r1, 20 - 3, r34
	andi	r34, 3 << 3, r33
	addi	r33, LOCAL(ct_r5_fp_copy) - datalabel LOCAL(ct_r5_fp_base), r32
LOCAL(ct_r5_fp_base):
	ptrel/l	r32, tr2
	movi	7, r30
	shlli	r30, 20, r31
	andc	r1, r31, r1
	blink	tr2, r63
LOCAL(ct_r5_fp_copy):
	fmov.dq	dr0, r5
	blink	tr1, r63
	fmov.dq	dr2, r5
	blink	tr1, r63
	fmov.dq	dr4, r5
	blink	tr1, r63
	fmov.dq	dr6, r5
	blink	tr1, r63
LOCAL(ct_r6_fph):	/* Copy r6 from a high FP register.  */
	/* It must be dr8.  */
	fmov.dq	dr8, r6
	movi	15, r30
	shlli	r30, 16, r31
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r6_fpl):	/* Copy r6 from a low FP register.  */
	shlri	r1, 16 - 3, r34
	andi	r34, 3 << 3, r33
	addi	r33, LOCAL(ct_r6_fp_copy) - datalabel LOCAL(ct_r6_fp_base), r32
LOCAL(ct_r6_fp_base):
	ptrel/l	r32, tr2
	movi	7, r30
	shlli	r30, 16, r31
	andc	r1, r31, r1
	blink	tr2, r63
LOCAL(ct_r6_fp_copy):
	fmov.dq	dr0, r6
	blink	tr1, r63
	fmov.dq	dr2, r6
	blink	tr1, r63
	fmov.dq	dr4, r6
	blink	tr1, r63
	fmov.dq	dr6, r6
	blink	tr1, r63
LOCAL(ct_r7_fph):	/* Copy r7 from a high FP register.  */
	/* It is either dr8 or dr10.  */
	movi	15 << 12, r31
	shlri	r1, 12, r32
	andc	r1, r31, r1
	fmov.dq	dr8, r7
	beqi/l	r32, 8, tr1
	fmov.dq	dr10, r7
	blink	tr1, r63
LOCAL(ct_r7_fpl):	/* Copy r7 from a low FP register.  */
	shlri	r1, 12 - 3, r34
	andi	r34, 3 << 3, r33
	addi	r33, LOCAL(ct_r7_fp_copy) - datalabel LOCAL(ct_r7_fp_base), r32
LOCAL(ct_r7_fp_base):
	ptrel/l	r32, tr2
	movi	7 << 12, r31
	andc	r1, r31, r1
	blink	tr2, r63
LOCAL(ct_r7_fp_copy):
	fmov.dq	dr0, r7
	blink	tr1, r63
	fmov.dq	dr2, r7
	blink	tr1, r63
	fmov.dq	dr4, r7
	blink	tr1, r63
	fmov.dq	dr6, r7
	blink	tr1, r63
LOCAL(ct_r8_fph):	/* Copy r8 from a high FP register.  */
	/* It is either dr8 or dr10.  */
	movi	15 << 8, r31
	andi	r1, 1 << 8, r32
	andc	r1, r31, r1
	fmov.dq	dr8, r8
	beq/l	r32, r63, tr1
	fmov.dq	dr10, r8
	blink	tr1, r63
LOCAL(ct_r8_fpl):	/* Copy r8 from a low FP register.  */
	shlri	r1, 8 - 3, r34
	andi	r34, 3 << 3, r33
	addi	r33, LOCAL(ct_r8_fp_copy) - datalabel LOCAL(ct_r8_fp_base), r32
LOCAL(ct_r8_fp_base):
	ptrel/l	r32, tr2
	movi	7 << 8, r31
	andc	r1, r31, r1
	blink	tr2, r63
LOCAL(ct_r8_fp_copy):
	fmov.dq	dr0, r8
	blink	tr1, r63
	fmov.dq	dr2, r8
	blink	tr1, r63
	fmov.dq	dr4, r8
	blink	tr1, r63
	fmov.dq	dr6, r8
	blink	tr1, r63
LOCAL(ct_r9_fph):	/* Copy r9 from a high FP register.  */
	/* It is either dr8 or dr10.  */
	movi	15 << 4, r31
	andi	r1, 1 << 4, r32
	andc	r1, r31, r1
	fmov.dq	dr8, r9
	beq/l	r32, r63, tr1
	fmov.dq	dr10, r9
	blink	tr1, r63
LOCAL(ct_r9_fpl):	/* Copy r9 from a low FP register.  */
	shlri	r1, 4 - 3, r34
	andi	r34, 3 << 3, r33
	addi	r33, LOCAL(ct_r9_fp_copy) - datalabel LOCAL(ct_r9_fp_base), r32
LOCAL(ct_r9_fp_base):
	ptrel/l	r32, tr2
	movi	7 << 4, r31
	andc	r1, r31, r1
	blink	tr2, r63
LOCAL(ct_r9_fp_copy):
	fmov.dq	dr0, r9
	blink	tr1, r63
	fmov.dq	dr2, r9
	blink	tr1, r63
	fmov.dq	dr4, r9
	blink	tr1, r63
	fmov.dq	dr6, r9
	blink	tr1, r63
LOCAL(ct_r2_ld):	/* Copy r2 from a memory address.  */
	pt/l	LOCAL(ct_r2_load), tr2
	movi	3, r30
	shlli	r30, 29, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r2, 8, r3
	ldx.q	r2, r63, r2
	/* Fall through.  */
LOCAL(ct_r3_ld):	/* Copy r3 from a memory address.  */
	pt/l	LOCAL(ct_r3_load), tr2
	movi	3, r30
	shlli	r30, 26, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r3, 8, r4
	ldx.q	r3, r63, r3
LOCAL(ct_r4_ld):	/* Copy r4 from a memory address.  */
	pt/l	LOCAL(ct_r4_load), tr2
	movi	3, r30
	shlli	r30, 23, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r4, 8, r5
	ldx.q	r4, r63, r4
LOCAL(ct_r5_ld):	/* Copy r5 from a memory address.  */
	pt/l	LOCAL(ct_r5_load), tr2
	movi	3, r30
	shlli	r30, 20, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r5, 8, r6
	ldx.q	r5, r63, r5
LOCAL(ct_r6_ld):	/* Copy r6 from a memory address.  */
	pt/l	LOCAL(ct_r6_load), tr2
	movi	3 << 16, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r6, 8, r7
	ldx.q	r6, r63, r6
LOCAL(ct_r7_ld):	/* Copy r7 from a memory address.  */
	pt/l	LOCAL(ct_r7_load), tr2
	movi	3 << 12, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r7, 8, r8
	ldx.q	r7, r63, r7
LOCAL(ct_r8_ld):	/* Copy r8 from a memory address.  */
	pt/l	LOCAL(ct_r8_load), tr2
	movi	3 << 8, r31
	and	r1, r31, r32
	andc	r1, r31, r1
	beq/l	r31, r32, tr2
	addi.l	r8, 8, r9
	ldx.q	r8, r63, r8
LOCAL(ct_r9_ld):	/* Copy r9 from a memory address.  */
	pt/l	LOCAL(ct_check_tramp), tr2
	ldx.q	r9, r63, r9
	blink	tr2, r63
LOCAL(ct_r2_load):
	ldx.q	r2, r63, r2
	blink	tr1, r63
LOCAL(ct_r3_load):
	ldx.q	r3, r63, r3
	blink	tr1, r63
LOCAL(ct_r4_load):
	ldx.q	r4, r63, r4
	blink	tr1, r63
LOCAL(ct_r5_load):
	ldx.q	r5, r63, r5
	blink	tr1, r63
LOCAL(ct_r6_load):
	ldx.q	r6, r63, r6
	blink	tr1, r63
LOCAL(ct_r7_load):
	ldx.q	r7, r63, r7
	blink	tr1, r63
LOCAL(ct_r8_load):
	ldx.q	r8, r63, r8
	blink	tr1, r63
LOCAL(ct_r2_pop):	/* Pop r2 from the stack.  */
	movi	1, r30
	ldx.q	r15, r63, r2
	shlli	r30, 29, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r3_pop):	/* Pop r3 from the stack.  */
	movi	1, r30
	ldx.q	r15, r63, r3
	shlli	r30, 26, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r4_pop):	/* Pop r4 from the stack.  */
	movi	1, r30
	ldx.q	r15, r63, r4
	shlli	r30, 23, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r5_pop):	/* Pop r5 from the stack.  */
	movi	1, r30
	ldx.q	r15, r63, r5
	shlli	r30, 20, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r6_pop):	/* Pop r6 from the stack.  */
	movi	1, r30
	ldx.q	r15, r63, r6
	shlli	r30, 16, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r7_pop):	/* Pop r7 from the stack.  */
	ldx.q	r15, r63, r7
	movi	1 << 12, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_r8_pop):	/* Pop r8 from the stack.  */
	ldx.q	r15, r63, r8
	movi	1 << 8, r31
	addi.l	r15, 8, r15
	andc	r1, r31, r1
	blink	tr1, r63
LOCAL(ct_pop_seq):	/* Pop a sequence of registers off the stack.  */
	andi	r1, 7 << 1, r30
	movi	(LOCAL(ct_end_of_pop_seq) >> 16) & 65535, r32
	shlli	r30, 2, r31
	shori	LOCAL(ct_end_of_pop_seq) & 65535, r32
	sub.l	r32, r31, r33
	ptabs/l	r33, tr2
	blink	tr2, r63
LOCAL(ct_start_of_pop_seq):	/* Beginning of pop sequence.  */
	ldx.q	r15, r63, r3
	addi.l	r15, 8, r15
	ldx.q	r15, r63, r4
	addi.l	r15, 8, r15
	ldx.q	r15, r63, r5
	addi.l	r15, 8, r15
	ldx.q	r15, r63, r6
	addi.l	r15, 8, r15
	ldx.q	r15, r63, r7
	addi.l	r15, 8, r15
	ldx.q	r15, r63, r8
	addi.l	r15, 8, r15
LOCAL(ct_r9_pop):	/* Pop r9 from the stack.  */
	ldx.q	r15, r63, r9
	addi.l	r15, 8, r15
LOCAL(ct_end_of_pop_seq): /* Label used to compute first pop instruction.  */
LOCAL(ct_check_tramp):	/* Check whether we need a trampoline.  */
	pt/u	LOCAL(ct_ret_wide), tr2
	andi	r1, 1, r1
	bne/u	r1, r63, tr2
LOCAL(ct_call_func):	/* Just branch to the function.  */
	blink	tr0, r63
LOCAL(ct_ret_wide):	/* Call the function, so that we can unpack its 
			   64-bit return value.  */
	add.l	r18, r63, r10
	blink	tr0, r18
	ptabs	r10, tr0
#if __LITTLE_ENDIAN__
	shari	r2, 32, r3
	add.l	r2, r63, r2
#else
	add.l	r2, r63, r3
	shari	r2, 32, r2
#endif
	blink	tr0, r63

	ENDFUNC(GLOBAL(GCC_shcompact_call_trampoline))
#endif /* L_shcompact_call_trampoline */

#ifdef L_shcompact_return_trampoline
     /* This function does the converse of the code in `ret_wide'
	above.  It is tail-called by SHcompact functions returning
	64-bit non-floating-point values, to pack the 32-bit values in
	r2 and r3 into r2.  */

	.mode	SHmedia
	.section	.text..SHmedia32, "ax"
	.align	2
	.global	GLOBAL(GCC_shcompact_return_trampoline)
	FUNC(GLOBAL(GCC_shcompact_return_trampoline))
GLOBAL(GCC_shcompact_return_trampoline):
	ptabs/l	r18, tr0
#if __LITTLE_ENDIAN__
	addz.l	r2, r63, r2
	shlli	r3, 32, r3
#else
	addz.l	r3, r63, r3
	shlli	r2, 32, r2
#endif
	or	r3, r2, r2
	blink	tr0, r63

	ENDFUNC(GLOBAL(GCC_shcompact_return_trampoline))
#endif /* L_shcompact_return_trampoline */

#ifdef L_shcompact_incoming_args
	.section	.rodata
	.align	1
LOCAL(ia_main_table):
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r2_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r2_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r3_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r3_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r4_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r4_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r5_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r5_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r6_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r6_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r7_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r7_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r8_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r8_push) - datalabel LOCAL(ia_main_label)
.word	1 /* Invalid, just loop */
.word	1 /* Invalid, just loop */
.word	LOCAL(ia_r9_ld) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r9_push) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_push_seq) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_push_seq) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_r9_push) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_return) - datalabel LOCAL(ia_main_label)
.word	LOCAL(ia_return) - datalabel LOCAL(ia_main_label)
	.mode	SHmedia
	.section	.text..SHmedia32, "ax"
	.align	2
	
     /* This function stores 64-bit general-purpose registers back in
	the stack, and loads the address in which each register
	was stored into itself.  The lower 32 bits of r17 hold the address
	to begin storing, and the upper 32 bits of r17 hold the cookie.
	Its execution time is linear on the
	number of registers that actually have to be copied, and it is
	optimized for structures larger than 64 bits, as opposed to
	individual `long long' arguments.  See sh.h for details on the
	actual bit pattern.  */
	
	.global	GLOBAL(GCC_shcompact_incoming_args)
	FUNC(GLOBAL(GCC_shcompact_incoming_args))
GLOBAL(GCC_shcompact_incoming_args):
	ptabs/l	r18, tr0	/* Prepare to return.  */
	shlri	r17, 32, r0	/* Load the cookie.  */
	movi	((datalabel LOCAL(ia_main_table) - 31 * 2) >> 16) & 65535, r43
	pt/l	LOCAL(ia_loop), tr1
	add.l	r17, r63, r17
	shori	((datalabel LOCAL(ia_main_table) - 31 * 2)) & 65535, r43
LOCAL(ia_loop):
	nsb	r0, r36
	shlli	r36, 1, r37
	ldx.w	r43, r37, r38
LOCAL(ia_main_label):
	ptrel/l	r38, tr2
	blink	tr2, r63
LOCAL(ia_r2_ld):	/* Store r2 and load its address.  */
	movi	3, r38
	shlli	r38, 29, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r2
	add.l	r17, r63, r2
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r3_ld):	/* Store r3 and load its address.  */
	movi	3, r38
	shlli	r38, 26, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r3
	add.l	r17, r63, r3
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r4_ld):	/* Store r4 and load its address.  */
	movi	3, r38
	shlli	r38, 23, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r4
	add.l	r17, r63, r4
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r5_ld):	/* Store r5 and load its address.  */
	movi	3, r38
	shlli	r38, 20, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r5
	add.l	r17, r63, r5
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r6_ld):	/* Store r6 and load its address.  */
	movi	3, r38
	shlli	r38, 16, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r6
	add.l	r17, r63, r6
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r7_ld):	/* Store r7 and load its address.  */
	movi	3 << 12, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r7
	add.l	r17, r63, r7
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r8_ld):	/* Store r8 and load its address.  */
	movi	3 << 8, r39
	and	r0, r39, r40
	andc	r0, r39, r0
	stx.q	r17, r63, r8
	add.l	r17, r63, r8
	addi.l	r17, 8, r17
	beq/u	r39, r40, tr1
LOCAL(ia_r9_ld):	/* Store r9 and load its address.  */
	stx.q	r17, r63, r9
	add.l	r17, r63, r9
	blink	tr0, r63
LOCAL(ia_r2_push):	/* Push r2 onto the stack.  */
	movi	1, r38
	shlli	r38, 29, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r2
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_r3_push):	/* Push r3 onto the stack.  */
	movi	1, r38
	shlli	r38, 26, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r3
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_r4_push):	/* Push r4 onto the stack.  */
	movi	1, r38
	shlli	r38, 23, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r4
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_r5_push):	/* Push r5 onto the stack.  */
	movi	1, r38
	shlli	r38, 20, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r5
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_r6_push):	/* Push r6 onto the stack.  */
	movi	1, r38
	shlli	r38, 16, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r6
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_r7_push):	/* Push r7 onto the stack.  */
	movi	1 << 12, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r7
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_r8_push):	/* Push r8 onto the stack.  */
	movi	1 << 8, r39
	andc	r0, r39, r0
	stx.q	r17, r63, r8
	addi.l	r17, 8, r17
	blink	tr1, r63
LOCAL(ia_push_seq):	/* Push a sequence of registers onto the stack.  */
	andi	r0, 7 << 1, r38
	movi	(LOCAL(ia_end_of_push_seq) >> 16) & 65535, r40
	shlli	r38, 2, r39
	shori	LOCAL(ia_end_of_push_seq) & 65535, r40
	sub.l	r40, r39, r41
	ptabs/l	r41, tr2
	blink	tr2, r63
LOCAL(ia_stack_of_push_seq):	 /* Beginning of push sequence.  */
	stx.q	r17, r63, r3
	addi.l	r17, 8, r17
	stx.q	r17, r63, r4
	addi.l	r17, 8, r17
	stx.q	r17, r63, r5
	addi.l	r17, 8, r17
	stx.q	r17, r63, r6
	addi.l	r17, 8, r17
	stx.q	r17, r63, r7
	addi.l	r17, 8, r17
	stx.q	r17, r63, r8
	addi.l	r17, 8, r17
LOCAL(ia_r9_push):	/* Push r9 onto the stack.  */
	stx.q	r17, r63, r9
LOCAL(ia_return):	/* Return.  */
	blink	tr0, r63
LOCAL(ia_end_of_push_seq): /* Label used to compute the first push instruction.  */
	ENDFUNC(GLOBAL(GCC_shcompact_incoming_args))
#endif /* L_shcompact_incoming_args */
#endif
#if __SH5__
#ifdef L_nested_trampoline
#if __SH5__ == 32
	.section	.text..SHmedia32,"ax"
#else
	.text
#endif
	.align	3 /* It is copied in units of 8 bytes in SHmedia mode.  */
	.global	GLOBAL(GCC_nested_trampoline)
	FUNC(GLOBAL(GCC_nested_trampoline))
GLOBAL(GCC_nested_trampoline):
	.mode	SHmedia
	ptrel/u	r63, tr0
	gettr	tr0, r0
#if __SH5__ == 64
	ld.q	r0, 24, r1
#else
	ld.l	r0, 24, r1
#endif
	ptabs/l	r1, tr1
#if __SH5__ == 64
	ld.q	r0, 32, r1
#else
	ld.l	r0, 28, r1
#endif
	blink	tr1, r63

	ENDFUNC(GLOBAL(GCC_nested_trampoline))
#endif /* L_nested_trampoline */
#endif /* __SH5__ */
#if __SH5__ == 32
#ifdef L_push_pop_shmedia_regs
	.section	.text..SHmedia32,"ax"
	.mode	SHmedia
	.align	2
#ifndef __SH4_NOFPU__	
	.global	GLOBAL(GCC_push_shmedia_regs)
	FUNC(GLOBAL(GCC_push_shmedia_regs))
GLOBAL(GCC_push_shmedia_regs):
	addi.l	r15, -14*8, r15
	fst.d	r15, 13*8, dr62
	fst.d	r15, 12*8, dr60
	fst.d	r15, 11*8, dr58
	fst.d	r15, 10*8, dr56
	fst.d	r15,  9*8, dr54
	fst.d	r15,  8*8, dr52
	fst.d	r15,  7*8, dr50
	fst.d	r15,  6*8, dr48
	fst.d	r15,  5*8, dr46
	fst.d	r15,  4*8, dr44
	fst.d	r15,  3*8, dr42
	fst.d	r15,  2*8, dr40
	fst.d	r15,  1*8, dr38
	fst.d	r15,  0*8, dr36
#endif
	.global	GLOBAL(GCC_push_shmedia_regs_nofpu)
	FUNC(GLOBAL(GCC_push_shmedia_regs_nofpu))
GLOBAL(GCC_push_shmedia_regs_nofpu):
	ptabs/l	r18, tr0
	addi.l	r15, -27*8, r15
	gettr	tr7, r62
	gettr	tr6, r61
	gettr	tr5, r60
	st.q	r15, 26*8, r62
	st.q	r15, 25*8, r61
	st.q	r15, 24*8, r60
	st.q	r15, 23*8, r59
	st.q	r15, 22*8, r58
	st.q	r15, 21*8, r57
	st.q	r15, 20*8, r56
	st.q	r15, 19*8, r55
	st.q	r15, 18*8, r54
	st.q	r15, 17*8, r53
	st.q	r15, 16*8, r52
	st.q	r15, 15*8, r51
	st.q	r15, 14*8, r50
	st.q	r15, 13*8, r49
	st.q	r15, 12*8, r48
	st.q	r15, 11*8, r47
	st.q	r15, 10*8, r46
	st.q	r15,  9*8, r45
	st.q	r15,  8*8, r44
	st.q	r15,  7*8, r35
	st.q	r15,  6*8, r34
	st.q	r15,  5*8, r33
	st.q	r15,  4*8, r32
	st.q	r15,  3*8, r31
	st.q	r15,  2*8, r30
	st.q	r15,  1*8, r29
	st.q	r15,  0*8, r28
	blink	tr0, r63

#ifndef __SH4_NOFPU__	
	ENDFUNC(GLOBAL(GCC_push_shmedia_regs))
#endif
	ENDFUNC(GLOBAL(GCC_push_shmedia_regs_nofpu))
#ifndef __SH4_NOFPU__
	.global	GLOBAL(GCC_pop_shmedia_regs)
	FUNC(GLOBAL(GCC_pop_shmedia_regs))
GLOBAL(GCC_pop_shmedia_regs):
	pt	.L0, tr1
	movi	41*8, r0
	fld.d	r15, 40*8, dr62
	fld.d	r15, 39*8, dr60
	fld.d	r15, 38*8, dr58
	fld.d	r15, 37*8, dr56
	fld.d	r15, 36*8, dr54
	fld.d	r15, 35*8, dr52
	fld.d	r15, 34*8, dr50
	fld.d	r15, 33*8, dr48
	fld.d	r15, 32*8, dr46
	fld.d	r15, 31*8, dr44
	fld.d	r15, 30*8, dr42
	fld.d	r15, 29*8, dr40
	fld.d	r15, 28*8, dr38
	fld.d	r15, 27*8, dr36
	blink	tr1, r63
#endif
	.global	GLOBAL(GCC_pop_shmedia_regs_nofpu)
	FUNC(GLOBAL(GCC_pop_shmedia_regs_nofpu))
GLOBAL(GCC_pop_shmedia_regs_nofpu):
	movi	27*8, r0
.L0:
	ptabs	r18, tr0
	ld.q	r15, 26*8, r62
	ld.q	r15, 25*8, r61
	ld.q	r15, 24*8, r60
	ptabs	r62, tr7
	ptabs	r61, tr6
	ptabs	r60, tr5
	ld.q	r15, 23*8, r59
	ld.q	r15, 22*8, r58
	ld.q	r15, 21*8, r57
	ld.q	r15, 20*8, r56
	ld.q	r15, 19*8, r55
	ld.q	r15, 18*8, r54
	ld.q	r15, 17*8, r53
	ld.q	r15, 16*8, r52
	ld.q	r15, 15*8, r51
	ld.q	r15, 14*8, r50
	ld.q	r15, 13*8, r49
	ld.q	r15, 12*8, r48
	ld.q	r15, 11*8, r47
	ld.q	r15, 10*8, r46
	ld.q	r15,  9*8, r45
	ld.q	r15,  8*8, r44
	ld.q	r15,  7*8, r35
	ld.q	r15,  6*8, r34
	ld.q	r15,  5*8, r33
	ld.q	r15,  4*8, r32
	ld.q	r15,  3*8, r31
	ld.q	r15,  2*8, r30
	ld.q	r15,  1*8, r29
	ld.q	r15,  0*8, r28
	add.l	r15, r0, r15
	blink	tr0, r63

#ifndef __SH4_NOFPU__
	ENDFUNC(GLOBAL(GCC_pop_shmedia_regs))
#endif
	ENDFUNC(GLOBAL(GCC_pop_shmedia_regs_nofpu))
#endif /* __SH5__ == 32 */
#endif /* L_push_pop_shmedia_regs */
