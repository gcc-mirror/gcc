/* Assembly support functions for libgcc.
 *
 *   Copyright (C) 1997 Free Software Foundation, Inc.
 *   Contributed by Cygnus Support
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * In addition to the permissions in the GNU General Public License, the
 * Free Software Foundation gives you unlimited permission to link the
 * compiled version of this file into combinations with other programs,
 * and to distribute those combinations without any restriction coming
 * from the use of this file.  (The General Public License restrictions
 * do apply in other respects; for example, they cover modification of
 * the file, and distribution when not linked into a combine
 * executable.)
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 * 
 */


#ifdef L_udivsi3

/* For division, we use the following algorithm:
 *
 *	unsigned
 *	__divsi3 (unsigned a, unsigned b)
 *	{
 *	  unsigned al = a;
 *	  unsigned ah = 0;
 *	  unsigned tmpf;
 *	  int i;
 *
 *	  for (i = 32; i > 0; i--)
 *	    {
 *	      ah = (ah << 1) | (al >> 31);
 *	      tmpf = (ah >= b) ? 1 : 0;
 *	      ah -= ((tmpf) ? b : 0);
 *	      al = (al << 1) | tmpf;
 *	    }
 *
 *	  return al;	// for __udivsi3
 *	  return ah;	// for __umodsi3
 *	}
 */

	.file	"_udivsi3"
	.text
	.globl	__umodsi3
	.globl	__udivsi3
	.type	__umodsi3,@function
	.type	__udivsi3,@function
	.stabs	"libgcc1.asm",100,0,0,__umodsi3
	.stabs	"int:t(0,1)=r(0,1);-2147483648;2147483647;",128,0,0,0
	.stabs	"__umodsi3:F(0,1)",36,0,1,__umodsi3
	.stabs	"a:P(0,1)",64,0,1,2
	.stabs	"b:P(0,1)",64,0,1,3

__umodsi3:
	bra.s .Lmerge		|| orfg	f1,f1,1		; indicate this is __umodsi3
.Lumod:
	.size	__umodsi3,.Lumod-__umodsi3
	.stabs	"",36,0,0,.Lumod-__umodsi3

	.stabs	"__udivsi3:F(0,1)",36,0,1,__udivsi3
	.stabs	"a:P(0,1)",64,0,1,2
	.stabs	"b:P(0,1)",64,0,1,3
__udivsi3:
	andfg	f1,f1,0		|| nop			; indicate this is __udivsi3

.Lmerge:
	; r2 = al
	; r3 = b
	; r4 = ah
	; r5 = loop counter
	; f0 = tmpf
	; f1 = 1 if this is mod, 0 if this is div
	or	 r4,r0,0	|| sub	 r5,r0,-32	; ah = 0, loop = 32

.Lloop:
	src	 r4,r2,-1	|| sub	 r5,r5,1	; ah = (ah << 1) | (al >> 31); decrement loop count
	cmpuge	 f0,r4,r3	|| sra	 r2,r2,-1	; f0 = (ah >= b); al <<= 1
	sub/tx	 r4,r4,r3	|| or/tx r2,r2,1	; ah -= (tmpf) ? b : 0; al |= tmpf
	bratnz.s r5,.Lloop	|| nop			; loop back if not done
	jmp	 link           || or/xt r2,r0,r4	; if mod, update register, then return to user
.Ludiv:
	.size	 __udivsi3,.Ludiv-__udivsi3
	.stabs	"",36,0,0,.Ludiv-__udivsi3

#endif /* L_udivsi3 */


#ifdef L_divsi3

/* For division, we use the following algorithm:
 *
 *	unsigned
 *	__divsi3 (unsigned a, unsigned b)
 *	{
 *	  unsigned al = __builtin_abs (a);
 *	  unsigned b2 = __builtin_abs (b);
 *	  unsigned ah = 0;
 *	  unsigned tmpf;
 *	  int i;
 *
 *	  for (i = 32; i > 0; i--)
 *	    {
 *	      ah = (ah << 1) | (al >> 31);
 *	      tmpf = (ah >= b2) ? 1 : 0;
 *	      ah -= ((tmpf) ? b2 : 0);
 *	      al = (al << 1) | tmpf;
 *	    }
 *
 *	  if (a < 0)
 *	    ah = -ah, al = -al;
 *
 *	  if (b < 0)
 *	    al = -al;
 *
 *	  return al;	// for __divsi3
 *	  return ah;	// for __modsi3
 *	}
 */

	.file	"_divsi3"
	.text
	.globl	__modsi3
	.globl	__divsi3
	.type	__modsi3,@function
	.type	__divsi3,@function
	.stabs	"libgcc1.asm",100,0,0,__modsi3
	.stabs	"int:t(0,1)=r(0,1);-2147483648;2147483647;",128,0,0,0
	.stabs	"__modsi3:F(0,1)",36,0,1,__modsi3
	.stabs	"a:P(0,1)",64,0,1,2
	.stabs	"b:P(0,1)",64,0,1,3

__modsi3:
	bra.s .Lmerge		|| orfg	f1,f1,1		; indicate this is __modsi3
.Lmod:
	.size	__modsi3,.Lmod-__modsi3
	.stabs	"",36,0,0,.Lmod-__modsi3

	.stabs	"__divsi3:F(0,1)",36,0,1,__divsi3
	.stabs	"a:P(0,1)",64,0,1,2
	.stabs	"b:P(0,1)",64,0,1,3
__divsi3:
	andfg	f1,f1,0		|| nop			; indicate this is __divsi3

.Lmerge:
	; r2 = al
	; r3 = b2
	; r4 = ah
	; r5 = loop counter
	; r6 = a
	; r7 = b
	; f0 = tmpf
	; f1 = 1 if this is mod, 0 if this is div
	or	 r6,r0,r2	|| or	  r7,r0,r3	; copy original inputs
	abs	 r2,r2		|| abs	  r3,r3		; make both postive
	or	 r4,r0,0	|| sub	 r5,r0,-32	; ah = 0, loop = 32

.Lloop:
	src	 r4,r2,-1	|| sub	  r5,r5,1	; ah = (ah << 1) | (al >> 31); decrement loop count
	cmpuge	 f0,r4,r3	|| sra	  r2,r2,-1	; f0 = (ah >= b); al <<= 1
	sub/tx	 r4,r4,r3	|| or/tx  r2,r2,1	; ah -= (tmpf) ? b : 0; al |= tmpf
	bratnz.s r5,.Lloop	|| nop			; loop back if not done
	cmplt    f0,r6,0	|| nop			; f0 = (a < 0)

	sub/tx	 r2,r0,r2	|| sub/tx r4,r0,r4	; negate both al, ah if (a < 0)
	cmplt	 f0,r7,0	-> sub/tx r2,r0,r2	; negate al if (b < 0)
	jmp	 link		|| or/xt  r2,r0,r4	; update result if mod; return to user
.Ldiv:
	.size	 __divsi3,.Ldiv-__divsi3
	.stabs	"",36,0,0,.Ldiv-__divsi3

#endif /* L_divsi3 */
