/* longlong.h -- definitions for mixed size 32/64 bit arithmetic.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

   This definition file is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation; either
   version 2, or (at your option) any later version.

   This definition file is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32
#endif

#define __BITS4 (LONG_TYPE_SIZE / 4)
#define __ll_B (1L << (LONG_TYPE_SIZE / 2))
#define __ll_lowpart(t) ((unsigned long int) (t) % __ll_B)
#define __ll_highpart(t) ((unsigned long int) (t) / __ll_B)

/* Define auxiliary asm macros.

   1) umul_ppmm(high_prod, low_prod, multipler, multiplicand)
   multiplies two unsigned long integers MULTIPLER and MULTIPLICAND,
   and generates a two unsigned word product in HIGH_PROD and
   LOW_PROD.

   2) __umulsidi3(a,b) multiplies two unsigned long integers A and B,
   and returns a long long product.  This is just a variant of umul_ppmm.

   3) udiv_qrnnd(quotient, remainder, high_numerator, low_numerator,
   denominator) divides a two-word unsigned integer, composed by the
   integers HIGH_NUMERATOR and LOW_NUMERATOR, by DENOMINATOR and
   places the quotient in QUOTIENT and the remainder in REMAINDER.
   HIGH_NUMERATOR must be less than DENOMINATOR for correct operation.
   If, in addition, the most significant bit of DENOMINATOR must be 1,
   then the pre-processor symbol UDIV_NEEDS_NORMALIZATION is defined to 1.

   4) count_leading_zeros(count, x) counts the number of zero-bits from
   the msb to the first non-zero bit.  This is the number of steps X
   needs to be shifted left to set the msb.  Undefined for X == 0.

   5) add_ssaaaa(high_sum, low_sum, high_addend_1, low_addend_1,
   high_addend_2, low_addend_2) adds two two-word unsigned integers,
   composed by HIGH_ADDEND_1 and LOW_ADDEND_1, and HIGH_ADDEND_2 and
   LOW_ADDEND_2 respectively.  The result is placed in HIGH_SUM and
   LOW_SUM.  Overflow (i.e. carry out) is not stored anywhere, and is
   lost.

   6) sub_ddmmss(high_difference, low_difference, high_minuend,
   low_minuend, high_subtrahend, low_subtrahend) subtracts two
   two-word unsigned integers, composed by HIGH_MINUEND_1 and
   LOW_MINUEND_1, and HIGH_SUBTRAHEND_2 and LOW_SUBTRAHEND_2
   respectively.  The result is placed in HIGH_DIFFERENCE and
   LOW_DIFFERENCE.  Overflow (i.e. carry out) is not stored anywhere,
   and is lost.

   If any of these macros are left undefined for a particular CPU,
   C macros are used.  */

/* The CPUs come in alphabetical order below.

   Please add support for more CPUs here, or improve the current support
   for the CPUs below!
   (E.g. WE32100, i960, IBM360.)  */

#if defined (__GNUC__) && !defined (NO_ASM)

/* We sometimes need to clobber "cc" with gcc2, but that would not be
   understood by gcc1.  Use cpp to avoid major code duplication.  */
#if __GNUC__ < 2
#define __CLOBBER_CC
#define __AND_CLOBBER_CC
#else /* __GNUC__ >= 2 */
#define __CLOBBER_CC : "cc"
#define __AND_CLOBBER_CC , "cc"
#endif /* __GNUC__ < 2 */

#if defined (__a29k__) || defined (___AM29K__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add %1,%4,%5
	addc %0,%2,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	    "=&r" ((unsigned long int)(sl))				\
	   : "%r" ((unsigned long int)(ah)),				\
	     "rI" ((unsigned long int)(bh)),				\
	     "%r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub %1,%4,%5
	subc %0,%2,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "r" ((unsigned long int)(ah)),				\
	     "rI" ((unsigned long int)(bh)),				\
	     "r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl)))
#define umul_ppmm(xh, xl, m0, m1) \
  do {									\
    unsigned long int __m0 = (m0), __m1 = (m1);				\
    __asm__ ("multiplu %0,%1,%2"					\
	     : "=r" ((unsigned long int)(xl))				\
	     : "r" (__m0),						\
	       "r" (__m1));						\
    __asm__ ("multmu %0,%1,%2"						\
	     : "=r" ((unsigned long int)(xh))				\
	     : "r" (__m0),						\
	       "r" (__m1));						\
  } while (0)
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("dividu %0,%3,%4"						\
	   : "=r" ((unsigned long int)(q)),				\
	     "=q" ((unsigned long int)(r))				\
	   : "1" ((unsigned long int)(n1)),				\
	     "r" ((unsigned long int)(n0)),				\
	     "r" ((unsigned long int)(d)))
#define count_leading_zeros(count, x) \
    __asm__ ("clz %0,%1"						\
	     : "=r" ((unsigned long int)(count))			\
	     : "r" ((unsigned long int)(x)))
#endif /* __a29k__ */

#if defined (__arm__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("adds %1,%4,%5
	adc %0,%2,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%r" ((unsigned long int)(ah)),				\
	     "rI" ((unsigned long int)(bh)),				\
	     "%r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subs %1,%4,%5
	sbc %0,%2,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "r" ((unsigned long int)(ah)),				\
	     "rI" ((unsigned long int)(bh)),				\
	     "r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl)))
#endif /* __arm__ */

#if defined (__gmicro__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add.w %5,%1
	addx %3,%0"							\
	   : "=g" ((unsigned long int)(sh)),				\
	     "=&g" ((unsigned long int)(sl))				\
	   : "%0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "%1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub.w %5,%1
	subx %3,%0"							\
	   : "=g" ((unsigned long int)(sh)),				\
	     "=&g" ((unsigned long int)(sl))				\
	   : "0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define umul_ppmm(ph, pl, m0, m1) \
  __asm__ ("mulx %3,%0,%1"						\
	   : "=g" ((unsigned long int)(ph)),				\
	     "=r" ((unsigned long int)(pl))				\
	   : "%0" ((unsigned long int)(m0)),				\
	     "g" ((unsigned long int)(m1)))
#define udiv_qrnnd(q, r, nh, nl, d) \
  __asm__ ("divx %4,%0,%1"						\
	   : "=g" ((unsigned long int)(q)),				\
	     "=r" ((unsigned long int)(r))				\
	   : "1" ((unsigned long int)(nh)),				\
	     "0" ((unsigned long int)(nl)),				\
	     "g" ((unsigned long int)(d)))
#define count_leading_zeros(count, x) \
  __asm__ ("bsch/1 %1,%0"						\
	   : "=g" (count)						\
	   : "g" ((unsigned long int)(x)),				\
	     "0" (0UL))
#endif

#if defined (__hppa)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add %4,%5,%1
	addc %2,%3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%rM" ((unsigned long int)(ah)),				\
	     "rM" ((unsigned long int)(bh)),				\
	     "%rM" ((unsigned long int)(al)),				\
	     "rM" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub %5,%4,%1
	subb %3,%2,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "rM" ((unsigned long int)(ah)),				\
	     "rM" ((unsigned long int)(bh)),				\
	     "rM" ((unsigned long int)(al)),				\
	     "rM" ((unsigned long int)(bl)))
#if defined (_PA_RISC1_1)
#define umul_ppmm(w1, w0, u, v) \
  do {									\
    union								\
      {									\
	long long __f;							\
	struct {unsigned long int __w1, __w0;} __w1w0;			\
      } __t;								\
    __asm__ ("xmpyu %1,%2,%0"						\
	     : "=x" (__t.__f)						\
	     : "x" ((u)),						\
	       "x" ((v)));						\
    (w1) = __t.__w1w0.__w1;						\
    (w0) = __t.__w1w0.__w0;						\
     } while (0)
#define UMUL_TIME 8
#else
#define UMUL_TIME 30
#endif
#define UDIV_TIME 40
#endif

#if defined (__i386__) || defined (__i486__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addl %5,%1
	adcl %3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "%1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subl %5,%1
	sbbl %3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mull %3"							\
	   : "=a" ((unsigned long int)(w0)),				\
	     "=d" ((unsigned long int)(w1))				\
	   : "%0" ((unsigned long int)(u)),				\
	     "rm" ((unsigned long int)(v)))
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("divl %4"							\
	   : "=a" ((unsigned long int)(q)),				\
	     "=d" ((unsigned long int)(r))				\
	   : "0" ((unsigned long int)(n0)),				\
	     "1" ((unsigned long int)(n1)),				\
	     "rm" ((unsigned long int)(d)))
#define count_leading_zeros(count, x) \
  do {									\
    unsigned long int __cbtmp;						\
    __asm__ ("bsrl %1,%0"						\
	     : "=r" (__cbtmp) : "rm" ((unsigned long int)(x)));		\
    (count) = __cbtmp ^ 31;						\
  } while (0)
#define UMUL_TIME 40
#define UDIV_TIME 40
#endif /* 80x86 */

#if defined (__i860__)
#if 0
/* Make sure these patterns really improve the code before
   switching them on.  */
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  do {									\
    union								\
      {									\
	long long int ll;						\
	struct {unsigned long int l, h;} i;				\
      }  __a, __b, __s;							\
    __a.i.l = (al); __a.i.h = (ah);					\
    __b.i.l = (bl); __b.i.h = (bh);					\
    __asm__ ("fiadd.dd %1,%2,%0"					\
	     : "=f" (__s.ll)						\
	     : "%f" (__a.ll), "f" (__b.ll));				\
    (sh) = __s.i.h; (sl) = __s.i.l;					\
    } while (0)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  do {									\
    union								\
      {									\
	long long int ll;						\
	struct {unsigned long int l, h;} i;				\
      }  __a, __b, __s;							\
    __a.i.l = (al); __a.i.h = (ah);					\
    __b.i.l = (bl); __b.i.h = (bh);					\
    __asm__ ("fisub.dd %1,%2,%0"					\
	     : "=f" (__s.ll)						\
	     : "%f" (__a.ll), "f" (__b.ll));				\
    (sh) = __s.i.h; (sl) = __s.i.l;					\
    } while (0)
#endif
#endif /* __i860__ */

#if defined (___IBMR2__) /* IBM RS6000 */
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("a%I5 %1,%4,%5
	ae %0,%2,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%r" ((unsigned long int)(ah)),				\
	     "r" ((unsigned long int)(bh)),				\
	     "%r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sf%I4 %1,%5,%4
	sfe %0,%3,%2"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "r" ((unsigned long int)(ah)),				\
	     "r" ((unsigned long int)(bh)),				\
	     "rI" ((unsigned long int)(al)),				\
	     "r" ((unsigned long int)(bl)))
#define umul_ppmm(xh, xl, m0, m1) \
  do {									\
    unsigned long int __m0 = (m0), __m1 = (m1);				\
    __asm__ ("mul %0,%2,%3"						\
	     : "=r" ((unsigned long int)(xh)),				\
	       "=q" ((unsigned long int)(xl))				\
	     : "r" (__m0),						\
	       "r" (__m1));						\
    (xh) += ((((signed long int) __m0 >> 31) & __m1)			\
	     + (((signed long int) __m1 >> 31) & __m0));		\
  } while (0)
#define smul_ppmm(xh, xl, m0, m1) \
  __asm__ ("mul %0,%2,%3"						\
	   : "=r" ((unsigned long int)(xh)),				\
	     "=q" ((unsigned long int)(xl))				\
	   : "r" (m0),							\
	     "r" (m1))
#define UMUL_TIME 8
#define sdiv_qrnnd(q, r, nh, nl, d) \
  __asm__ ("div %0,%2,%4"						\
	   : "=r" (q), "=q" (r)						\
	   : "r" (nh), "1" (nl), "r" (d))
#define UDIV_TIME 40
#define UDIV_NEEDS_NORMALIZATION 1
#define count_leading_zeros(count, x) \
  __asm__ ("cntlz %0,%1"						\
	   : "=r" ((unsigned long int)(count))				\
	   : "r" ((unsigned long int)(x)))
#endif /* ___IBMR2__ */

#if defined (__mc68000__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("add%.l %5,%1
	addx%.l %3,%0"							\
	   : "=d" ((unsigned long int)(sh)),				\
	     "=&d" ((unsigned long int)(sl))				\
	   : "%0" ((unsigned long int)(ah)),				\
	     "d" ((unsigned long int)(bh)),				\
	     "%1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("sub%.l %5,%1
	subx%.l %3,%0"							\
	   : "=d" ((unsigned long int)(sh)),				\
	     "=&d" ((unsigned long int)(sl))				\
	   : "0" ((unsigned long int)(ah)),				\
	     "d" ((unsigned long int)(bh)),				\
	     "1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#if defined (__mc68020__) || defined (__NeXT__) || defined(mc68020)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulu%.l %3,%1:%0"						\
	   : "=d" ((unsigned long int)(w0)),				\
	     "=d" ((unsigned long int)(w1))				\
	   : "%0" ((unsigned long int)(u)),				\
	     "dmi" ((unsigned long int)(v)))
#define UMUL_TIME 45
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("divu%.l %4,%1:%0"						\
	   : "=d" ((unsigned long int)(q)),				\
	     "=d" ((unsigned long int)(r))				\
	   : "0" ((unsigned long int)(n0)),				\
	     "1" ((unsigned long int)(n1)),				\
	     "dmi" ((unsigned long int)(d)))
#define UDIV_TIME 90
#define sdiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("divs%.l %4,%1:%0"						\
	   : "=d" ((unsigned long int)(q)),				\
	     "=d" ((unsigned long int)(r))				\
	   : "0" ((unsigned long int)(n0)),				\
	     "1" ((unsigned long int)(n1)),				\
	     "dmi" ((unsigned long int)(d)))
#define count_leading_zeros(count, x) \
  __asm__ ("bfffo %1{%b2:%b2},%0"					\
	   : "=d" ((unsigned long int)(count))				\
	   : "od" ((unsigned long int)(x)), "n" (0))
#else /* not mc68020 */
#define umul_ppmm(xh, xl, a, b) \
  __asm__ ("| Inlined umul_ppmm
	movel	%2,d0
	movel	%3,d1
	movel	d0,d2
	swap	d0
	movel	d1,d3
	swap	d1
	movew	d2,d4
	mulu	d3,d4
	mulu	d1,d2
	mulu	d0,d3
	mulu	d0,d1
	movel	d4,d0
	eorw	d0,d0
	swap	d0
	addl	d0,d2
	addl	d3,d2
	jcc	1f
	addl	#65536,d1
1:	swap	d2
	moveq	#0,d0
	movew	d2,d0
	movew	d4,d2
	movel	d2,%1
	addl	d1,d0
	movel	d0,%0"							\
	   : "=g" ((unsigned long int)(xh)),				\
	     "=g" ((unsigned long int)(xl))				\
	   : "g" ((unsigned long int)(a)),				\
	     "g" ((unsigned long int)(b))				\
	   : "d0", "d1", "d2", "d3", "d4")
#define UMUL_TIME 100
#define UDIV_TIME 400
#endif /* not mc68020 */
#endif /* mc68000 */

#if defined (__m88000__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addu.co %1,%r4,%r5
	addu.ci %0,%r2,%r3"						\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%rJ" ((unsigned long int)(ah)),				\
	     "rJ" ((unsigned long int)(bh)),				\
	     "%rJ" ((unsigned long int)(al)),				\
	     "rJ" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subu.co %1,%r4,%r5
	subu.ci %0,%r2,%r3"						\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "rJ" ((unsigned long int)(ah)),				\
	     "rJ" ((unsigned long int)(bh)),				\
	     "rJ" ((unsigned long int)(al)),				\
	     "rJ" ((unsigned long int)(bl)))
#define UMUL_TIME 17
#define UDIV_TIME 150
#define count_leading_zeros(count, x) \
  do {									\
    unsigned long int __cbtmp;						\
    __asm__ ("ff1 %0,%1"						\
	     : "=r" (__cbtmp)						\
	     : "r" ((unsigned long int)(x)));				\
    (count) = __cbtmp ^ 31;						\
  } while (0)
#if defined (__mc88110__)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulu.d	r10,%2,%3
	or	%0,r10,0
	or	%1,r11,0"						\
	   : "=r" (w1),							\
	     "=r" (w0)							\
	   : "r" (u),							\
	     "r" (v)							\
	   : "r10", "r11")
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("or	r10,%2,0
	or	r11,%3,0
	divu.d	r10,r10,%4
	mulu	%1,%4,r11
	subu	%1,%3,%1
	or	%0,r11,0"						\
	   : "=r" (q),							\
	     "=&r" (r)							\
	   : "r" (n1),							\
	     "r" (n0),							\
	     "r" (d)							\
	   : "r10", "r11")
#endif
#endif /* __m88000__ */

#if defined (__mips__)
#define umul_ppmm(w1, w0, u, v)						\
  __asm__ ("multu %2,%3							\
	mflo %0								\
	mfhi %1"							\
	   : "=d" ((unsigned long int)(w0)),				\
	     "=d" ((unsigned long int)(w1))				\
	   : "d" ((unsigned long int)(u)),				\
	     "d" ((unsigned long int)(v)))
#define UMUL_TIME 5
#define UDIV_TIME 100
#endif /* __mips__ */

#if defined (__ns32000__)
#define __umulsidi3(u, v) \
  ({long long int __w;							\
    __asm__ ("meid %2,%0"						\
	     : "=g" (__w)						\
	     : "%0" ((unsigned long int)(u)),				\
	       "g" ((unsigned long int)(v)));				\
    __w; })
#define div_qrnnd(q, r, n1, n0, d) \
  __asm__ ("movd %2,r0
	movd %3,r1
	deid %4,r0
	movd r1,%0
	movd r0,%1"							\
	   : "=g" ((unsigned long int)(q)),				\
	     "=g" ((unsigned long int)(r))				\
	   : "g" ((unsigned long int)(n0)),				\
	     "g" ((unsigned long int)(n1)),				\
	     "g" ((unsigned long int)(d))				\
	   : "r0", "r1")
#endif /* __ns32000__ */

#if defined (__pyr__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addw	%5,%1
	addwc	%3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "%1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subw	%5,%1
	subwb	%3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
/* This insn doesn't work on ancient pyramids.  */
#define umul_ppmm(w1, w0, u, v) \
  ({union {long long int ll;struct {unsigned long int h, l;} i;} __xx;	\
  __xx.i.l = u;								\
  __asm__ ("uemul %3,%0"						\
	   : "=r" (__xx.i.h),						\
	     "=r" (__xx.i.l)						\
	   : "1" (__xx.i.l),						\
	     "g" (v));							\
  (w1) = __xx.i.h; (w0) = __xx.i.l;})
#endif /* __pyr__ */

#if defined (__ibm032__) /* RT/ROMP */
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("a %1,%5
	ae %0,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%0" ((unsigned long int)(ah)),				\
	     "r" ((unsigned long int)(bh)),				\
	     "%1" ((unsigned long int)(al)),				\
	     "r" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("s %1,%5
	se %0,%3"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "0" ((unsigned long int)(ah)),				\
	     "r" ((unsigned long int)(bh)),				\
	     "1" ((unsigned long int)(al)),				\
	     "r" ((unsigned long int)(bl)))
#define umul_ppmm(ph, pl, m0, m1) \
  do {									\
    unsigned long int __m0 = (m0), __m1 = (m1);				\
    __asm__ (								\
       "s	r2,r2
	mts	r10,%2
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	m	r2,%3
	cas	%0,r2,r0
	mfs	r10,%1"							\
	     : "=r" ((unsigned long int)(ph)),				\
	       "=r" ((unsigned long int)(pl))				\
	     : "%r" (__m0),						\
		"r" (__m1)						\
	     : "r2");							\
    (ph) += ((((signed long int) __m0 >> 31) & __m1)			\
	     + (((signed long int) __m1 >> 31) & __m0));		\
  } while (0)
#define UMUL_TIME 20
#define UDIV_TIME 200
#define count_leading_zeros(count, x) \
  do {									\
    if ((x) >= 0x10000)							\
      __asm__ ("clz	%0,%1"						\
	       : "=r" ((unsigned long int)(count))			\
	       : "r" ((unsigned long int)(x) >> 16));			\
    else								\
      {									\
	__asm__ ("clz	%0,%1"						\
		 : "=r" ((unsigned long int)(count))			\
		 : "r" ((unsigned long int)(x)));			\
	(count) += 16;							\
      }									\
  } while (0)
#endif

#if defined (__sparc__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addcc %4,%5,%1
	addx %2,%3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "%r" ((unsigned long int)(ah)),				\
	     "rI" ((unsigned long int)(bh)),				\
	     "%r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl))				\
	   __CLOBBER_CC)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subcc %4,%5,%1
	subx %2,%3,%0"							\
	   : "=r" ((unsigned long int)(sh)),				\
	     "=&r" ((unsigned long int)(sl))				\
	   : "r" ((unsigned long int)(ah)),				\
	     "rI" ((unsigned long int)(bh)),				\
	     "r" ((unsigned long int)(al)),				\
	     "rI" ((unsigned long int)(bl))				\
	   __CLOBBER_CC)
#if defined (__sparcv8__)
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("umul %2,%3,%1;rd %%y,%0"					\
	   : "=r" ((unsigned long int)(w1)),				\
	     "=r" ((unsigned long int)(w0))				\
	   : "r" ((unsigned long int)(u)),				\
	     "r" ((unsigned long int)(v)))
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("mov %2,%%y;nop;nop;nop;udiv %3,%4,%0;umul %0,%4,%1;sub %3,%1,%1"\
	   : "=&r" ((unsigned long int)(q)),				\
	     "=&r" ((unsigned long int)(r))				\
	   : "r" ((unsigned long int)(n1)),				\
	     "r" ((unsigned long int)(n0)),				\
	     "r" ((unsigned long int)(d)))
#else
/* SPARC without integer multiplication and divide instructions.
   (i.e. at least Sun4/20,40,60,65,75,110,260,280,330,360,380,470,490) */
#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("! Inlined umul_ppmm
	wr	%%g0,%2,%%y	! SPARC has 0-3 delay insn after a wr
	sra	%3,31,%%g2	! Don't move this insn
	and	%2,%%g2,%%g2	! Don't move this insn
	andcc	%%g0,0,%%g1	! Don't move this insn
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,%3,%%g1
	mulscc	%%g1,0,%%g1
	add	%%g1,%%g2,%0
	rd	%%y,%1"							\
	   : "=r" ((unsigned long int)(w1)),				\
	     "=r" ((unsigned long int)(w0))				\
	   : "%rI" ((unsigned long int)(u)),				\
	     "r" ((unsigned long int)(v))				\
	   : "%g1", "%g2" __AND_CLOBBER_CC)
#define UMUL_TIME 39		/* 39 instructions */
/* It's quite necessary to add this much assembler for the sparc.
   The default udiv_qrnnd (in C) is more than 10 times slower!  */
#define udiv_qrnnd(q, r, n1, n0, d) \
  __asm__ ("! Inlined udiv_qrnnd
	mov	32,%%g1
	subcc	%1,%2,%%g0
1:	bcs	5f
	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb
	sub	%1,%2,%1	! this kills msb of n
	addx	%1,%1,%1	! so this can't give carry
	subcc	%%g1,1,%%g1
2:	bne	1b
	 subcc	%1,%2,%%g0
	bcs	3f
	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb
	b	3f
	 sub	%1,%2,%1	! this kills msb of n
4:	sub	%1,%2,%1
5:	addxcc	%1,%1,%1
	bcc	2b
	 subcc	%%g1,1,%%g1
! Got carry from n.  Subtract next step to cancel this carry.
	bne	4b
	 addcc	%0,%0,%0	! shift n1n0 and a 0-bit in lsb
	sub	%1,%2,%1
3:	xnor	%0,0,%0
	! End of inline udiv_qrnnd"					\
	   : "=&r" ((unsigned long int)(q)),				\
	     "=&r" ((unsigned long int)(r))				\
	   : "r" ((unsigned long int)(d)),				\
	     "1" ((unsigned long int)(n1)),				\
	     "0" ((unsigned long int)(n0)) : "%g1" __AND_CLOBBER_CC)
#define UDIV_TIME (3+7*32)	/* 7 instructions/iteration. 32 iterations. */
#endif /* __sparc8__ */
#endif /* __sparc__ */

#if defined (__vax__)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  __asm__ ("addl2 %5,%1
	adwc %3,%0"							\
	   : "=g" ((unsigned long int)(sh)),				\
	     "=&g" ((unsigned long int)(sl))				\
	   : "%0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "%1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  __asm__ ("subl2 %5,%1
	sbwc %3,%0"							\
	   : "=g" ((unsigned long int)(sh)),				\
	     "=&g" ((unsigned long int)(sl))				\
	   : "0" ((unsigned long int)(ah)),				\
	     "g" ((unsigned long int)(bh)),				\
	     "1" ((unsigned long int)(al)),				\
	     "g" ((unsigned long int)(bl)))
#define umul_ppmm(xh, xl, m0, m1) \
  do {									\
    union {long long int ll;struct {unsigned long int l, h;} i;} __xx;	\
    unsigned long int __m0 = (m0), __m1 = (m1);				\
    __asm__ ("emul %1,%2,$0,%0"						\
	     : "=r" (__xx.ll)						\
	     : "g" (__m0),						\
	       "g" (__m1));						\
    (xh) = __xx.i.h; (xl) = __xx.i.l;					\
    (xh) += ((((signed long int) __m0 >> 31) & __m1)			\
	     + (((signed long int) __m1 >> 31) & __m0));		\
  } while (0)
#endif /* __vax__ */

#endif /* __GNUC__ */

/* If this machine has no inline assembler, use C macros.  */

#if !defined (add_ssaaaa)
#define add_ssaaaa(sh, sl, ah, al, bh, bl) \
  do {									\
    unsigned long int __x;						\
    __x = (al) + (bl);							\
    (sh) = (ah) + (bh) + (__x < (al));					\
    (sl) = __x;								\
  } while (0)
#endif

#if !defined (sub_ddmmss)
#define sub_ddmmss(sh, sl, ah, al, bh, bl) \
  do {									\
    unsigned long int __x;						\
    __x = (al) - (bl);							\
    (sh) = (ah) - (bh) - (__x > (al));					\
    (sl) = __x;								\
  } while (0)
#endif

#if !defined (umul_ppmm)
#define umul_ppmm(w1, w0, u, v)						\
  do {									\
    unsigned long int __x0, __x1, __x2, __x3;				\
    unsigned int __ul, __vl, __uh, __vh;				\
									\
    __ul = __ll_lowpart (u);						\
    __uh = __ll_highpart (u);						\
    __vl = __ll_lowpart (v);						\
    __vh = __ll_highpart (v);						\
									\
    __x0 = (unsigned long int) __ul * __vl;				\
    __x1 = (unsigned long int) __ul * __vh;				\
    __x2 = (unsigned long int) __uh * __vl;				\
    __x3 = (unsigned long int) __uh * __vh;				\
									\
    __x1 += __ll_highpart (__x0);/* this can't give carry */		\
    __x1 += __x2;		/* but this indeed can */		\
    if (__x1 < __x2)		/* did we get it? */			\
      __x3 += __ll_B;		/* yes, add it in the proper pos. */	\
									\
    (w1) = __x3 + __ll_highpart (__x1);					\
    (w0) = __ll_lowpart (__x1) * __ll_B + __ll_lowpart (__x0);		\
  } while (0)
#endif

#if !defined (__umulsidi3)
#define __umulsidi3(u, v) \
  ({long_long __w;							\
    umul_ppmm (__w.s.high, __w.s.low, u, v);				\
    __w.ll; })
#endif

/* Define this unconditionally, so it can be used for debugging.  */
#define __udiv_qrnnd_c(q, r, n1, n0, d) \
  do {									\
    unsigned int __d1, __d0, __q1, __q0;				\
    unsigned long int __r1, __r0, __m;					\
    __d1 = __ll_highpart (d);						\
    __d0 = __ll_lowpart (d);						\
									\
    __r1 = (n1) % __d1;							\
    __q1 = (n1) / __d1;							\
    __m = (unsigned long int) __q1 * __d0;				\
    __r1 = __r1 * __ll_B | __ll_highpart (n0);				\
    if (__r1 < __m)							\
      {									\
	__q1--, __r1 += (d);						\
	if (__r1 >= (d)) /* i.e. we didn't get carry when adding to __r1 */\
	  if (__r1 < __m)						\
	    __q1--, __r1 += (d);					\
      }									\
    __r1 -= __m;							\
									\
    __r0 = __r1 % __d1;							\
    __q0 = __r1 / __d1;							\
    __m = (unsigned long int) __q0 * __d0;				\
    __r0 = __r0 * __ll_B | __ll_lowpart (n0);				\
    if (__r0 < __m)							\
      {									\
	__q0--, __r0 += (d);						\
	if (__r0 >= (d))						\
	  if (__r0 < __m)						\
	    __q0--, __r0 += (d);					\
      }									\
    __r0 -= __m;							\
									\
    (q) = (unsigned long int) __q1 * __ll_B | __q0;			\
    (r) = __r0;								\
  } while (0)
/* If udiv_qrnnd was not defined for this processor, use __udiv_qrnnd_c.  */
#if !defined (udiv_qrnnd)
#define UDIV_NEEDS_NORMALIZATION 1
#define udiv_qrnnd __udiv_qrnnd_c
#endif

#if !defined (count_leading_zeros)
extern const unsigned char __clz_tab[];
#define count_leading_zeros(count, x) \
  do {									\
    unsigned long int __xr = (x);					\
    unsigned int __a;							\
									\
    if (LONG_TYPE_SIZE <= 32)						\
      {									\
	__a = __xr < (1<<2*__BITS4)					\
	  ? (__xr < (1<<__BITS4) ? 0 : __BITS4)				\
	  : (__xr < (1<<3*__BITS4) ?  2*__BITS4 : 3*__BITS4);		\
      }									\
    else								\
      {									\
	for (__a = LONG_TYPE_SIZE - 8; __a > 0; __a -= 8)		\
	  if (((__xr >> __a) & 0xff) != 0)				\
	    break;							\
      }									\
									\
    (count) = LONG_TYPE_SIZE - (__clz_tab[__xr >> __a] + __a);		\
  } while (0)
#endif

#ifndef UDIV_NEEDS_NORMALIZATION
#define UDIV_NEEDS_NORMALIZATION 0
#endif
