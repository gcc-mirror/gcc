/*
** libgcc support for software floating point.
** Copyright (C) 1991 by Pipeline Associates, Inc.  All rights reserved.
** Permission is granted to do *anything* you want with this file,
** commercial or otherwise, provided this message remains intact.  So there!
** I would appreciate receiving any updates/patches/changes that anyone
** makes, and am willing to be the repository for said changes (am I
** making a big mistake?).

Warning! Only single-precision is actually implemented.  This file
won't really be much use until double-precision is supported.

However, once that is done, this file might eventually become a
replacement for libgcc1.c.  It might also make possible
cross-compilation for an IEEE target machine from a non-IEEE
host such as a VAX.

If you'd like to work on completing this, please talk to rms@gnu.ai.mit.edu.

--> Double precision floating support added by James Carlson on 20 April 1998.

**
** Pat Wood
** Pipeline Associates, Inc.
** pipeline!phw@motown.com or
** sun!pipeline!phw or
** uunet!motown!pipeline!phw
**
** 05/01/91 -- V1.0 -- first release to gcc mailing lists
** 05/04/91 -- V1.1 -- added float and double prototypes and return values
**                  -- fixed problems with adding and subtracting zero
**                  -- fixed rounding in truncdfsf2
**                  -- fixed SWAP define and tested on 386
*/

/*
** The following are routines that replace the libgcc soft floating point
** routines that are called automatically when -msoft-float is selected.
** The support single and double precision IEEE format, with provisions
** for byte-swapped machines (tested on 386).  Some of the double-precision
** routines work at full precision, but most of the hard ones simply punt
** and call the single precision routines, producing a loss of accuracy.
** long long support is not assumed or included.
** Overall accuracy is close to IEEE (actually 68882) for single-precision
** arithmetic.  I think there may still be a 1 in 1000 chance of a bit
** being rounded the wrong way during a multiply.  I'm not fussy enough to
** bother with it, but if anyone is, knock yourself out.
**
** Efficiency has only been addressed where it was obvious that something
** would make a big difference.  Anyone who wants to do this right for
** best speed should go in and rewrite in assembler.
**
** I have tested this only on a 68030 workstation and 386/ix integrated
** in with -msoft-float.
*/

/* the following deal with IEEE single-precision numbers */
#define EXCESS		126
#define SIGNBIT		0x80000000
#define HIDDEN		(1 << 23)
#define SIGN(fp)	((fp) & SIGNBIT)
#define EXP(fp)		(((fp) >> 23) & 0xFF)
#define MANT(fp)	(((fp) & 0x7FFFFF) | HIDDEN)
#define PACK(s,e,m)	((s) | ((e) << 23) | (m))

/* the following deal with IEEE double-precision numbers */
#define EXCESSD		1022
#define HIDDEND		(1 << 20)
#define EXPD(fp)	(((fp.l.upper) >> 20) & 0x7FF)
#define SIGND(fp)	((fp.l.upper) & SIGNBIT)
#define MANTD(fp)	(((((fp.l.upper) & 0xFFFFF) | HIDDEND) << 10) | \
				(fp.l.lower >> 22))
#define HIDDEND_LL	((long long)1 << 52)
#define MANTD_LL(fp)	((fp.ll & (HIDDEND_LL-1)) | HIDDEND_LL)
#define PACKD_LL(s,e,m)	(((long long)((s)+((e)<<20))<<32)|(m))

/* define SWAP for 386/960 reverse-byte-order brain-damaged CPUs */
union double_long {
    double d;
#ifdef SWAP
    struct {
      unsigned long lower;
      long upper;
    } l;
#else
    struct {
      long upper;
      unsigned long lower;
    } l;
#endif
    long long ll;
};

union float_long
  {
    float f;
    long l;
  };

/* add two floats */
float
__addsf3 (float a1, float a2)
{
  register long mant1, mant2;
  register union float_long fl1, fl2;
  register int exp1, exp2;
  int sign = 0;

  fl1.f = a1;
  fl2.f = a2;

  /* check for zero args */
  if (!fl1.l) {
      fl1.f = fl2.f;
      goto test_done;
  }
  if (!fl2.l)
      goto test_done;

  exp1 = EXP (fl1.l);
  exp2 = EXP (fl2.l);

  if (exp1 > exp2 + 25)
      goto test_done;
  if (exp2 > exp1 + 25) {
      fl1.f = fl2.f;
      goto test_done;
  }

  /* do everything in excess precision so's we can round later */
  mant1 = MANT (fl1.l) << 6;
  mant2 = MANT (fl2.l) << 6;

  if (SIGN (fl1.l))
    mant1 = -mant1;
  if (SIGN (fl2.l))
    mant2 = -mant2;

  if (exp1 > exp2)
    {
      mant2 >>= exp1 - exp2;
    }
  else
    {
      mant1 >>= exp2 - exp1;
      exp1 = exp2;
    }
  mant1 += mant2;

  if (mant1 < 0)
    {
      mant1 = -mant1;
      sign = SIGNBIT;
    }
  else if (!mant1) {
      fl1.f = 0;
      goto test_done;
  }

  /* normalize up */
  while (!(mant1 & 0xE0000000))
    {
      mant1 <<= 1;
      exp1--;
    }

  /* normalize down? */
  if (mant1 & (1 << 30))
    {
      mant1 >>= 1;
      exp1++;
    }

  /* round to even */
  mant1 += (mant1 & 0x40) ? 0x20 : 0x1F;

  /* normalize down? */
  if (mant1 & (1 << 30))
    {
      mant1 >>= 1;
      exp1++;
    }

  /* lose extra precision */
  mant1 >>= 6;

  /* turn off hidden bit */
  mant1 &= ~HIDDEN;

  /* pack up and go home */
  fl1.l = PACK (sign, exp1, mant1);
test_done:
  return (fl1.f);
}

/* subtract two floats */
float
__subsf3 (float a1, float a2)
{
  register union float_long fl1, fl2;

  fl1.f = a1;
  fl2.f = a2;

  /* check for zero args */
  if (!fl2.l)
    return (fl1.f);
  if (!fl1.l)
    return (-fl2.f);

  /* twiddle sign bit and add */
  fl2.l ^= SIGNBIT;
  return __addsf3 (a1, fl2.f);
}

/* compare two floats */
long
__cmpsf2 (float a1, float a2)
{
  register union float_long fl1, fl2;

  fl1.f = a1;
  fl2.f = a2;

  if (SIGN (fl1.l) && SIGN (fl2.l))
    {
      fl1.l ^= SIGNBIT;
      fl2.l ^= SIGNBIT;
    }
  if (fl1.l < fl2.l)
    return (-1);
  if (fl1.l > fl2.l)
    return (1);
  return (0);
}

/* multiply two floats */
float
__mulsf3 (float a1, float a2)
{
  register union float_long fl1, fl2;
  register unsigned long result;
  register int exp;
  int sign;

  fl1.f = a1;
  fl2.f = a2;

  if (!fl1.l || !fl2.l) {
      fl1.f = 0;
      goto test_done;
  }

  /* compute sign and exponent */
  sign = SIGN (fl1.l) ^ SIGN (fl2.l);
  exp = EXP (fl1.l) - EXCESS;
  exp += EXP (fl2.l);

  fl1.l = MANT (fl1.l);
  fl2.l = MANT (fl2.l);

  /* the multiply is done as one 16x16 multiply and two 16x8 multiples */
  result = (fl1.l >> 8) * (fl2.l >> 8);
  result += ((fl1.l & 0xFF) * (fl2.l >> 8)) >> 8;
  result += ((fl2.l & 0xFF) * (fl1.l >> 8)) >> 8;

  result >>= 2;
  if (result & 0x20000000)
    {
      /* round */
      result += 0x20;
      result >>= 6;
    }
  else
    {
      /* round */
      result += 0x10;
      result >>= 5;
      exp--;
    }
  if (result & (HIDDEN<<1)) {
    result >>= 1;
    exp++;
  }

  result &= ~HIDDEN;

  /* pack up and go home */
  fl1.l = PACK (sign, exp, result);
test_done:
  return (fl1.f);
}

/* divide two floats */
float
__divsf3 (float a1, float a2)
{
  register union float_long fl1, fl2;
  register int result;
  register int mask;
  register int exp, sign;

  fl1.f = a1;
  fl2.f = a2;

  /* subtract exponents */
  exp = EXP (fl1.l) - EXP (fl2.l) + EXCESS;

  /* compute sign */
  sign = SIGN (fl1.l) ^ SIGN (fl2.l);

  /* divide by zero??? */
  if (!fl2.l)
    /* return NaN or -NaN */
    return (sign ? 0xFFFFFFFF : 0x7FFFFFFF);

  /* numerator zero??? */
  if (!fl1.l)
    return (0);

  /* now get mantissas */
  fl1.l = MANT (fl1.l);
  fl2.l = MANT (fl2.l);

  /* this assures we have 25 bits of precision in the end */
  if (fl1.l < fl2.l)
    {
      fl1.l <<= 1;
      exp--;
    }

  /* now we perform repeated subtraction of fl2.l from fl1.l */
  mask = 0x1000000;
  result = 0;
  while (mask)
    {
      if (fl1.l >= fl2.l)
	{
	  result |= mask;
	  fl1.l -= fl2.l;
	}
      fl1.l <<= 1;
      mask >>= 1;
    }

  /* round */
  result += 1;

  /* normalize down */
  exp++;
  result >>= 1;

  result &= ~HIDDEN;

  /* pack up and go home */
  fl1.l = PACK (sign, exp, result);
  return (fl1.f);
}

/* convert int to double */
double
__floatsidf (register long a1)
{
  register int sign = 0, exp = 31 + EXCESSD;
  union double_long dl;

  if (!a1)
    {
      dl.l.upper = dl.l.lower = 0;
      return (dl.d);
    }

  if (a1 < 0)
    {
      sign = SIGNBIT;
      a1 = -a1;
    }

  while (a1 < 0x1000000)
    {
      a1 <<= 4;
      exp -= 4;
    }

  while (a1 < 0x40000000)
    {
      a1 <<= 1;
      exp--;
    }

  /* pack up and go home */
  dl.l.upper = sign;
  dl.l.upper |= exp << 20;
  dl.l.upper |= (a1 >> 10) & ~HIDDEND;
  dl.l.lower = a1 << 22;

  return (dl.d);
}

double
__floatdidf (register long long a1)
{
    register int exp = 63 + EXCESSD;
    union double_long dl;

    dl.l.upper = dl.l.lower = 0;
    if (a1 == 0)
	return (dl.d);

    if (a1 < 0) {
	dl.l.upper = SIGNBIT;
	a1 = -a1;
    }

    while (a1 < (long long)1<<54) {
	a1 <<= 8;
	exp -= 8;
    }
    while (a1 < (long long)1<<62) {
	a1 <<= 1;
	exp -= 1;
    }

  /* pack up and go home */
    dl.ll |= (a1 >> 10) & ~HIDDEND_LL;
    dl.l.upper |= exp << 20;

    return (dl.d);
}

float
__floatsisf (register long a1)
{
    (float)__floatsidf(a1);
}

float
__floatdisf (register long long a1)
{
    (float)__floatdidf(a1);
}

/* negate a float */
float
__negsf2 (float a1)
{
  register union float_long fl1;

  fl1.f = a1;
  if (!fl1.l)
    return (0);

  fl1.l ^= SIGNBIT;
  return (fl1.f);
}

/* negate a double */
double
__negdf2 (double a1)
{
  register union double_long dl1;

  dl1.d = a1;

  if (!dl1.l.upper && !dl1.l.lower)
      return (dl1.d);

  dl1.l.upper ^= SIGNBIT;
  return (dl1.d);
}

/* convert float to double */
double
__extendsfdf2 (float a1)
{
  register union float_long fl1;
  register union double_long dl;
  register int exp;

  fl1.f = a1;

  if (!fl1.l)
    {
      dl.l.upper = dl.l.lower = 0;
      return (dl.d);
    }

  dl.l.upper = SIGN (fl1.l);
  exp = EXP (fl1.l) - EXCESS + EXCESSD;
  dl.l.upper |= exp << 20;
  dl.l.upper |= (MANT (fl1.l) & ~HIDDEN) >> 3;
  dl.l.lower = MANT (fl1.l) << 29;

  return (dl.d);
}

/* convert double to float */
float
__truncdfsf2 (double a1)
{
  register int exp;
  register long mant;
  register union float_long fl;
  register union double_long dl1;

  dl1.d = a1;

  if (!dl1.l.upper && !dl1.l.lower)
    return (float)(0);

  exp = EXPD (dl1) - EXCESSD + EXCESS;

  /* shift double mantissa 6 bits so we can round */
  mant = MANTD (dl1) >> 6;

  /* now round and shift down */
  mant += 1;
  mant >>= 1;

  /* did the round overflow? */
  if (mant & 0xFE000000)
    {
      mant >>= 1;
      exp++;
    }

  mant &= ~HIDDEN;

  /* pack up and go home */
  fl.l = PACK (SIGND (dl1), exp, mant);
  return (fl.f);
}

/* compare two doubles */
long
__cmpdf2 (double a1, double a2)
{
  register union double_long dl1, dl2;

  dl1.d = a1;
  dl2.d = a2;

  if (SIGND (dl1) && SIGND (dl2))
    {
      dl1.l.upper ^= SIGNBIT;
      dl2.l.upper ^= SIGNBIT;
    }
  if (dl1.l.upper < dl2.l.upper)
    return (-1);
  if (dl1.l.upper > dl2.l.upper)
    return (1);
  if (dl1.l.lower < dl2.l.lower)
    return (-1);
  if (dl1.l.lower > dl2.l.lower)
    return (1);
  return (0);
}

/* convert double to int */
long
__fixdfsi (double a1)
{
  register union double_long dl1;
  register int exp;
  register long l;

  dl1.d = a1;

  if (!dl1.l.upper && !dl1.l.lower)
    return (0);

  exp = EXPD (dl1) - EXCESSD - 31;
  l = MANTD (dl1);

  if (exp > 0)
      return SIGND(dl1) ? (1<<31) : ((1ul<<31)-1);

  /* shift down until exp = 0 or l = 0 */
  if (exp < 0 && exp > -32 && l)
    l >>= -exp;
  else
    return (0);

  return (SIGND (dl1) ? -l : l);
}

/* convert double to int */
long long
__fixdfdi (double a1)
{
    register union double_long dl1;
    register int exp;
    register long long l;

    dl1.d = a1;

    if (!dl1.l.upper && !dl1.l.lower)
	return (0);

    exp = EXPD (dl1) - EXCESSD - 64;
    l = MANTD_LL(dl1);

    if (exp > 0) {
	l = (long long)1<<63;
	if (!SIGND(dl1))
	    l--;
	return l;
    }

    /* shift down until exp = 0 or l = 0 */
    if (exp < 0 && exp > -64 && l)
	l >>= -exp;
    else
	return (0);

    return (SIGND (dl1) ? -l : l);
}

/* convert double to unsigned int */
unsigned long
__fixunsdfsi (double a1)
{
  register union double_long dl1;
  register int exp;
  register unsigned long l;

  dl1.d = a1;

  if (!dl1.l.upper && !dl1.l.lower)
    return (0);

  exp = EXPD (dl1) - EXCESSD - 32;
  l = (((((dl1.l.upper) & 0xFFFFF) | HIDDEND) << 11) | (dl1.l.lower >> 21));

  if (exp > 0)
    return (0xFFFFFFFFul);	/* largest integer */

  /* shift down until exp = 0 or l = 0 */
  if (exp < 0 && exp > -32 && l)
    l >>= -exp;
  else
    return (0);

  return (l);
}

/* convert double to unsigned int */
unsigned long long
__fixunsdfdi (double a1)
{
    register union double_long dl1;
    register int exp;
    register unsigned long long l;

    dl1.d = a1;

    if (dl1.ll == 0)
	return (0);

    exp = EXPD (dl1) - EXCESSD - 64;

    l = dl1.ll;

    if (exp > 0)
	return (unsigned long long)-1;

    /* shift down until exp = 0 or l = 0 */
    if (exp < 0 && exp > -64 && l)
	l >>= -exp;
    else
	return (0);

    return (l);
}

/* addtwo doubles */
double
__adddf3 (double a1, double a2)
{
    register long long mant1, mant2;
    register union double_long fl1, fl2;
    register int exp1, exp2;
    int sign = 0;

    fl1.d = a1;
    fl2.d = a2;

    /* check for zero args */
    if (!fl2.ll)
	goto test_done;
    if (!fl1.ll) {
	fl1.d = fl2.d;
	goto test_done;
    }

    exp1 = EXPD(fl1);
    exp2 = EXPD(fl2);

    if (exp1 > exp2 + 54)
	goto test_done;
    if (exp2 > exp1 + 54) {
	fl1.d = fl2.d;
	goto test_done;
    }

    /* do everything in excess precision so's we can round later */
    mant1 = MANTD_LL(fl1) << 9;
    mant2 = MANTD_LL(fl2) << 9;

    if (SIGND(fl1))
	mant1 = -mant1;
    if (SIGND(fl2))
	mant2 = -mant2;

    if (exp1 > exp2)
	mant2 >>= exp1 - exp2;
    else {
	mant1 >>= exp2 - exp1;
	exp1 = exp2;
    }
    mant1 += mant2;

    if (mant1 < 0) {
	mant1 = -mant1;
	sign = SIGNBIT;
    } else if (!mant1) {
	fl1.d = 0;
	goto test_done;
    }

    /* normalize up */
    while (!(mant1 & ((long long)7<<61))) {
	mant1 <<= 1;
	exp1--;
    }

    /* normalize down? */
    if (mant1 & ((long long)3<<62)) {
	mant1 >>= 1;
	exp1++;
    }

    /* round to even */
    mant1 += (mant1 & (1<<9)) ? (1<<8) : ((1<<8)-1);

    /* normalize down? */
    if (mant1 & ((long long)3<<62)) {
	mant1 >>= 1;
	exp1++;
    }

    /* lose extra precision */
    mant1 >>= 9;

    /* turn off hidden bit */
    mant1 &= ~HIDDEND_LL;

    /* pack up and go home */
    fl1.ll = PACKD_LL(sign,exp1,mant1);

test_done:
    return (fl1.d);
}

/* subtract two doubles */
double
__subdf3 (double a1, double a2)
{
    register union double_long fl1, fl2;

    fl1.d = a1;
    fl2.d = a2;

    /* check for zero args */
    if (!fl2.ll)
	return (fl1.d);
    /* twiddle sign bit and add */
    fl2.l.upper ^= SIGNBIT;
    if (!fl1.ll)
	return (fl2.d);
    return __adddf3 (a1, fl2.d);
}

/* multiply two doubles */
double
__muldf3 (double a1, double a2)
{
    register union double_long fl1, fl2;
    register unsigned long long result;
    register int exp;
    int sign;

    fl1.d = a1;
    fl2.d = a2;

    if (!fl1.ll || !fl2.ll) {
	fl1.d = 0;
	goto test_done;
    }

    /* compute sign and exponent */
    sign = SIGND(fl1) ^ SIGND(fl2);
    exp = EXPD(fl1) - EXCESSD;
    exp += EXPD(fl2);

    fl1.ll = MANTD_LL(fl1);
    fl2.ll = MANTD_LL(fl2);

  /* the multiply is done as one 31x31 multiply and two 31x21 multiples */
    result = (fl1.ll >> 21) * (fl2.ll >> 21);
    result += ((fl1.ll & 0x1FFFFF) * (fl2.ll >> 21)) >> 21;
    result += ((fl2.ll & 0x1FFFFF) * (fl1.ll >> 21)) >> 21;

    result >>= 2;
    if (result & ((long long)1<<61)) {
	/* round */
	result += 1<<8;
	result >>= 9;
    } else {
	/* round */
	result += 1<<7;
	result >>= 8;
	exp--;
    }
    if (result & (HIDDEND_LL<<1)) {
	result >>= 1;
	exp++;
    }

    result &= ~HIDDEND_LL;

    /* pack up and go home */
    fl1.ll = PACKD_LL(sign,exp,result);
test_done:
    return (fl1.d);
}

/* divide two doubles */
double
__divdf3 (double a1, double a2)
{
    register union double_long fl1, fl2;
    register long long mask,result;
    register int exp, sign;

    fl1.d = a1;
    fl2.d = a2;

    /* subtract exponents */
    exp = EXPD(fl1) - EXPD(fl2) + EXCESSD;

    /* compute sign */
    sign = SIGND(fl1) ^ SIGND(fl2);

    /* numerator zero??? */
    if (fl1.ll == 0) {
	/* divide by zero??? */
	if (fl2.ll == 0)
	    fl1.ll = ((unsigned long long)1<<63)-1;	/* NaN */
	else
	    fl1.ll = 0;
	goto test_done;
    }

    /* return +Inf or -Inf */
    if (fl2.ll == 0) {
	fl1.ll = PACKD_LL(SIGND(fl1),2047,0);
	goto test_done;
    }


    /* now get mantissas */
    fl1.ll = MANTD_LL(fl1);
    fl2.ll = MANTD_LL(fl2);

    /* this assures we have 54 bits of precision in the end */
    if (fl1.ll < fl2.ll) {
	fl1.ll <<= 1;
	exp--;
    }

    /* now we perform repeated subtraction of fl2.ll from fl1.ll */
    mask = (long long)1<<53;
    result = 0;
    while (mask) {
	if (fl1.ll >= fl2.ll)
	{
	    result |= mask;
	    fl1.ll -= fl2.ll;
	}
	fl1.ll <<= 1;
	mask >>= 1;
    }

    /* round */
    result += 1;

    /* normalize down */
    exp++;
    result >>= 1;

    result &= ~HIDDEND_LL;

    /* pack up and go home */
    fl1.ll = PACKD_LL(sign, exp, result);

test_done:
    return (fl1.d);
}

int
__gtdf2 (double a1, double a2)
{
    return __cmpdf2 ((float) a1, (float) a2) > 0;
}

int
__gedf2 (double a1, double a2)
{
    return (__cmpdf2 ((float) a1, (float) a2) >= 0) - 1;
}

int
__ltdf2 (double a1, double a2)
{
    return - (__cmpdf2 ((float) a1, (float) a2) < 0);
}

int
__ledf2 (double a1, double a2)
{
    return __cmpdf2 ((float) a1, (float) a2) > 0;
}

int
__eqdf2 (double a1, double a2)
{
    return *(long long *) &a1 == *(long long *) &a2;
}

int
__nedf2 (double a1, double a2)
{
    return *(long long *) &a1 != *(long long *) &a2;
}
