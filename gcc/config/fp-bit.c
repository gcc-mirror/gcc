/* This is a software floating point library which can be used
   for targets without hardware floating point. 
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001
   Free Software Foundation, Inc.

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

/* This implements IEEE 754 format arithmetic, but does not provide a
   mechanism for setting the rounding mode, or for generating or handling
   exceptions.

   The original code by Steve Chamberlain, hacked by Mark Eichin and Jim
   Wilson, all of Cygnus Support.  */

/* The intended way to use this file is to make two copies, add `#define FLOAT'
   to one copy, then compile both copies and add them to libgcc.a.  */

#include "tconfig.h"
#include "fp-bit.h"

/* The following macros can be defined to change the behaviour of this file:
   FLOAT: Implement a `float', aka SFmode, fp library.  If this is not
     defined, then this file implements a `double', aka DFmode, fp library.
   FLOAT_ONLY: Used with FLOAT, to implement a `float' only library, i.e.
     don't include float->double conversion which requires the double library.
     This is useful only for machines which can't support doubles, e.g. some
     8-bit processors.
   CMPtype: Specify the type that floating point compares should return.
     This defaults to SItype, aka int.
   US_SOFTWARE_GOFAST: This makes all entry points use the same names as the
     US Software goFast library.
   _DEBUG_BITFLOAT: This makes debugging the code a little easier, by adding
     two integers to the FLO_union_type.
   NO_DENORMALS: Disable handling of denormals.
   NO_NANS: Disable nan and infinity handling
   SMALL_MACHINE: Useful when operations on QIs and HIs are faster
     than on an SI */

/* We don't currently support extended floats (long doubles) on machines
   without hardware to deal with them.

   These stubs are just to keep the linker from complaining about unresolved
   references which can be pulled in from libio & libstdc++, even if the
   user isn't using long doubles.  However, they may generate an unresolved
   external to abort if abort is not used by the function, and the stubs
   are referenced from within libc, since libgcc goes before and after the
   system library.  */

#ifdef EXTENDED_FLOAT_STUBS
__truncxfsf2 (){ abort(); }
__extendsfxf2 (){ abort(); }
__addxf3 (){ abort(); }
__divxf3 (){ abort(); }
__eqxf2 (){ abort(); }
__extenddfxf2 (){ abort(); }
__gtxf2 (){ abort(); }
__lexf2 (){ abort(); }
__ltxf2 (){ abort(); }
__mulxf3 (){ abort(); }
__negxf2 (){ abort(); }
__nexf2 (){ abort(); }
__subxf3 (){ abort(); }
__truncxfdf2 (){ abort(); }

__trunctfsf2 (){ abort(); }
__extendsftf2 (){ abort(); }
__addtf3 (){ abort(); }
__divtf3 (){ abort(); }
__eqtf2 (){ abort(); }
__extenddftf2 (){ abort(); }
__gttf2 (){ abort(); }
__letf2 (){ abort(); }
__lttf2 (){ abort(); }
__multf3 (){ abort(); }
__negtf2 (){ abort(); }
__netf2 (){ abort(); }
__subtf3 (){ abort(); }
__trunctfdf2 (){ abort(); }
__gexf2 (){ abort(); }
__fixxfsi (){ abort(); }
__floatsixf (){ abort(); }
#else	/* !EXTENDED_FLOAT_STUBS, rest of file */

/* IEEE "special" number predicates */

#ifdef NO_NANS

#define nan() 0
#define isnan(x) 0
#define isinf(x) 0
#else

#if   defined L_thenan_sf
const fp_number_type __thenan_sf = { CLASS_SNAN, 0, 0, {(fractype) 0} };
#elif defined L_thenan_df
const fp_number_type __thenan_df = { CLASS_SNAN, 0, 0, {(fractype) 0} };
#elif defined FLOAT
extern const fp_number_type __thenan_sf;
#else
extern const fp_number_type __thenan_df;
#endif

INLINE
static fp_number_type *
nan (void)
{
  /* Discard the const qualifier... */
#ifdef FLOAT  
  return (fp_number_type *) (& __thenan_sf);
#else
  return (fp_number_type *) (& __thenan_df);
#endif
}

INLINE
static int
isnan ( fp_number_type *  x)
{
  return x->class == CLASS_SNAN || x->class == CLASS_QNAN;
}

INLINE
static int
isinf ( fp_number_type *  x)
{
  return x->class == CLASS_INFINITY;
}

#endif /* NO_NANS */

INLINE
static int
iszero ( fp_number_type *  x)
{
  return x->class == CLASS_ZERO;
}

INLINE 
static void
flip_sign ( fp_number_type *  x)
{
  x->sign = !x->sign;
}

extern FLO_type pack_d ( fp_number_type * );

#if defined(L_pack_df) || defined(L_pack_sf)
FLO_type
pack_d ( fp_number_type *  src)
{
  FLO_union_type dst;
  fractype fraction = src->fraction.ll;	/* wasn't unsigned before? */
  int sign = src->sign;
  int exp = 0;

  if (isnan (src))
    {
      exp = EXPMAX;
      if (src->class == CLASS_QNAN || 1)
	{
	  fraction |= QUIET_NAN;
	}
    }
  else if (isinf (src))
    {
      exp = EXPMAX;
      fraction = 0;
    }
  else if (iszero (src))
    {
      exp = 0;
      fraction = 0;
    }
  else if (fraction == 0)
    {
      exp = 0;
    }
  else
    {
      if (src->normal_exp < NORMAL_EXPMIN)
	{
	  /* This number's exponent is too low to fit into the bits
	     available in the number, so we'll store 0 in the exponent and
	     shift the fraction to the right to make up for it.  */

	  int shift = NORMAL_EXPMIN - src->normal_exp;

	  exp = 0;

	  if (shift > FRAC_NBITS - NGARDS)
	    {
	      /* No point shifting, since it's more that 64 out.  */
	      fraction = 0;
	    }
	  else
	    {
	      int lowbit = (fraction & (((fractype)1 << shift) - 1)) ? 1 : 0;
	      fraction = (fraction >> shift) | lowbit;
	    }
	  if ((fraction & GARDMASK) == GARDMSB)
	    {
	      if ((fraction & (1 << NGARDS)))
		fraction += GARDROUND + 1;
	    }
	  else
	    {
	      /* Add to the guards to round up.  */
	      fraction += GARDROUND;
	    }
	  /* Perhaps the rounding means we now need to change the
             exponent, because the fraction is no longer denormal.  */
	  if (fraction >= IMPLICIT_1)
	    {
	      exp += 1;
	    }
	  fraction >>= NGARDS;
	}
      else if (src->normal_exp > EXPBIAS)
	{
	  exp = EXPMAX;
	  fraction = 0;
	}
      else
	{
	  exp = src->normal_exp + EXPBIAS;
	  /* IF the gard bits are the all zero, but the first, then we're
	     half way between two numbers, choose the one which makes the
	     lsb of the answer 0.  */
	  if ((fraction & GARDMASK) == GARDMSB)
	    {
	      if (fraction & (1 << NGARDS))
		fraction += GARDROUND + 1;
	    }
	  else
	    {
	      /* Add a one to the guards to round up */
	      fraction += GARDROUND;
	    }
	  if (fraction >= IMPLICIT_2)
	    {
	      fraction >>= 1;
	      exp += 1;
	    }
	  fraction >>= NGARDS;
	}
    }

  /* We previously used bitfields to store the number, but this doesn't
     handle little/big endian systems conveniently, so use shifts and
     masks */
#ifdef FLOAT_BIT_ORDER_MISMATCH
  dst.bits.fraction = fraction;
  dst.bits.exp = exp;
  dst.bits.sign = sign;
#else
  dst.value_raw = fraction & ((((fractype)1) << FRACBITS) - (fractype)1);
  dst.value_raw |= ((fractype) (exp & ((1 << EXPBITS) - 1))) << FRACBITS;
  dst.value_raw |= ((fractype) (sign & 1)) << (FRACBITS | EXPBITS);
#endif

#if defined(FLOAT_WORD_ORDER_MISMATCH) && !defined(FLOAT)
  {
    halffractype tmp = dst.words[0];
    dst.words[0] = dst.words[1];
    dst.words[1] = tmp;
  }
#endif

  return dst.value;
}
#endif

#if defined(L_unpack_df) || defined(L_unpack_sf)
void
unpack_d (FLO_union_type * src, fp_number_type * dst)
{
  /* We previously used bitfields to store the number, but this doesn't
     handle little/big endian systems conveniently, so use shifts and
     masks */
  fractype fraction;
  int exp;
  int sign;

#if defined(FLOAT_WORD_ORDER_MISMATCH) && !defined(FLOAT)
  FLO_union_type swapped;

  swapped.words[0] = src->words[1];
  swapped.words[1] = src->words[0];
  src = &swapped;
#endif
  
#ifdef FLOAT_BIT_ORDER_MISMATCH
  fraction = src->bits.fraction;
  exp = src->bits.exp;
  sign = src->bits.sign;
#else
  fraction = src->value_raw & ((((fractype)1) << FRACBITS) - (fractype)1);
  exp = ((int)(src->value_raw >> FRACBITS)) & ((1 << EXPBITS) - 1);
  sign = ((int)(src->value_raw >> (FRACBITS + EXPBITS))) & 1;
#endif

  dst->sign = sign;
  if (exp == 0)
    {
      /* Hmm.  Looks like 0 */
      if (fraction == 0
#ifdef NO_DENORMALS
	  || 1
#endif
	  )
	{
	  /* tastes like zero */
	  dst->class = CLASS_ZERO;
	}
      else
	{
	  /* Zero exponent with non zero fraction - it's denormalized,
	     so there isn't a leading implicit one - we'll shift it so
	     it gets one.  */
	  dst->normal_exp = exp - EXPBIAS + 1;
	  fraction <<= NGARDS;

	  dst->class = CLASS_NUMBER;
#if 1
	  while (fraction < IMPLICIT_1)
	    {
	      fraction <<= 1;
	      dst->normal_exp--;
	    }
#endif
	  dst->fraction.ll = fraction;
	}
    }
  else if (exp == EXPMAX)
    {
      /* Huge exponent*/
      if (fraction == 0)
	{
	  /* Attached to a zero fraction - means infinity */
	  dst->class = CLASS_INFINITY;
	}
      else
	{
	  /* Non zero fraction, means nan */
	  if (fraction & QUIET_NAN)
	    {
	      dst->class = CLASS_QNAN;
	    }
	  else
	    {
	      dst->class = CLASS_SNAN;
	    }
	  /* Keep the fraction part as the nan number */
	  dst->fraction.ll = fraction;
	}
    }
  else
    {
      /* Nothing strange about this number */
      dst->normal_exp = exp - EXPBIAS;
      dst->class = CLASS_NUMBER;
      dst->fraction.ll = (fraction << NGARDS) | IMPLICIT_1;
    }
}
#endif /* L_unpack_df || L_unpack_sf */

#if defined(L_addsub_sf) || defined(L_addsub_df)
static fp_number_type *
_fpadd_parts (fp_number_type * a,
	      fp_number_type * b,
	      fp_number_type * tmp)
{
  intfrac tfraction;

  /* Put commonly used fields in local variables.  */
  int a_normal_exp;
  int b_normal_exp;
  fractype a_fraction;
  fractype b_fraction;

  if (isnan (a))
    {
      return a;
    }
  if (isnan (b))
    {
      return b;
    }
  if (isinf (a))
    {
      /* Adding infinities with opposite signs yields a NaN.  */
      if (isinf (b) && a->sign != b->sign)
	return nan ();
      return a;
    }
  if (isinf (b))
    {
      return b;
    }
  if (iszero (b))
    {
      if (iszero (a))
	{
	  *tmp = *a;
	  tmp->sign = a->sign & b->sign;
	  return tmp;
	}
      return a;
    }
  if (iszero (a))
    {
      return b;
    }

  /* Got two numbers. shift the smaller and increment the exponent till
     they're the same */
  {
    int diff;

    a_normal_exp = a->normal_exp;
    b_normal_exp = b->normal_exp;
    a_fraction = a->fraction.ll;
    b_fraction = b->fraction.ll;

    diff = a_normal_exp - b_normal_exp;

    if (diff < 0)
      diff = -diff;
    if (diff < FRAC_NBITS)
      {
	/* ??? This does shifts one bit at a time.  Optimize.  */
	while (a_normal_exp > b_normal_exp)
	  {
	    b_normal_exp++;
	    LSHIFT (b_fraction);
	  }
	while (b_normal_exp > a_normal_exp)
	  {
	    a_normal_exp++;
	    LSHIFT (a_fraction);
	  }
      }
    else
      {
	/* Somethings's up.. choose the biggest */
	if (a_normal_exp > b_normal_exp)
	  {
	    b_normal_exp = a_normal_exp;
	    b_fraction = 0;
	  }
	else
	  {
	    a_normal_exp = b_normal_exp;
	    a_fraction = 0;
	  }
      }
  }

  if (a->sign != b->sign)
    {
      if (a->sign)
	{
	  tfraction = -a_fraction + b_fraction;
	}
      else
	{
	  tfraction = a_fraction - b_fraction;
	}
      if (tfraction >= 0)
	{
	  tmp->sign = 0;
	  tmp->normal_exp = a_normal_exp;
	  tmp->fraction.ll = tfraction;
	}
      else
	{
	  tmp->sign = 1;
	  tmp->normal_exp = a_normal_exp;
	  tmp->fraction.ll = -tfraction;
	}
      /* and renormalize it */

      while (tmp->fraction.ll < IMPLICIT_1 && tmp->fraction.ll)
	{
	  tmp->fraction.ll <<= 1;
	  tmp->normal_exp--;
	}
    }
  else
    {
      tmp->sign = a->sign;
      tmp->normal_exp = a_normal_exp;
      tmp->fraction.ll = a_fraction + b_fraction;
    }
  tmp->class = CLASS_NUMBER;
  /* Now the fraction is added, we have to shift down to renormalize the
     number */

  if (tmp->fraction.ll >= IMPLICIT_2)
    {
      LSHIFT (tmp->fraction.ll);
      tmp->normal_exp++;
    }
  return tmp;

}

FLO_type
add (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  res = _fpadd_parts (&a, &b, &tmp);

  return pack_d (res);
}

FLO_type
sub (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  b.sign ^= 1;

  res = _fpadd_parts (&a, &b, &tmp);

  return pack_d (res);
}
#endif /* L_addsub_sf || L_addsub_df */

#if defined(L_mul_sf) || defined(L_mul_df)
static INLINE fp_number_type *
_fpmul_parts ( fp_number_type *  a,
	       fp_number_type *  b,
	       fp_number_type * tmp)
{
  fractype low = 0;
  fractype high = 0;

  if (isnan (a))
    {
      a->sign = a->sign != b->sign;
      return a;
    }
  if (isnan (b))
    {
      b->sign = a->sign != b->sign;
      return b;
    }
  if (isinf (a))
    {
      if (iszero (b))
	return nan ();
      a->sign = a->sign != b->sign;
      return a;
    }
  if (isinf (b))
    {
      if (iszero (a))
	{
	  return nan ();
	}
      b->sign = a->sign != b->sign;
      return b;
    }
  if (iszero (a))
    {
      a->sign = a->sign != b->sign;
      return a;
    }
  if (iszero (b))
    {
      b->sign = a->sign != b->sign;
      return b;
    }

  /* Calculate the mantissa by multiplying both numbers to get a
     twice-as-wide number.  */
  {
#if defined(NO_DI_MODE)
    {
      fractype x = a->fraction.ll;
      fractype ylow = b->fraction.ll;
      fractype yhigh = 0;
      int bit;

      /* ??? This does multiplies one bit at a time.  Optimize.  */
      for (bit = 0; bit < FRAC_NBITS; bit++)
	{
	  int carry;

	  if (x & 1)
	    {
	      carry = (low += ylow) < ylow;
	      high += yhigh + carry;
	    }
	  yhigh <<= 1;
	  if (ylow & FRACHIGH)
	    {
	      yhigh |= 1;
	    }
	  ylow <<= 1;
	  x >>= 1;
	}
    }
#elif defined(FLOAT) 
    /* Multiplying two USIs to get a UDI, we're safe.  */
    {
      UDItype answer = (UDItype)a->fraction.ll * (UDItype)b->fraction.ll;
      
      high = answer >> BITS_PER_SI;
      low = answer;
    }
#else
    /* fractype is DImode, but we need the result to be twice as wide.
       Assuming a widening multiply from DImode to TImode is not
       available, build one by hand.  */
    {
      USItype nl = a->fraction.ll;
      USItype nh = a->fraction.ll >> BITS_PER_SI;
      USItype ml = b->fraction.ll;
      USItype mh = b->fraction.ll >> BITS_PER_SI;
      UDItype pp_ll = (UDItype) ml * nl;
      UDItype pp_hl = (UDItype) mh * nl;
      UDItype pp_lh = (UDItype) ml * nh;
      UDItype pp_hh = (UDItype) mh * nh;
      UDItype res2 = 0;
      UDItype res0 = 0;
      UDItype ps_hh__ = pp_hl + pp_lh;
      if (ps_hh__ < pp_hl)
	res2 += (UDItype)1 << BITS_PER_SI;
      pp_hl = (UDItype)(USItype)ps_hh__ << BITS_PER_SI;
      res0 = pp_ll + pp_hl;
      if (res0 < pp_ll)
	res2++;
      res2 += (ps_hh__ >> BITS_PER_SI) + pp_hh;
      high = res2;
      low = res0;
    }
#endif
  }

  tmp->normal_exp = a->normal_exp + b->normal_exp;
  tmp->sign = a->sign != b->sign;
#ifdef FLOAT
  tmp->normal_exp += 2;		/* ??????????????? */
#else
  tmp->normal_exp += 4;		/* ??????????????? */
#endif
  while (high >= IMPLICIT_2)
    {
      tmp->normal_exp++;
      if (high & 1)
	{
	  low >>= 1;
	  low |= FRACHIGH;
	}
      high >>= 1;
    }
  while (high < IMPLICIT_1)
    {
      tmp->normal_exp--;

      high <<= 1;
      if (low & FRACHIGH)
	high |= 1;
      low <<= 1;
    }
  /* rounding is tricky. if we only round if it won't make us round later. */
#if 0
  if (low & FRACHIGH2)
    {
      if (((high & GARDMASK) != GARDMSB)
	  && (((high + 1) & GARDMASK) == GARDMSB))
	{
	  /* don't round, it gets done again later. */
	}
      else
	{
	  high++;
	}
    }
#endif
  if ((high & GARDMASK) == GARDMSB)
    {
      if (high & (1 << NGARDS))
	{
	  /* half way, so round to even */
	  high += GARDROUND + 1;
	}
      else if (low)
	{
	  /* but we really weren't half way */
	  high += GARDROUND + 1;
	}
    }
  tmp->fraction.ll = high;
  tmp->class = CLASS_NUMBER;
  return tmp;
}

FLO_type
multiply (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  res = _fpmul_parts (&a, &b, &tmp);

  return pack_d (res);
}
#endif /* L_mul_sf || L_mul_df */

#if defined(L_div_sf) || defined(L_div_df)
static INLINE fp_number_type *
_fpdiv_parts (fp_number_type * a,
	      fp_number_type * b)
{
  fractype bit;
  fractype numerator;
  fractype denominator;
  fractype quotient;

  if (isnan (a))
    {
      return a;
    }
  if (isnan (b))
    {
      return b;
    }

  a->sign = a->sign ^ b->sign;

  if (isinf (a) || iszero (a))
    {
      if (a->class == b->class)
	return nan ();
      return a;
    }

  if (isinf (b))
    {
      a->fraction.ll = 0;
      a->normal_exp = 0;
      return a;
    }
  if (iszero (b))
    {
      a->class = CLASS_INFINITY;
      return a;
    }

  /* Calculate the mantissa by multiplying both 64bit numbers to get a
     128 bit number */
  {
    /* quotient =
       ( numerator / denominator) * 2^(numerator exponent -  denominator exponent)
     */

    a->normal_exp = a->normal_exp - b->normal_exp;
    numerator = a->fraction.ll;
    denominator = b->fraction.ll;

    if (numerator < denominator)
      {
	/* Fraction will be less than 1.0 */
	numerator *= 2;
	a->normal_exp--;
      }
    bit = IMPLICIT_1;
    quotient = 0;
    /* ??? Does divide one bit at a time.  Optimize.  */
    while (bit)
      {
	if (numerator >= denominator)
	  {
	    quotient |= bit;
	    numerator -= denominator;
	  }
	bit >>= 1;
	numerator *= 2;
      }

    if ((quotient & GARDMASK) == GARDMSB)
      {
	if (quotient & (1 << NGARDS))
	  {
	    /* half way, so round to even */
	    quotient += GARDROUND + 1;
	  }
	else if (numerator)
	  {
	    /* but we really weren't half way, more bits exist */
	    quotient += GARDROUND + 1;
	  }
      }

    a->fraction.ll = quotient;
    return (a);
  }
}

FLO_type
divide (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  res = _fpdiv_parts (&a, &b);

  return pack_d (res);
}
#endif /* L_div_sf || L_div_df */

#if defined(L_fpcmp_parts_sf) || defined(L_fpcmp_parts_df)
/* according to the demo, fpcmp returns a comparison with 0... thus
   a<b -> -1
   a==b -> 0
   a>b -> +1
 */

int
__fpcmp_parts (fp_number_type * a, fp_number_type * b)
{
#if 0
  /* either nan -> unordered. Must be checked outside of this routine. */
  if (isnan (a) && isnan (b))
    {
      return 1;			/* still unordered! */
    }
#endif

  if (isnan (a) || isnan (b))
    {
      return 1;			/* how to indicate unordered compare? */
    }
  if (isinf (a) && isinf (b))
    {
      /* +inf > -inf, but +inf != +inf */
      /* b    \a| +inf(0)| -inf(1)
       ______\+--------+--------
       +inf(0)| a==b(0)| a<b(-1)
       -------+--------+--------
       -inf(1)| a>b(1) | a==b(0)
       -------+--------+--------
       So since unordered must be non zero, just line up the columns...
       */
      return b->sign - a->sign;
    }
  /* but not both... */
  if (isinf (a))
    {
      return a->sign ? -1 : 1;
    }
  if (isinf (b))
    {
      return b->sign ? 1 : -1;
    }
  if (iszero (a) && iszero (b))
    {
      return 0;
    }
  if (iszero (a))
    {
      return b->sign ? 1 : -1;
    }
  if (iszero (b))
    {
      return a->sign ? -1 : 1;
    }
  /* now both are "normal". */
  if (a->sign != b->sign)
    {
      /* opposite signs */
      return a->sign ? -1 : 1;
    }
  /* same sign; exponents? */
  if (a->normal_exp > b->normal_exp)
    {
      return a->sign ? -1 : 1;
    }
  if (a->normal_exp < b->normal_exp)
    {
      return a->sign ? 1 : -1;
    }
  /* same exponents; check size. */
  if (a->fraction.ll > b->fraction.ll)
    {
      return a->sign ? -1 : 1;
    }
  if (a->fraction.ll < b->fraction.ll)
    {
      return a->sign ? 1 : -1;
    }
  /* after all that, they're equal. */
  return 0;
}
#endif

#if defined(L_compare_sf) || defined(L_compare_df)
CMPtype
compare (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  return __fpcmp_parts (&a, &b);
}
#endif /* L_compare_sf || L_compare_df */

#ifndef US_SOFTWARE_GOFAST

/* These should be optimized for their specific tasks someday.  */

#if defined(L_eq_sf) || defined(L_eq_df)
CMPtype
_eq_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;			/* false, truth == 0 */

  return __fpcmp_parts (&a, &b) ;
}
#endif /* L_eq_sf || L_eq_df */

#if defined(L_ne_sf) || defined(L_ne_df)
CMPtype
_ne_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;			/* true, truth != 0 */

  return  __fpcmp_parts (&a, &b) ;
}
#endif /* L_ne_sf || L_ne_df */

#if defined(L_gt_sf) || defined(L_gt_df)
CMPtype
_gt_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return -1;			/* false, truth > 0 */

  return __fpcmp_parts (&a, &b);
}
#endif /* L_gt_sf || L_gt_df */

#if defined(L_ge_sf) || defined(L_ge_df)
CMPtype
_ge_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return -1;			/* false, truth >= 0 */
  return __fpcmp_parts (&a, &b) ;
}
#endif /* L_ge_sf || L_ge_df */

#if defined(L_lt_sf) || defined(L_lt_df)
CMPtype
_lt_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;			/* false, truth < 0 */

  return __fpcmp_parts (&a, &b);
}
#endif /* L_lt_sf || L_lt_df */

#if defined(L_le_sf) || defined(L_le_df)
CMPtype
_le_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;			/* false, truth <= 0 */

  return __fpcmp_parts (&a, &b) ;
}
#endif /* L_le_sf || L_le_df */

#if defined(L_unord_sf) || defined(L_unord_df)
CMPtype
_unord_f2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  unpack_d (&au, &a);
  unpack_d (&bu, &b);

  return (isnan (&a) || isnan (&b));
}
#endif /* L_unord_sf || L_unord_df */

#endif /* ! US_SOFTWARE_GOFAST */

#if defined(L_si_to_sf) || defined(L_si_to_df)
FLO_type
si_to_float (SItype arg_a)
{
  fp_number_type in;

  in.class = CLASS_NUMBER;
  in.sign = arg_a < 0;
  if (!arg_a)
    {
      in.class = CLASS_ZERO;
    }
  else
    {
      in.normal_exp = FRACBITS + NGARDS;
      if (in.sign) 
	{
	  /* Special case for minint, since there is no +ve integer
	     representation for it */
	  if (arg_a == (- MAX_SI_INT - 1))
	    {
	      return (FLO_type)(- MAX_SI_INT - 1);
	    }
	  in.fraction.ll = (-arg_a);
	}
      else
	in.fraction.ll = arg_a;

      while (in.fraction.ll < (1LL << (FRACBITS + NGARDS)))
	{
	  in.fraction.ll <<= 1;
	  in.normal_exp -= 1;
	}
    }
  return pack_d (&in);
}
#endif /* L_si_to_sf || L_si_to_df */

#if defined(L_usi_to_sf) || defined(L_usi_to_df)
FLO_type
usi_to_float (USItype arg_a)
{
  fp_number_type in;

  in.sign = 0;
  if (!arg_a)
    {
      in.class = CLASS_ZERO;
    }
  else
    {
      in.class = CLASS_NUMBER;
      in.normal_exp = FRACBITS + NGARDS;
      in.fraction.ll = arg_a;

      while (in.fraction.ll > (1LL << (FRACBITS + NGARDS)))
        {
          in.fraction.ll >>= 1;
          in.normal_exp += 1;
        }
      while (in.fraction.ll < (1LL << (FRACBITS + NGARDS)))
	{
	  in.fraction.ll <<= 1;
	  in.normal_exp -= 1;
	}
    }
  return pack_d (&in);
}
#endif

#if defined(L_sf_to_si) || defined(L_df_to_si)
SItype
float_to_si (FLO_type arg_a)
{
  fp_number_type a;
  SItype tmp;
  FLO_union_type au;

  au.value = arg_a;
  unpack_d (&au, &a);

  if (iszero (&a))
    return 0;
  if (isnan (&a))
    return 0;
  /* get reasonable MAX_SI_INT... */
  if (isinf (&a))
    return a.sign ? (-MAX_SI_INT)-1 : MAX_SI_INT;
  /* it is a number, but a small one */
  if (a.normal_exp < 0)
    return 0;
  if (a.normal_exp > BITS_PER_SI - 2)
    return a.sign ? (-MAX_SI_INT)-1 : MAX_SI_INT;
  tmp = a.fraction.ll >> ((FRACBITS + NGARDS) - a.normal_exp);
  return a.sign ? (-tmp) : (tmp);
}
#endif /* L_sf_to_si || L_df_to_si */

#if defined(L_sf_to_usi) || defined(L_df_to_usi)
#ifdef US_SOFTWARE_GOFAST
/* While libgcc2.c defines its own __fixunssfsi and __fixunsdfsi routines,
   we also define them for GOFAST because the ones in libgcc2.c have the
   wrong names and I'd rather define these here and keep GOFAST CYG-LOC's
   out of libgcc2.c.  We can't define these here if not GOFAST because then
   there'd be duplicate copies.  */

USItype
float_to_usi (FLO_type arg_a)
{
  fp_number_type a;
  FLO_union_type au;

  au.value = arg_a;
  unpack_d (&au, &a);

  if (iszero (&a))
    return 0;
  if (isnan (&a))
    return 0;
  /* it is a negative number */
  if (a.sign)
    return 0;
  /* get reasonable MAX_USI_INT... */
  if (isinf (&a))
    return MAX_USI_INT;
  /* it is a number, but a small one */
  if (a.normal_exp < 0)
    return 0;
  if (a.normal_exp > BITS_PER_SI - 1)
    return MAX_USI_INT;
  else if (a.normal_exp > (FRACBITS + NGARDS))
    return a.fraction.ll << (a.normal_exp - (FRACBITS + NGARDS));
  else
    return a.fraction.ll >> ((FRACBITS + NGARDS) - a.normal_exp);
}
#endif /* US_SOFTWARE_GOFAST */
#endif /* L_sf_to_usi || L_df_to_usi */

#if defined(L_negate_sf) || defined(L_negate_df)
FLO_type
negate (FLO_type arg_a)
{
  fp_number_type a;
  FLO_union_type au;

  au.value = arg_a;
  unpack_d (&au, &a);

  flip_sign (&a);
  return pack_d (&a);
}
#endif /* L_negate_sf || L_negate_df */

#ifdef FLOAT

#if defined(L_make_sf)
SFtype
__make_fp(fp_class_type class,
	     unsigned int sign,
	     int exp, 
	     USItype frac)
{
  fp_number_type in;

  in.class = class;
  in.sign = sign;
  in.normal_exp = exp;
  in.fraction.ll = frac;
  return pack_d (&in);
}
#endif /* L_make_sf */

#ifndef FLOAT_ONLY

/* This enables one to build an fp library that supports float but not double.
   Otherwise, we would get an undefined reference to __make_dp.
   This is needed for some 8-bit ports that can't handle well values that
   are 8-bytes in size, so we just don't support double for them at all.  */

#if defined(L_sf_to_df)
DFtype
sf_to_df (SFtype arg_a)
{
  fp_number_type in;
  FLO_union_type au;

  au.value = arg_a;
  unpack_d (&au, &in);

  return __make_dp (in.class, in.sign, in.normal_exp,
		    ((UDItype) in.fraction.ll) << F_D_BITOFF);
}
#endif /* L_sf_to_df */

#endif /* ! FLOAT_ONLY */
#endif /* FLOAT */

#ifndef FLOAT

extern SFtype __make_fp (fp_class_type, unsigned int, int, USItype);

#if defined(L_make_df)
DFtype
__make_dp (fp_class_type class, unsigned int sign, int exp, UDItype frac)
{
  fp_number_type in;

  in.class = class;
  in.sign = sign;
  in.normal_exp = exp;
  in.fraction.ll = frac;
  return pack_d (&in);
}
#endif /* L_make_df */

#if defined(L_df_to_sf)
SFtype
df_to_sf (DFtype arg_a)
{
  fp_number_type in;
  USItype sffrac;
  FLO_union_type au;

  au.value = arg_a;
  unpack_d (&au, &in);

  sffrac = in.fraction.ll >> F_D_BITOFF;

  /* We set the lowest guard bit in SFFRAC if we discarded any non
     zero bits.  */
  if ((in.fraction.ll & (((USItype) 1 << F_D_BITOFF) - 1)) != 0)
    sffrac |= 1;

  return __make_fp (in.class, in.sign, in.normal_exp, sffrac);
}
#endif /* L_df_to_sf */

#endif /* ! FLOAT */
#endif /* !EXTENDED_FLOAT_STUBS */
