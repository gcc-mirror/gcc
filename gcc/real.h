/* Definitions of floating-point access for GNU compiler.
   Copyright (C) 1989, 1991, 1994, 1996, 1997, 1998,
   1999, 2000, 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_REAL_H
#define GCC_REAL_H

/* Define codes for all the float formats that we know of.  */
#define UNKNOWN_FLOAT_FORMAT 0
#define IEEE_FLOAT_FORMAT 1
#define VAX_FLOAT_FORMAT 2
#define IBM_FLOAT_FORMAT 3
#define C4X_FLOAT_FORMAT 4

/* Default to IEEE float if not specified.  Nearly all machines use it.  */

#ifndef TARGET_FLOAT_FORMAT
#define	TARGET_FLOAT_FORMAT	IEEE_FLOAT_FORMAT
#endif

#ifndef HOST_FLOAT_FORMAT
#define	HOST_FLOAT_FORMAT	IEEE_FLOAT_FORMAT
#endif

#ifndef INTEL_EXTENDED_IEEE_FORMAT
#define INTEL_EXTENDED_IEEE_FORMAT 0
#endif

/* If FLOAT_WORDS_BIG_ENDIAN and HOST_FLOAT_WORDS_BIG_ENDIAN are not defined
   in the header files, then this implies the word-endianness is the same as
   for integers.  */

/* This is defined 0 or 1, like WORDS_BIG_ENDIAN.  */
#ifndef FLOAT_WORDS_BIG_ENDIAN
#define FLOAT_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN
#endif

/* This is defined 0 or 1, unlike HOST_WORDS_BIG_ENDIAN.  */
#ifndef HOST_FLOAT_WORDS_BIG_ENDIAN
#ifdef HOST_WORDS_BIG_ENDIAN
#define HOST_FLOAT_WORDS_BIG_ENDIAN 1
#else
#define HOST_FLOAT_WORDS_BIG_ENDIAN 0
#endif
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#endif
/* MAX_LONG_DOUBLE_TYPE_SIZE is a constant tested by #if.
   LONG_DOUBLE_TYPE_SIZE can vary at compiler run time.
   So long as macros like REAL_VALUE_TO_TARGET_LONG_DOUBLE cannot
   vary too, however, then XFmode and TFmode long double
   cannot both be supported at the same time.  */
#ifndef MAX_LONG_DOUBLE_TYPE_SIZE
#define MAX_LONG_DOUBLE_TYPE_SIZE LONG_DOUBLE_TYPE_SIZE
#endif

/* **** Start of software floating point emulator interface macros **** */

/* REAL_VALUE_TYPE is an array of the minimum number of HOST_WIDE_INTs
   required to hold either a 96- or 160-bit extended precision floating
   point type.  This is true even if the maximum precision floating
   point type on the target is smaller.  */
#if MAX_LONG_DOUBLE_TYPE_SIZE == 128 && !INTEL_EXTENDED_IEEE_FORMAT
#define REAL_VALUE_TYPE_SIZE 160
#else
#define REAL_VALUE_TYPE_SIZE 96
#endif
#define REAL_WIDTH \
  (REAL_VALUE_TYPE_SIZE/HOST_BITS_PER_WIDE_INT \
   + (REAL_VALUE_TYPE_SIZE%HOST_BITS_PER_WIDE_INT ? 1 : 0)) /* round up */
typedef struct {
  HOST_WIDE_INT r[REAL_WIDTH];
} realvaluetype;
/* Various headers condition prototypes on #ifdef REAL_VALUE_TYPE, so it needs
   to be a macro.  */
#define REAL_VALUE_TYPE realvaluetype

/* Calculate the format for CONST_DOUBLE.  We need as many slots as
   are necessary to overlay a REAL_VALUE_TYPE on them.  This could be
   as many as five (32-bit HOST_WIDE_INT, 160-bit REAL_VALUE_TYPE).

   A number of places assume that there are always at least two 'w'
   slots in a CONST_DOUBLE, so we provide them even if one would suffice.  */

#if REAL_WIDTH == 1
# define CONST_DOUBLE_FORMAT	 "ww"
#else
# if REAL_WIDTH == 2
#  define CONST_DOUBLE_FORMAT	 "ww"
# else
#  if REAL_WIDTH == 3
#   define CONST_DOUBLE_FORMAT	 "www"
#  else
#   if REAL_WIDTH == 4
#    define CONST_DOUBLE_FORMAT	 "wwww"
#   else
#    if REAL_WIDTH == 5
#     define CONST_DOUBLE_FORMAT "wwwww"
#    else
      #error "REAL_WIDTH > 5 not supported"
#    endif
#   endif
#  endif
# endif
#endif

extern unsigned int significand_size	PARAMS ((enum machine_mode));

#define REAL_ARITHMETIC(value, code, d1, d2) \
  earith (&(value), (code), &(d1), &(d2))

/* Declare functions in real.c.  */
extern void earith		PARAMS ((REAL_VALUE_TYPE *, int,
				       REAL_VALUE_TYPE *, REAL_VALUE_TYPE *));
extern REAL_VALUE_TYPE etrunci	PARAMS ((REAL_VALUE_TYPE));
extern REAL_VALUE_TYPE etruncui	PARAMS ((REAL_VALUE_TYPE));
extern REAL_VALUE_TYPE ereal_negate PARAMS ((REAL_VALUE_TYPE));
extern HOST_WIDE_INT efixi	PARAMS ((REAL_VALUE_TYPE));
extern unsigned HOST_WIDE_INT efixui PARAMS ((REAL_VALUE_TYPE));
extern void ereal_from_int	PARAMS ((REAL_VALUE_TYPE *,
				       HOST_WIDE_INT, HOST_WIDE_INT,
				       enum machine_mode));
extern void ereal_from_uint	PARAMS ((REAL_VALUE_TYPE *,
				       unsigned HOST_WIDE_INT,
				       unsigned HOST_WIDE_INT,
				       enum machine_mode));
extern void ereal_to_int	PARAMS ((HOST_WIDE_INT *, HOST_WIDE_INT *,
				       REAL_VALUE_TYPE));
extern REAL_VALUE_TYPE ereal_ldexp PARAMS ((REAL_VALUE_TYPE, int));

extern void etartdouble		PARAMS ((REAL_VALUE_TYPE, long *));
extern void etarldouble		PARAMS ((REAL_VALUE_TYPE, long *));
extern void etardouble		PARAMS ((REAL_VALUE_TYPE, long *));
extern long etarsingle		PARAMS ((REAL_VALUE_TYPE));
extern void ereal_to_decimal	PARAMS ((REAL_VALUE_TYPE, char *));
extern int ereal_cmp		PARAMS ((REAL_VALUE_TYPE, REAL_VALUE_TYPE));
extern int ereal_isneg		PARAMS ((REAL_VALUE_TYPE));
extern REAL_VALUE_TYPE ereal_unto_float PARAMS ((long));
extern REAL_VALUE_TYPE ereal_unto_double PARAMS ((long *));
extern REAL_VALUE_TYPE ereal_from_float PARAMS ((HOST_WIDE_INT));
extern REAL_VALUE_TYPE ereal_from_double PARAMS ((HOST_WIDE_INT *));

#define REAL_VALUES_EQUAL(x, y) (ereal_cmp ((x), (y)) == 0)
/* true if x < y : */
#define REAL_VALUES_LESS(x, y) (ereal_cmp ((x), (y)) == -1)
#define REAL_VALUE_LDEXP(x, n) ereal_ldexp (x, n)

/* Compare two floating-point objects for bitwise identity.
   This is not the same as comparing for equality on IEEE hosts:
   -0.0 equals 0.0 but they are not identical, and conversely
   two NaNs might be identical but they cannot be equal.  */
#define REAL_VALUES_IDENTICAL(x, y) \
  (!memcmp ((char *) &(x), (char *) &(y), sizeof (REAL_VALUE_TYPE)))

/* These return REAL_VALUE_TYPE: */
#define REAL_VALUE_RNDZINT(x) (etrunci (x))
#define REAL_VALUE_UNSIGNED_RNDZINT(x) (etruncui (x))

/* Truncate the floating-point value X to mode MODE.  */
#define REAL_VALUE_TRUNCATE(mode, x)  real_value_truncate (mode, x)
extern REAL_VALUE_TYPE real_value_truncate PARAMS ((enum machine_mode,
                                                 REAL_VALUE_TYPE));

/* Expansion of REAL_VALUE_TRUNCATE.
   The result is in floating point, rounded to nearest or even.  */
extern bool exact_real_truncate PARAMS ((enum machine_mode,
					 REAL_VALUE_TYPE *));

/* These return HOST_WIDE_INT: */
/* Convert a floating-point value to integer, rounding toward zero.  */
#define REAL_VALUE_FIX(x) (efixi (x))
/* Convert a floating-point value to unsigned integer, rounding
   toward zero.  */
#define REAL_VALUE_UNSIGNED_FIX(x) (efixui (x))

/* Convert ASCII string S to floating point in mode M.
   Decimal input uses ATOF.  Hexadecimal uses HTOF.  */
#define REAL_VALUE_ATOF(s,m) ereal_atof(s,m)
#define REAL_VALUE_HTOF(s,m) ereal_atof(s,m)

#define REAL_VALUE_NEGATE ereal_negate

/* Compute the absolute value of a floating-point value X.  */
#define REAL_VALUE_ABS(x) \
   (REAL_VALUE_NEGATIVE (x) ? REAL_VALUE_NEGATE (x) : (x))

/* Determine whether a floating-point value X is infinite.  */
#define REAL_VALUE_ISINF(x) (target_isinf (x))

/* Determine whether a floating-point value X is a NaN.  */
#define REAL_VALUE_ISNAN(x) (target_isnan (x))

/* Determine whether a floating-point value X is negative.  */
#define REAL_VALUE_NEGATIVE(x) (target_negative (x))

/* Determine whether a floating-point value X is minus zero.  */
#define REAL_VALUE_MINUS_ZERO(x) \
 ((ereal_cmp (x, dconst0) == 0) && (ereal_isneg (x) != 0 ))

#define REAL_VALUE_TO_INT ereal_to_int

/* Here the cast to HOST_WIDE_INT sign-extends arguments such as ~0.  */
#define REAL_VALUE_FROM_INT(d, lo, hi, mode) \
  ereal_from_int (&d, (HOST_WIDE_INT) (lo), (HOST_WIDE_INT) (hi), mode)

#define REAL_VALUE_FROM_UNSIGNED_INT(d, lo, hi, mode) \
  ereal_from_uint (&d, lo, hi, mode)

/* IN is a REAL_VALUE_TYPE.  OUT is an array of longs.  */
#define REAL_VALUE_TO_TARGET_LONG_DOUBLE(IN, OUT) 		\
   (LONG_DOUBLE_TYPE_SIZE == 64 ? etardouble ((IN), (OUT))	\
    : LONG_DOUBLE_TYPE_SIZE == 96 ? etarldouble ((IN), (OUT))	\
    : LONG_DOUBLE_TYPE_SIZE == 128 ? etartdouble ((IN), (OUT))  \
    : abort ())
#define REAL_VALUE_TO_TARGET_DOUBLE(IN, OUT) (etardouble ((IN), (OUT)))

/* IN is a REAL_VALUE_TYPE.  OUT is a long.  */
#define REAL_VALUE_TO_TARGET_SINGLE(IN, OUT) ((OUT) = etarsingle ((IN)))

/* Inverse of REAL_VALUE_TO_TARGET_DOUBLE.  */
#define REAL_VALUE_UNTO_TARGET_DOUBLE(d)  (ereal_unto_double (d))

/* Inverse of REAL_VALUE_TO_TARGET_SINGLE.  */
#define REAL_VALUE_UNTO_TARGET_SINGLE(f)  (ereal_unto_float (f))

/* d is an array of HOST_WIDE_INT that holds a double precision
   value in the target computer's floating point format.  */
#define REAL_VALUE_FROM_TARGET_DOUBLE(d)  (ereal_from_double (d))

/* f is a HOST_WIDE_INT containing a single precision target float value.  */
#define REAL_VALUE_FROM_TARGET_SINGLE(f)  (ereal_from_float (f))

/* Conversions to decimal ASCII string.  */
#define REAL_VALUE_TO_DECIMAL(r, fmt, s) (ereal_to_decimal (r, s))

/* **** End of software floating point emulator interface macros **** */

/* Constant real values 0, 1, 2, and -1.  */

extern REAL_VALUE_TYPE dconst0;
extern REAL_VALUE_TYPE dconst1;
extern REAL_VALUE_TYPE dconst2;
extern REAL_VALUE_TYPE dconstm1;

/* Given a CONST_DOUBLE in FROM, store into TO the value it represents.  */
/* Function to return a real value (not a tree node)
   from a given integer constant.  */
union tree_node;
REAL_VALUE_TYPE real_value_from_int_cst	PARAMS ((union tree_node *,
						union tree_node *));

#define REAL_VALUE_FROM_CONST_DOUBLE(to, from)		\
  memcpy (&(to), &CONST_DOUBLE_LOW ((from)), sizeof (REAL_VALUE_TYPE))

/* Return a CONST_DOUBLE with value R and mode M.  */

#define CONST_DOUBLE_FROM_REAL_VALUE(r, m) \
  const_double_from_real_value (r, m)
extern rtx const_double_from_real_value PARAMS ((REAL_VALUE_TYPE,
						 enum machine_mode));

/* Shorthand; can be handy in machine descriptions.  */
#define CONST_DOUBLE_ATOF(s, m) \
  CONST_DOUBLE_FROM_REAL_VALUE (REAL_VALUE_ATOF (s, m), m)

/* Replace R by 1/R in the given machine mode, if the result is exact.  */
extern int exact_real_inverse	PARAMS ((enum machine_mode, REAL_VALUE_TYPE *));
extern int target_isnan		PARAMS ((REAL_VALUE_TYPE));
extern int target_isinf		PARAMS ((REAL_VALUE_TYPE));
extern int target_negative	PARAMS ((REAL_VALUE_TYPE));
extern void debug_real		PARAMS ((REAL_VALUE_TYPE));
extern REAL_VALUE_TYPE ereal_atof PARAMS ((const char *, enum machine_mode));

#endif /* ! GCC_REAL_H */
