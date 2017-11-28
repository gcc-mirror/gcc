/* Define builtin-in macros for the C family front ends.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-common.h"
#include "memmodel.h"
#include "tm_p.h"		/* For TARGET_CPU_CPP_BUILTINS & friends.  */
#include "stringpool.h"
#include "stor-layout.h"
#include "flags.h"
#include "c-pragma.h"
#include "output.h"		/* For user_label_prefix.  */
#include "debug.h"		/* For dwarf2out_do_cfi_asm.  */
#include "common/common-target.h"
#include "cpp-id-data.h"
#include "cppbuiltin.h"

#ifndef TARGET_OS_CPP_BUILTINS
# define TARGET_OS_CPP_BUILTINS()
#endif

#ifndef TARGET_OBJFMT_CPP_BUILTINS
# define TARGET_OBJFMT_CPP_BUILTINS()
#endif

#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX ""
#endif

/* Non-static as some targets don't use it.  */
static void builtin_define_with_hex_fp_value (const char *, tree,
					      int, const char *,
					      const char *,
					      const char *);
static void builtin_define_stdint_macros (void);
static void builtin_define_constants (const char *, tree);
static void builtin_define_type_max (const char *, tree);
static void builtin_define_type_minmax (const char *, const char *, tree);
static void builtin_define_type_width (const char *, tree, tree);
static void builtin_define_float_constants (const char *,
					    const char *,
					    const char *,
					    const char *,
					    tree);

/* Return true if MODE provides a fast multiply/add (FMA) builtin function.
   Originally this function used the fma optab, but that doesn't work with
   -save-temps, so just rely on the HAVE_fma macros for the standard floating
   point types.  */

static bool
mode_has_fma (machine_mode mode)
{
  switch (mode)
    {
#ifdef HAVE_fmasf4
    case E_SFmode:
      return !!HAVE_fmasf4;
#endif

#ifdef HAVE_fmadf4
    case E_DFmode:
      return !!HAVE_fmadf4;
#endif

#ifdef HAVE_fmakf4	/* PowerPC if long double != __float128.  */
    case E_KFmode:
      return !!HAVE_fmakf4;
#endif

#ifdef HAVE_fmaxf4
    case E_XFmode:
      return !!HAVE_fmaxf4;
#endif

#ifdef HAVE_fmatf4
    case E_TFmode:
      return !!HAVE_fmatf4;
#endif

    default:
      break;
    }

  return false;
}

/* Define NAME with value TYPE size_unit.  */
void
builtin_define_type_sizeof (const char *name, tree type)
{
  builtin_define_with_int_value (name,
				 tree_to_uhwi (TYPE_SIZE_UNIT (type)));
}

/* Define the float.h constants for TYPE using NAME_PREFIX, FP_SUFFIX,
   and FP_CAST. */
static void
builtin_define_float_constants (const char *name_prefix,
		                const char *fp_suffix,
				const char *fp_cast,
				const char *fma_suffix,
				tree type)
{
  /* Used to convert radix-based values to base 10 values in several cases.

     In the max_exp -> max_10_exp conversion for 128-bit IEEE, we need at
     least 6 significant digits for correct results.  Using the fraction
     formed by (log(2)*1e6)/(log(10)*1e6) overflows a 32-bit integer as an
     intermediate; perhaps someone can find a better approximation, in the
     mean time, I suspect using doubles won't harm the bootstrap here.  */

  const double log10_2 = .30102999566398119521;
  double log10_b;
  const struct real_format *fmt;
  const struct real_format *widefmt;

  char name[64], buf[128];
  int dig, min_10_exp, max_10_exp;
  int decimal_dig;
  int type_decimal_dig;

  fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
  gcc_assert (fmt->b != 10);
  widefmt = REAL_MODE_FORMAT (TYPE_MODE (long_double_type_node));
  gcc_assert (widefmt->b != 10);
  for (int i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    {
      tree wtype = FLOATN_NX_TYPE_NODE (i);
      if (wtype != NULL_TREE)
	{
	  const struct real_format *wfmt
	    = REAL_MODE_FORMAT (TYPE_MODE (wtype));
	  gcc_assert (wfmt->b != 10);
	  if (wfmt->p > widefmt->p)
	    widefmt = wfmt;
	}
    }

  /* The radix of the exponent representation.  */
  if (type == float_type_node)
    builtin_define_with_int_value ("__FLT_RADIX__", fmt->b);
  log10_b = log10_2;

  /* The number of radix digits, p, in the floating-point significand.  */
  sprintf (name, "__%s_MANT_DIG__", name_prefix);
  builtin_define_with_int_value (name, fmt->p);

  /* The number of decimal digits, q, such that any floating-point number
     with q decimal digits can be rounded into a floating-point number with
     p radix b digits and back again without change to the q decimal digits,

	p log10 b			if b is a power of 10
	floor((p - 1) log10 b)		otherwise
  */
  dig = (fmt->p - 1) * log10_b;
  sprintf (name, "__%s_DIG__", name_prefix);
  builtin_define_with_int_value (name, dig);

  /* The minimum negative int x such that b**(x-1) is a normalized float.  */
  sprintf (name, "__%s_MIN_EXP__", name_prefix);
  sprintf (buf, "(%d)", fmt->emin);
  builtin_define_with_value (name, buf, 0);

  /* The minimum negative int x such that 10**x is a normalized float,

	  ceil (log10 (b ** (emin - 1)))
	= ceil (log10 (b) * (emin - 1))

     Recall that emin is negative, so the integer truncation calculates
     the ceiling, not the floor, in this case.  */
  min_10_exp = (fmt->emin - 1) * log10_b;
  sprintf (name, "__%s_MIN_10_EXP__", name_prefix);
  sprintf (buf, "(%d)", min_10_exp);
  builtin_define_with_value (name, buf, 0);

  /* The maximum int x such that b**(x-1) is a representable float.  */
  sprintf (name, "__%s_MAX_EXP__", name_prefix);
  builtin_define_with_int_value (name, fmt->emax);

  /* The maximum int x such that 10**x is in the range of representable
     finite floating-point numbers,

	  floor (log10((1 - b**-p) * b**emax))
	= floor (log10(1 - b**-p) + log10(b**emax))
	= floor (log10(1 - b**-p) + log10(b)*emax)

     The safest thing to do here is to just compute this number.  But since
     we don't link cc1 with libm, we cannot.  We could implement log10 here
     a series expansion, but that seems too much effort because:

     Note that the first term, for all extant p, is a number exceedingly close
     to zero, but slightly negative.  Note that the second term is an integer
     scaling an irrational number, and that because of the floor we are only
     interested in its integral portion.

     In order for the first term to have any effect on the integral portion
     of the second term, the second term has to be exceedingly close to an
     integer itself (e.g. 123.000000000001 or something).  Getting a result
     that close to an integer requires that the irrational multiplicand have
     a long series of zeros in its expansion, which doesn't occur in the
     first 20 digits or so of log10(b).

     Hand-waving aside, crunching all of the sets of constants above by hand
     does not yield a case for which the first term is significant, which
     in the end is all that matters.  */
  max_10_exp = fmt->emax * log10_b;
  sprintf (name, "__%s_MAX_10_EXP__", name_prefix);
  builtin_define_with_int_value (name, max_10_exp);

  /* The number of decimal digits, n, such that any floating-point number
     can be rounded to n decimal digits and back again without change to
     the value.

	p * log10(b)			if b is a power of 10
	ceil(1 + p * log10(b))		otherwise

     The only macro we care about is this number for the widest supported
     floating type, but we want this value for rendering constants below.  */
  {
    double d_decimal_dig
      = 1 + (fmt->p < widefmt->p ? widefmt->p : fmt->p) * log10_b;
    decimal_dig = d_decimal_dig;
    if (decimal_dig < d_decimal_dig)
      decimal_dig++;
  }
  /* Similar, for this type rather than long double.  */
  {
    double type_d_decimal_dig = 1 + fmt->p * log10_b;
    type_decimal_dig = type_d_decimal_dig;
    if (type_decimal_dig < type_d_decimal_dig)
      type_decimal_dig++;
  }
  /* Define __DECIMAL_DIG__ to the value for long double to be
     compatible with C99 and C11; see DR#501 and N2108.  */
  if (type == long_double_type_node)
    builtin_define_with_int_value ("__DECIMAL_DIG__", type_decimal_dig);
  sprintf (name, "__%s_DECIMAL_DIG__", name_prefix);
  builtin_define_with_int_value (name, type_decimal_dig);

  /* Since, for the supported formats, B is always a power of 2, we
     construct the following numbers directly as a hexadecimal
     constants.  */
  get_max_float (fmt, buf, sizeof (buf));

  sprintf (name, "__%s_MAX__", name_prefix);
  builtin_define_with_hex_fp_value (name, type, decimal_dig, buf, fp_suffix, fp_cast);

  /* The minimum normalized positive floating-point number,
     b**(emin-1).  */
  sprintf (name, "__%s_MIN__", name_prefix);
  sprintf (buf, "0x1p%d", fmt->emin - 1);
  builtin_define_with_hex_fp_value (name, type, decimal_dig, buf, fp_suffix, fp_cast);

  /* The difference between 1 and the least value greater than 1 that is
     representable in the given floating point type, b**(1-p).  */
  sprintf (name, "__%s_EPSILON__", name_prefix);
  if (fmt->pnan < fmt->p)
    /* This is an IBM extended double format, so 1.0 + any double is
       representable precisely.  */
      sprintf (buf, "0x1p%d", fmt->emin - fmt->p);
    else
      sprintf (buf, "0x1p%d", 1 - fmt->p);
  builtin_define_with_hex_fp_value (name, type, decimal_dig, buf, fp_suffix, fp_cast);

  /* For C++ std::numeric_limits<T>::denorm_min and C11 *_TRUE_MIN.
     The minimum denormalized positive floating-point number, b**(emin-p).
     The minimum normalized positive floating-point number for formats
     that don't support denormals.  */
  sprintf (name, "__%s_DENORM_MIN__", name_prefix);
  sprintf (buf, "0x1p%d", fmt->emin - (fmt->has_denorm ? fmt->p : 1));
  builtin_define_with_hex_fp_value (name, type, decimal_dig,
				    buf, fp_suffix, fp_cast);

  sprintf (name, "__%s_HAS_DENORM__", name_prefix);
  builtin_define_with_value (name, fmt->has_denorm ? "1" : "0", 0);

  /* For C++ std::numeric_limits<T>::has_infinity.  */
  sprintf (name, "__%s_HAS_INFINITY__", name_prefix);
  builtin_define_with_int_value (name,
				 MODE_HAS_INFINITIES (TYPE_MODE (type)));
  /* For C++ std::numeric_limits<T>::has_quiet_NaN.  We do not have a
     predicate to distinguish a target that has both quiet and
     signalling NaNs from a target that has only quiet NaNs or only
     signalling NaNs, so we assume that a target that has any kind of
     NaN has quiet NaNs.  */
  sprintf (name, "__%s_HAS_QUIET_NAN__", name_prefix);
  builtin_define_with_int_value (name, MODE_HAS_NANS (TYPE_MODE (type)));

  /* Note whether we have fast FMA.  */
  if (mode_has_fma (TYPE_MODE (type)) && fma_suffix != NULL)
    {
      sprintf (name, "__FP_FAST_FMA%s", fma_suffix);
      builtin_define_with_int_value (name, 1);
    }
}

/* Define __DECx__ constants for TYPE using NAME_PREFIX and SUFFIX. */
static void
builtin_define_decimal_float_constants (const char *name_prefix,
					const char *suffix,
					tree type)
{
  const struct real_format *fmt;
  char name[64], buf[128], *p;
  int digits;

  fmt = REAL_MODE_FORMAT (TYPE_MODE (type));

  /* The number of radix digits, p, in the significand.  */
  sprintf (name, "__%s_MANT_DIG__", name_prefix);
  builtin_define_with_int_value (name, fmt->p);

  /* The minimum negative int x such that b**(x-1) is a normalized float.  */
  sprintf (name, "__%s_MIN_EXP__", name_prefix);
  sprintf (buf, "(%d)", fmt->emin);
  builtin_define_with_value (name, buf, 0);

  /* The maximum int x such that b**(x-1) is a representable float.  */
  sprintf (name, "__%s_MAX_EXP__", name_prefix);
  builtin_define_with_int_value (name, fmt->emax);

  /* Compute the minimum representable value.  */
  sprintf (name, "__%s_MIN__", name_prefix);
  sprintf (buf, "1E%d%s", fmt->emin - 1, suffix);
  builtin_define_with_value (name, buf, 0);

  /* Compute the maximum representable value.  */
  sprintf (name, "__%s_MAX__", name_prefix);
  p = buf;
  for (digits = fmt->p; digits; digits--)
    {
      *p++ = '9';
      if (digits == fmt->p)
	*p++ = '.';
    }
  *p = 0;
  /* fmt->p plus 1, to account for the decimal point and fmt->emax
     minus 1 because the digits are nines, not 1.0.  */
  sprintf (&buf[fmt->p + 1], "E%d%s", fmt->emax - 1, suffix);
  builtin_define_with_value (name, buf, 0);

  /* Compute epsilon (the difference between 1 and least value greater
     than 1 representable).  */
  sprintf (name, "__%s_EPSILON__", name_prefix);
  sprintf (buf, "1E-%d%s", fmt->p - 1, suffix);
  builtin_define_with_value (name, buf, 0);

  /* Minimum subnormal positive decimal value.  */
  sprintf (name, "__%s_SUBNORMAL_MIN__", name_prefix);
  p = buf;
  for (digits = fmt->p; digits > 1; digits--)
    {
      *p++ = '0';
      if (digits == fmt->p)
	*p++ = '.';
    }
  *p = 0;
  sprintf (&buf[fmt->p], "1E%d%s", fmt->emin - 1, suffix);
  builtin_define_with_value (name, buf, 0);
}

/* Define fixed-point constants for TYPE using NAME_PREFIX and SUFFIX.  */

static void
builtin_define_fixed_point_constants (const char *name_prefix,
				      const char *suffix,
				      tree type)
{
  char name[64], buf[256], *new_buf;
  int i, mod;

  sprintf (name, "__%s_FBIT__", name_prefix);
  builtin_define_with_int_value (name, TYPE_FBIT (type));

  sprintf (name, "__%s_IBIT__", name_prefix);
  builtin_define_with_int_value (name, TYPE_IBIT (type));

  /* If there is no suffix, defines are for fixed-point modes.
     We just return.  */
  if (strcmp (suffix, "") == 0)
    return;

  if (TYPE_UNSIGNED (type))
    {
      sprintf (name, "__%s_MIN__", name_prefix);
      sprintf (buf, "0.0%s", suffix);
      builtin_define_with_value (name, buf, 0);
    }
  else
    {
      sprintf (name, "__%s_MIN__", name_prefix);
      if (ALL_ACCUM_MODE_P (TYPE_MODE (type)))
	sprintf (buf, "(-0X1P%d%s-0X1P%d%s)", TYPE_IBIT (type) - 1, suffix,
		 TYPE_IBIT (type) - 1, suffix);
      else
	sprintf (buf, "(-0.5%s-0.5%s)", suffix, suffix);
      builtin_define_with_value (name, buf, 0);
    }

  sprintf (name, "__%s_MAX__", name_prefix);
  sprintf (buf, "0X");
  new_buf = buf + 2;
  mod = (TYPE_FBIT (type) + TYPE_IBIT (type)) % 4;
  if (mod)
    sprintf (new_buf++, "%x", (1 << mod) - 1);
  for (i = 0; i < (TYPE_FBIT (type) + TYPE_IBIT (type)) / 4; i++)
    sprintf (new_buf++, "F");
  sprintf (new_buf, "P-%d%s", TYPE_FBIT (type), suffix);
  builtin_define_with_value (name, buf, 0);

  sprintf (name, "__%s_EPSILON__", name_prefix);
  sprintf (buf, "0x1P-%d%s", TYPE_FBIT (type), suffix);
  builtin_define_with_value (name, buf, 0);
}

/* Define macros used by <stdint.h>.  */
static void
builtin_define_stdint_macros (void)
{
  builtin_define_type_max ("__INTMAX_MAX__", intmax_type_node);
  builtin_define_constants ("__INTMAX_C", intmax_type_node);
  builtin_define_type_max ("__UINTMAX_MAX__", uintmax_type_node);
  builtin_define_constants ("__UINTMAX_C", uintmax_type_node);
  builtin_define_type_width ("__INTMAX_WIDTH__", intmax_type_node,
			     uintmax_type_node);
  if (sig_atomic_type_node)
    {
      builtin_define_type_minmax ("__SIG_ATOMIC_MIN__", "__SIG_ATOMIC_MAX__",
				  sig_atomic_type_node);
      builtin_define_type_width ("__SIG_ATOMIC_WIDTH__", sig_atomic_type_node,
				 NULL_TREE);
    }
  if (int8_type_node)
    builtin_define_type_max ("__INT8_MAX__", int8_type_node);
  if (int16_type_node)
    builtin_define_type_max ("__INT16_MAX__", int16_type_node);
  if (int32_type_node)
    builtin_define_type_max ("__INT32_MAX__", int32_type_node);
  if (int64_type_node)
    builtin_define_type_max ("__INT64_MAX__", int64_type_node);
  if (uint8_type_node)
    builtin_define_type_max ("__UINT8_MAX__", uint8_type_node);
  if (c_uint16_type_node)
    builtin_define_type_max ("__UINT16_MAX__", c_uint16_type_node);
  if (c_uint32_type_node)
    builtin_define_type_max ("__UINT32_MAX__", c_uint32_type_node);
  if (c_uint64_type_node)
    builtin_define_type_max ("__UINT64_MAX__", c_uint64_type_node);
  if (int_least8_type_node)
    {
      builtin_define_type_max ("__INT_LEAST8_MAX__", int_least8_type_node);
      builtin_define_constants ("__INT8_C", int_least8_type_node);
      builtin_define_type_width ("__INT_LEAST8_WIDTH__", int_least8_type_node,
				 uint_least8_type_node);
    }
  if (int_least16_type_node)
    {
      builtin_define_type_max ("__INT_LEAST16_MAX__", int_least16_type_node);
      builtin_define_constants ("__INT16_C", int_least16_type_node);
      builtin_define_type_width ("__INT_LEAST16_WIDTH__",
				 int_least16_type_node,
				 uint_least16_type_node);
    }
  if (int_least32_type_node)
    {
      builtin_define_type_max ("__INT_LEAST32_MAX__", int_least32_type_node);
      builtin_define_constants ("__INT32_C", int_least32_type_node);
      builtin_define_type_width ("__INT_LEAST32_WIDTH__",
				 int_least32_type_node,
				 uint_least32_type_node);
    }
  if (int_least64_type_node)
    {
      builtin_define_type_max ("__INT_LEAST64_MAX__", int_least64_type_node);
      builtin_define_constants ("__INT64_C", int_least64_type_node);
      builtin_define_type_width ("__INT_LEAST64_WIDTH__",
				 int_least64_type_node,
				 uint_least64_type_node);
    }
  if (uint_least8_type_node)
    {
      builtin_define_type_max ("__UINT_LEAST8_MAX__", uint_least8_type_node);
      builtin_define_constants ("__UINT8_C", uint_least8_type_node);
    }
  if (uint_least16_type_node)
    {
      builtin_define_type_max ("__UINT_LEAST16_MAX__", uint_least16_type_node);
      builtin_define_constants ("__UINT16_C", uint_least16_type_node);
    }
  if (uint_least32_type_node)
    {
      builtin_define_type_max ("__UINT_LEAST32_MAX__", uint_least32_type_node);
      builtin_define_constants ("__UINT32_C", uint_least32_type_node);
    }
  if (uint_least64_type_node)
    {
      builtin_define_type_max ("__UINT_LEAST64_MAX__", uint_least64_type_node);
      builtin_define_constants ("__UINT64_C", uint_least64_type_node);
    }
  if (int_fast8_type_node)
    {
      builtin_define_type_max ("__INT_FAST8_MAX__", int_fast8_type_node);
      builtin_define_type_width ("__INT_FAST8_WIDTH__", int_fast8_type_node,
				 uint_fast8_type_node);
    }
  if (int_fast16_type_node)
    {
      builtin_define_type_max ("__INT_FAST16_MAX__", int_fast16_type_node);
      builtin_define_type_width ("__INT_FAST16_WIDTH__", int_fast16_type_node,
				 uint_fast16_type_node);
    }
  if (int_fast32_type_node)
    {
      builtin_define_type_max ("__INT_FAST32_MAX__", int_fast32_type_node);
      builtin_define_type_width ("__INT_FAST32_WIDTH__", int_fast32_type_node,
				 uint_fast32_type_node);
    }
  if (int_fast64_type_node)
    {
      builtin_define_type_max ("__INT_FAST64_MAX__", int_fast64_type_node);
      builtin_define_type_width ("__INT_FAST64_WIDTH__", int_fast64_type_node,
				 uint_fast64_type_node);
    }
  if (uint_fast8_type_node)
    builtin_define_type_max ("__UINT_FAST8_MAX__", uint_fast8_type_node);
  if (uint_fast16_type_node)
    builtin_define_type_max ("__UINT_FAST16_MAX__", uint_fast16_type_node);
  if (uint_fast32_type_node)
    builtin_define_type_max ("__UINT_FAST32_MAX__", uint_fast32_type_node);
  if (uint_fast64_type_node)
    builtin_define_type_max ("__UINT_FAST64_MAX__", uint_fast64_type_node);
  if (intptr_type_node)
    {
      builtin_define_type_max ("__INTPTR_MAX__", intptr_type_node);
      builtin_define_type_width ("__INTPTR_WIDTH__", intptr_type_node,
				 uintptr_type_node);
    }
  if (uintptr_type_node)
    builtin_define_type_max ("__UINTPTR_MAX__", uintptr_type_node);
}

/* Adjust the optimization macros when a #pragma GCC optimization is done to
   reflect the current level.  */
void
c_cpp_builtins_optimize_pragma (cpp_reader *pfile, tree prev_tree,
				tree cur_tree)
{
  struct cl_optimization *prev = TREE_OPTIMIZATION (prev_tree);
  struct cl_optimization *cur  = TREE_OPTIMIZATION (cur_tree);
  bool prev_fast_math;
  bool cur_fast_math;

  /* -undef turns off target-specific built-ins.  */
  if (flag_undef)
    return;

  /* Other target-independent built-ins determined by command-line
     options.  */
  if (!prev->x_optimize_size && cur->x_optimize_size)
    cpp_define (pfile, "__OPTIMIZE_SIZE__");
  else if (prev->x_optimize_size && !cur->x_optimize_size)
    cpp_undef (pfile, "__OPTIMIZE_SIZE__");

  if (!prev->x_optimize && cur->x_optimize)
    cpp_define (pfile, "__OPTIMIZE__");
  else if (prev->x_optimize && !cur->x_optimize)
    cpp_undef (pfile, "__OPTIMIZE__");

  prev_fast_math = fast_math_flags_struct_set_p (prev);
  cur_fast_math  = fast_math_flags_struct_set_p (cur);
  if (!prev_fast_math && cur_fast_math)
    cpp_define (pfile, "__FAST_MATH__");
  else if (prev_fast_math && !cur_fast_math)
    cpp_undef (pfile, "__FAST_MATH__");

  if (!prev->x_flag_signaling_nans && cur->x_flag_signaling_nans)
    cpp_define (pfile, "__SUPPORT_SNAN__");
  else if (prev->x_flag_signaling_nans && !cur->x_flag_signaling_nans)
    cpp_undef (pfile, "__SUPPORT_SNAN__");

  if (!prev->x_flag_errno_math && cur->x_flag_errno_math)
    cpp_undef (pfile, "__NO_MATH_ERRNO__");
  else if (prev->x_flag_errno_math && !cur->x_flag_errno_math)
    cpp_define (pfile, "__NO_MATH_ERRNO__");

  if (!prev->x_flag_finite_math_only && cur->x_flag_finite_math_only)
    {
      cpp_undef (pfile, "__FINITE_MATH_ONLY__");
      cpp_define (pfile, "__FINITE_MATH_ONLY__=1");
    }
  else if (prev->x_flag_finite_math_only && !cur->x_flag_finite_math_only)
    {
      cpp_undef (pfile, "__FINITE_MATH_ONLY__");
      cpp_define (pfile, "__FINITE_MATH_ONLY__=0");
    }
}


/* This function will emit cpp macros to indicate the presence of various lock
   free atomic operations.  */
   
static void
cpp_atomic_builtins (cpp_reader *pfile)
{
  /* Set a flag for each size of object that compare and swap exists for up to
     a 16 byte object.  */
#define SWAP_LIMIT  17
  bool have_swap[SWAP_LIMIT];
  unsigned int psize;

  /* Clear the map of sizes compare_and swap exists for.  */
  memset (have_swap, 0, sizeof (have_swap));

  /* Tell source code if the compiler makes sync_compare_and_swap
     builtins available.  */
#ifndef HAVE_sync_compare_and_swapqi
#define HAVE_sync_compare_and_swapqi 0
#endif
#ifndef HAVE_atomic_compare_and_swapqi
#define HAVE_atomic_compare_and_swapqi 0
#endif

  if (HAVE_sync_compare_and_swapqi || HAVE_atomic_compare_and_swapqi)
    {
      cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1");
      have_swap[1] = true;
    }

#ifndef HAVE_sync_compare_and_swaphi
#define HAVE_sync_compare_and_swaphi 0
#endif
#ifndef HAVE_atomic_compare_and_swaphi
#define HAVE_atomic_compare_and_swaphi 0
#endif
  if (HAVE_sync_compare_and_swaphi || HAVE_atomic_compare_and_swaphi)
    {
      cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2");
      have_swap[2] = true;
    }

#ifndef HAVE_sync_compare_and_swapsi
#define HAVE_sync_compare_and_swapsi 0
#endif
#ifndef HAVE_atomic_compare_and_swapsi
#define HAVE_atomic_compare_and_swapsi 0
#endif
  if (HAVE_sync_compare_and_swapsi || HAVE_atomic_compare_and_swapsi)
    {
      cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4");
      have_swap[4] = true;
    }

#ifndef HAVE_sync_compare_and_swapdi
#define HAVE_sync_compare_and_swapdi 0
#endif
#ifndef HAVE_atomic_compare_and_swapdi
#define HAVE_atomic_compare_and_swapdi 0
#endif
  if (HAVE_sync_compare_and_swapdi || HAVE_atomic_compare_and_swapdi)
    {
      cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8");
      have_swap[8] = true;
    }

#ifndef HAVE_sync_compare_and_swapti
#define HAVE_sync_compare_and_swapti 0
#endif
#ifndef HAVE_atomic_compare_and_swapti
#define HAVE_atomic_compare_and_swapti 0
#endif
  if (HAVE_sync_compare_and_swapti || HAVE_atomic_compare_and_swapti)
    {
      cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16");
      have_swap[16] = true;
    }

  /* Tell the source code about various types.  These map to the C++11 and C11
     macros where 2 indicates lock-free always, and 1 indicates sometimes
     lock free.  */
#define SIZEOF_NODE(T) (tree_to_uhwi (TYPE_SIZE_UNIT (T)))
#define SWAP_INDEX(T) ((SIZEOF_NODE (T) < SWAP_LIMIT) ? SIZEOF_NODE (T) : 0)
  builtin_define_with_int_value ("__GCC_ATOMIC_BOOL_LOCK_FREE", 
			(have_swap[SWAP_INDEX (boolean_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_CHAR_LOCK_FREE", 
			(have_swap[SWAP_INDEX (signed_char_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_CHAR16_T_LOCK_FREE", 
			(have_swap[SWAP_INDEX (char16_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_CHAR32_T_LOCK_FREE", 
			(have_swap[SWAP_INDEX (char32_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_WCHAR_T_LOCK_FREE", 
			(have_swap[SWAP_INDEX (wchar_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_SHORT_LOCK_FREE", 
		      (have_swap[SWAP_INDEX (short_integer_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_INT_LOCK_FREE", 
			(have_swap[SWAP_INDEX (integer_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_LONG_LOCK_FREE", 
		      (have_swap[SWAP_INDEX (long_integer_type_node)]? 2 : 1));
  builtin_define_with_int_value ("__GCC_ATOMIC_LLONG_LOCK_FREE", 
		(have_swap[SWAP_INDEX (long_long_integer_type_node)]? 2 : 1));

  /* If we're dealing with a "set" value that doesn't exactly correspond
     to a boolean truth value, let the library work around that.  */
  builtin_define_with_int_value ("__GCC_ATOMIC_TEST_AND_SET_TRUEVAL",
				 targetm.atomic_test_and_set_trueval);

  /* ptr_type_node can't be used here since ptr_mode is only set when
     toplev calls backend_init which is not done with -E  or pch.  */
  psize = POINTER_SIZE_UNITS;
  if (psize >= SWAP_LIMIT)
    psize = 0;
  builtin_define_with_int_value ("__GCC_ATOMIC_POINTER_LOCK_FREE", 
			(have_swap[psize]? 2 : 1));
}

/* Return TRUE if the implicit excess precision in which the back-end will
   compute floating-point calculations is not more than the explicit
   excess precision that the front-end will apply under
   -fexcess-precision=[standard|fast].

   More intuitively, return TRUE if the excess precision proposed by the
   front-end is the excess precision that will actually be used.  */

static bool
c_cpp_flt_eval_method_iec_559 (void)
{
  enum excess_precision_type front_end_ept
    = (flag_excess_precision_cmdline == EXCESS_PRECISION_STANDARD
       ? EXCESS_PRECISION_TYPE_STANDARD
       : EXCESS_PRECISION_TYPE_FAST);

  enum flt_eval_method back_end
    = targetm.c.excess_precision (EXCESS_PRECISION_TYPE_IMPLICIT);

  enum flt_eval_method front_end
    = targetm.c.excess_precision (front_end_ept);

  return excess_precision_mode_join (front_end, back_end) == front_end;
}

/* Return the value for __GCC_IEC_559.  */
static int
cpp_iec_559_value (void)
{
  /* The default is support for IEEE 754-2008.  */
  int ret = 2;

  /* float and double must be binary32 and binary64.  If they are but
     with reversed NaN convention, at most IEEE 754-1985 is
     supported.  */
  const struct real_format *ffmt
    = REAL_MODE_FORMAT (TYPE_MODE (float_type_node));
  const struct real_format *dfmt
    = REAL_MODE_FORMAT (TYPE_MODE (double_type_node));
  if (!ffmt->qnan_msb_set || !dfmt->qnan_msb_set)
    ret = 1;
  if (ffmt->b != 2
      || ffmt->p != 24
      || ffmt->pnan != 24
      || ffmt->emin != -125
      || ffmt->emax != 128
      || ffmt->signbit_rw != 31
      || ffmt->round_towards_zero
      || !ffmt->has_sign_dependent_rounding
      || !ffmt->has_nans
      || !ffmt->has_inf
      || !ffmt->has_denorm
      || !ffmt->has_signed_zero
      || dfmt->b != 2
      || dfmt->p != 53
      || dfmt->pnan != 53
      || dfmt->emin != -1021
      || dfmt->emax != 1024
      || dfmt->signbit_rw != 63
      || dfmt->round_towards_zero
      || !dfmt->has_sign_dependent_rounding
      || !dfmt->has_nans
      || !dfmt->has_inf
      || !dfmt->has_denorm
      || !dfmt->has_signed_zero)
    ret = 0;

  /* In strict C standards conformance mode, consider a back-end providing
     more implicit excess precision than the explicit excess precision
     the front-end options would require to mean a lack of IEEE 754
     support.  For C++, and outside strict conformance mode, do not consider
     this to mean a lack of IEEE 754 support.  */

  if (flag_iso
      && !c_dialect_cxx ()
      && !c_cpp_flt_eval_method_iec_559 ())
    ret = 0;

  if (flag_iso
      && !c_dialect_cxx ()
      && flag_fp_contract_mode == FP_CONTRACT_FAST)
    ret = 0;

  /* Various options are contrary to IEEE 754 semantics.  */
  if (flag_unsafe_math_optimizations
      || flag_associative_math
      || flag_reciprocal_math
      || flag_finite_math_only
      || !flag_signed_zeros
      || flag_single_precision_constant)
    ret = 0;

  /* If the target does not support IEEE 754 exceptions and rounding
     modes, consider IEEE 754 support to be absent.  */
  if (!targetm.float_exceptions_rounding_supported_p ())
    ret = 0;

  return ret;
}

/* Return the value for __GCC_IEC_559_COMPLEX.  */
static int
cpp_iec_559_complex_value (void)
{
  /* The value is no bigger than that of __GCC_IEC_559.  */
  int ret = cpp_iec_559_value ();

  /* Some options are contrary to the required default state of the
     CX_LIMITED_RANGE pragma.  */
  if (flag_complex_method != 2)
    ret = 0;

  return ret;
}

/* Hook that registers front end and target-specific built-ins.  */
void
c_cpp_builtins (cpp_reader *pfile)
{
  int i;

  /* -undef turns off target-specific built-ins.  */
  if (flag_undef)
    return;

  define_language_independent_builtin_macros (pfile);

  if (c_dialect_cxx ())
  {
    int major;
    parse_basever (&major, NULL, NULL);
    cpp_define_formatted (pfile, "__GNUG__=%d", major);
  }

  /* For stddef.h.  They require macros defined in c-common.c.  */
  c_stddef_cpp_builtins ();

  /* Set include test macros for all C/C++ (not for just C++11 etc.)
     The builtins __has_include__ and __has_include_next__ are defined
     in libcpp.  */
  cpp_define (pfile, "__has_include(STR)=__has_include__(STR)");
  cpp_define (pfile, "__has_include_next(STR)=__has_include_next__(STR)");

  if (c_dialect_cxx ())
    {
      if (flag_weak && SUPPORTS_ONE_ONLY)
	cpp_define (pfile, "__GXX_WEAK__=1");
      else
	cpp_define (pfile, "__GXX_WEAK__=0");

      if (warn_deprecated)
	cpp_define (pfile, "__DEPRECATED");

      if (flag_rtti)
	{
	  cpp_define (pfile, "__GXX_RTTI");
	  cpp_define (pfile, "__cpp_rtti=199711");
	}

      if (cxx_dialect >= cxx11)
        cpp_define (pfile, "__GXX_EXPERIMENTAL_CXX0X__");

      /* Binary literals have been allowed in g++ before C++11
	 and were standardized for C++14.  */
      if (!pedantic || cxx_dialect > cxx11)
	cpp_define (pfile, "__cpp_binary_literals=201304");

      /* Similarly for hexadecimal floating point literals and C++17.  */
      if (!pedantic || cpp_get_options (parse_in)->extended_numbers)
	cpp_define (pfile, "__cpp_hex_float=201603");

      /* Arrays of runtime bound were removed from C++14, but we still
	 support GNU VLAs.  Let's define this macro to a low number
	 (corresponding to the initial test release of GNU C++) if we won't
	 complain about use of VLAs.  */
      if (c_dialect_cxx ()
	  && (pedantic ? warn_vla == 0 : warn_vla <= 0))
	cpp_define (pfile, "__cpp_runtime_arrays=198712");

      if (cxx_dialect >= cxx11)
	{
	  /* Set feature test macros for C++11.  */
	  if (cxx_dialect <= cxx14)
	    cpp_define (pfile, "__cpp_unicode_characters=200704");
	  cpp_define (pfile, "__cpp_raw_strings=200710");
	  cpp_define (pfile, "__cpp_unicode_literals=200710");
	  cpp_define (pfile, "__cpp_user_defined_literals=200809");
	  cpp_define (pfile, "__cpp_lambdas=200907");
	  if (cxx_dialect == cxx11)
	    cpp_define (pfile, "__cpp_constexpr=200704");
	  if (cxx_dialect <= cxx14)
	    cpp_define (pfile, "__cpp_range_based_for=200907");
	  if (cxx_dialect <= cxx14)
	    cpp_define (pfile, "__cpp_static_assert=200410");
	  cpp_define (pfile, "__cpp_decltype=200707");
	  cpp_define (pfile, "__cpp_attributes=200809");
	  cpp_define (pfile, "__cpp_rvalue_reference=200610");
	  cpp_define (pfile, "__cpp_rvalue_references=200610");
	  cpp_define (pfile, "__cpp_variadic_templates=200704");
	  cpp_define (pfile, "__cpp_initializer_lists=200806");
	  cpp_define (pfile, "__cpp_delegating_constructors=200604");
	  cpp_define (pfile, "__cpp_nsdmi=200809");
	  if (!flag_new_inheriting_ctors)
	    cpp_define (pfile, "__cpp_inheriting_constructors=200802");
	  else
	    cpp_define (pfile, "__cpp_inheriting_constructors=201511");
	  cpp_define (pfile, "__cpp_ref_qualifiers=200710");
	  cpp_define (pfile, "__cpp_alias_templates=200704");
	}
      if (cxx_dialect > cxx11)
	{
	  /* Set feature test macros for C++14.  */
	  cpp_define (pfile, "__cpp_return_type_deduction=201304");
	  cpp_define (pfile, "__cpp_init_captures=201304");
	  cpp_define (pfile, "__cpp_generic_lambdas=201304");
	  if (cxx_dialect <= cxx14)
	    cpp_define (pfile, "__cpp_constexpr=201304");
	  cpp_define (pfile, "__cpp_decltype_auto=201304");
	  cpp_define (pfile, "__cpp_aggregate_nsdmi=201304");
	  cpp_define (pfile, "__cpp_variable_templates=201304");
	  cpp_define (pfile, "__cpp_digit_separators=201309");
	}
      if (cxx_dialect > cxx14)
	{
	  /* Set feature test macros for C++1z.  */
	  cpp_define (pfile, "__cpp_unicode_characters=201411");
	  cpp_define (pfile, "__cpp_static_assert=201411");
	  cpp_define (pfile, "__cpp_namespace_attributes=201411");
	  cpp_define (pfile, "__cpp_enumerator_attributes=201411");
	  cpp_define (pfile, "__cpp_nested_namespace_definitions=201411");
	  cpp_define (pfile, "__cpp_fold_expressions=201603");
	  cpp_define (pfile, "__cpp_nontype_template_args=201411");
	  cpp_define (pfile, "__cpp_range_based_for=201603");
	  cpp_define (pfile, "__cpp_constexpr=201603");
	  cpp_define (pfile, "__cpp_if_constexpr=201606");
	  cpp_define (pfile, "__cpp_capture_star_this=201603");
	  cpp_define (pfile, "__cpp_inline_variables=201606");
	  cpp_define (pfile, "__cpp_aggregate_bases=201603");
	  cpp_define (pfile, "__cpp_deduction_guides=201606");
	  cpp_define (pfile, "__cpp_noexcept_function_type=201510");
	  cpp_define (pfile, "__cpp_template_auto=201606");
	  cpp_define (pfile, "__cpp_structured_bindings=201606");
	  cpp_define (pfile, "__cpp_variadic_using=201611");
	}
      if (flag_concepts)
	cpp_define (pfile, "__cpp_concepts=201507");
      if (flag_tm)
	/* Use a value smaller than the 201505 specified in
	   the TS, since we don't yet support atomic_cancel.  */
	cpp_define (pfile, "__cpp_transactional_memory=210500");
      if (flag_sized_deallocation)
	cpp_define (pfile, "__cpp_sized_deallocation=201309");
      if (aligned_new_threshold)
	{
	  cpp_define (pfile, "__cpp_aligned_new=201606");
	  cpp_define_formatted (pfile, "__STDCPP_DEFAULT_NEW_ALIGNMENT__=%d",
				aligned_new_threshold);
	}
      if (flag_new_ttp)
	cpp_define (pfile, "__cpp_template_template_args=201611");
      if (flag_threadsafe_statics)
	cpp_define (pfile, "__cpp_threadsafe_static_init=200806");
    }
  /* Note that we define this for C as well, so that we know if
     __attribute__((cleanup)) will interface with EH.  */
  if (flag_exceptions)
    {
      cpp_define (pfile, "__EXCEPTIONS");
      if (c_dialect_cxx ())
	cpp_define (pfile, "__cpp_exceptions=199711");
    }

  /* Represents the C++ ABI version, always defined so it can be used while
     preprocessing C and assembler.  */
  if (flag_abi_version == 0)
    /* We should have set this to something real in c_common_post_options.  */
    gcc_unreachable ();
  else if (flag_abi_version == 1)
    /* Due to a historical accident, this version had the value
       "102".  */
    builtin_define_with_int_value ("__GXX_ABI_VERSION", 102);
  else
    /* Newer versions have values 1002, 1003, ....  */
    builtin_define_with_int_value ("__GXX_ABI_VERSION",
				   1000 + flag_abi_version);

  /* libgcc needs to know this.  */
  if (targetm_common.except_unwind_info (&global_options) == UI_SJLJ)
    cpp_define (pfile, "__USING_SJLJ_EXCEPTIONS__");

  /* limits.h and stdint.h need to know these.  */
  builtin_define_type_max ("__SCHAR_MAX__", signed_char_type_node);
  builtin_define_type_max ("__SHRT_MAX__", short_integer_type_node);
  builtin_define_type_max ("__INT_MAX__", integer_type_node);
  builtin_define_type_max ("__LONG_MAX__", long_integer_type_node);
  builtin_define_type_max ("__LONG_LONG_MAX__", long_long_integer_type_node);
  builtin_define_type_minmax ("__WCHAR_MIN__", "__WCHAR_MAX__",
			      underlying_wchar_type_node);
  builtin_define_type_minmax ("__WINT_MIN__", "__WINT_MAX__", wint_type_node);
  builtin_define_type_max ("__PTRDIFF_MAX__", ptrdiff_type_node);
  builtin_define_type_max ("__SIZE_MAX__", size_type_node);

  /* These are needed for TS 18661-1.  */
  builtin_define_type_width ("__SCHAR_WIDTH__", signed_char_type_node,
			     unsigned_char_type_node);
  builtin_define_type_width ("__SHRT_WIDTH__", short_integer_type_node,
			     short_unsigned_type_node);
  builtin_define_type_width ("__INT_WIDTH__", integer_type_node,
			     unsigned_type_node);
  builtin_define_type_width ("__LONG_WIDTH__", long_integer_type_node,
			     long_unsigned_type_node);
  builtin_define_type_width ("__LONG_LONG_WIDTH__",
			     long_long_integer_type_node,
			     long_long_unsigned_type_node);
  builtin_define_type_width ("__WCHAR_WIDTH__", underlying_wchar_type_node,
			     NULL_TREE);
  builtin_define_type_width ("__WINT_WIDTH__", wint_type_node, NULL_TREE);
  builtin_define_type_width ("__PTRDIFF_WIDTH__", ptrdiff_type_node, NULL_TREE);
  builtin_define_type_width ("__SIZE_WIDTH__", size_type_node, NULL_TREE);

  if (c_dialect_cxx ())
    for (i = 0; i < NUM_INT_N_ENTS; i ++)
      if (int_n_enabled_p[i])
	{
	  char buf[35+20+20];

	  /* These are used to configure the C++ library.  */

	  if (!flag_iso || int_n_data[i].bitsize == POINTER_SIZE)
	    {
	      sprintf (buf, "__GLIBCXX_TYPE_INT_N_%d=__int%d", i, int_n_data[i].bitsize);
	      cpp_define (parse_in, buf);

	      sprintf (buf, "__GLIBCXX_BITSIZE_INT_N_%d=%d", i, int_n_data[i].bitsize);
	      cpp_define (parse_in, buf);
	    }
	}

  /* stdint.h and the testsuite need to know these.  */
  builtin_define_stdint_macros ();

  /* Provide information for library headers to determine whether to
     define macros such as __STDC_IEC_559__ and
     __STDC_IEC_559_COMPLEX__.  */
  builtin_define_with_int_value ("__GCC_IEC_559", cpp_iec_559_value ());
  builtin_define_with_int_value ("__GCC_IEC_559_COMPLEX",
				 cpp_iec_559_complex_value ());

  /* float.h needs these to correctly set FLT_EVAL_METHOD

     We define two values:

     __FLT_EVAL_METHOD__
       Which, depending on the value given for
       -fpermitted-flt-eval-methods, may be limited to only those values
       for FLT_EVAL_METHOD defined in C99/C11.

     __FLT_EVAL_METHOD_TS_18661_3__
       Which always permits the values for FLT_EVAL_METHOD defined in
       ISO/IEC TS 18661-3.  */
  builtin_define_with_int_value ("__FLT_EVAL_METHOD__",
				 c_flt_eval_method (true));
  builtin_define_with_int_value ("__FLT_EVAL_METHOD_TS_18661_3__",
				 c_flt_eval_method (false));

  /* And decfloat.h needs this.  */
  builtin_define_with_int_value ("__DEC_EVAL_METHOD__",
                                 TARGET_DEC_EVAL_METHOD);

  builtin_define_float_constants ("FLT", "F", "%s", "F", float_type_node);
  /* Cast the double precision constants.  This is needed when single
     precision constants are specified or when pragma FLOAT_CONST_DECIMAL64
     is used.  The correct result is computed by the compiler when using
     macros that include a cast.  We use a different cast for C++ to avoid
     problems with -Wold-style-cast.  */
  builtin_define_float_constants ("DBL", "L",
				  (c_dialect_cxx ()
				   ? "double(%s)"
				   : "((double)%s)"),
				  "", double_type_node);
  builtin_define_float_constants ("LDBL", "L", "%s", "L",
				  long_double_type_node);

  for (int i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    {
      if (FLOATN_NX_TYPE_NODE (i) == NULL_TREE)
	continue;
      char prefix[20], csuffix[20];
      sprintf (prefix, "FLT%d%s", floatn_nx_types[i].n,
	       floatn_nx_types[i].extended ? "X" : "");
      sprintf (csuffix, "F%d%s", floatn_nx_types[i].n,
	       floatn_nx_types[i].extended ? "x" : "");
      builtin_define_float_constants (prefix, csuffix, "%s", csuffix,
				      FLOATN_NX_TYPE_NODE (i));
    }

  /* For decfloat.h.  */
  builtin_define_decimal_float_constants ("DEC32", "DF", dfloat32_type_node);
  builtin_define_decimal_float_constants ("DEC64", "DD", dfloat64_type_node);
  builtin_define_decimal_float_constants ("DEC128", "DL", dfloat128_type_node);

  /* For fixed-point fibt, ibit, max, min, and epsilon.  */
  if (targetm.fixed_point_supported_p ())
    {
      builtin_define_fixed_point_constants ("SFRACT", "HR",
					    short_fract_type_node);
      builtin_define_fixed_point_constants ("USFRACT", "UHR",
					    unsigned_short_fract_type_node);
      builtin_define_fixed_point_constants ("FRACT", "R",
					    fract_type_node);
      builtin_define_fixed_point_constants ("UFRACT", "UR",
					    unsigned_fract_type_node);
      builtin_define_fixed_point_constants ("LFRACT", "LR",
					    long_fract_type_node);
      builtin_define_fixed_point_constants ("ULFRACT", "ULR",
					    unsigned_long_fract_type_node);
      builtin_define_fixed_point_constants ("LLFRACT", "LLR",
					    long_long_fract_type_node);
      builtin_define_fixed_point_constants ("ULLFRACT", "ULLR",
					    unsigned_long_long_fract_type_node);
      builtin_define_fixed_point_constants ("SACCUM", "HK",
					    short_accum_type_node);
      builtin_define_fixed_point_constants ("USACCUM", "UHK",
					    unsigned_short_accum_type_node);
      builtin_define_fixed_point_constants ("ACCUM", "K",
					    accum_type_node);
      builtin_define_fixed_point_constants ("UACCUM", "UK",
					    unsigned_accum_type_node);
      builtin_define_fixed_point_constants ("LACCUM", "LK",
					    long_accum_type_node);
      builtin_define_fixed_point_constants ("ULACCUM", "ULK",
					    unsigned_long_accum_type_node);
      builtin_define_fixed_point_constants ("LLACCUM", "LLK",
					    long_long_accum_type_node);
      builtin_define_fixed_point_constants ("ULLACCUM", "ULLK",
					    unsigned_long_long_accum_type_node);

      builtin_define_fixed_point_constants ("QQ", "", qq_type_node);
      builtin_define_fixed_point_constants ("HQ", "", hq_type_node);
      builtin_define_fixed_point_constants ("SQ", "", sq_type_node);
      builtin_define_fixed_point_constants ("DQ", "", dq_type_node);
      builtin_define_fixed_point_constants ("TQ", "", tq_type_node);
      builtin_define_fixed_point_constants ("UQQ", "", uqq_type_node);
      builtin_define_fixed_point_constants ("UHQ", "", uhq_type_node);
      builtin_define_fixed_point_constants ("USQ", "", usq_type_node);
      builtin_define_fixed_point_constants ("UDQ", "", udq_type_node);
      builtin_define_fixed_point_constants ("UTQ", "", utq_type_node);
      builtin_define_fixed_point_constants ("HA", "", ha_type_node);
      builtin_define_fixed_point_constants ("SA", "", sa_type_node);
      builtin_define_fixed_point_constants ("DA", "", da_type_node);
      builtin_define_fixed_point_constants ("TA", "", ta_type_node);
      builtin_define_fixed_point_constants ("UHA", "", uha_type_node);
      builtin_define_fixed_point_constants ("USA", "", usa_type_node);
      builtin_define_fixed_point_constants ("UDA", "", uda_type_node);
      builtin_define_fixed_point_constants ("UTA", "", uta_type_node);
    }

  /* For libgcc-internal use only.  */
  if (flag_building_libgcc)
    {
      /* Properties of floating-point modes for libgcc2.c.  */
      opt_scalar_float_mode mode_iter;
      FOR_EACH_MODE_IN_CLASS (mode_iter, MODE_FLOAT)
	{
	  scalar_float_mode mode = mode_iter.require ();
	  const char *name = GET_MODE_NAME (mode);
	  char *macro_name
	    = (char *) alloca (strlen (name)
			       + sizeof ("__LIBGCC__MANT_DIG__"));
	  sprintf (macro_name, "__LIBGCC_%s_MANT_DIG__", name);
	  builtin_define_with_int_value (macro_name,
					 REAL_MODE_FORMAT (mode)->p);
	  if (!targetm.scalar_mode_supported_p (mode)
	      || !targetm.libgcc_floating_mode_supported_p (mode))
	    continue;
	  macro_name = (char *) alloca (strlen (name)
					+ sizeof ("__LIBGCC_HAS__MODE__"));
	  sprintf (macro_name, "__LIBGCC_HAS_%s_MODE__", name);
	  cpp_define (pfile, macro_name);
	  macro_name = (char *) alloca (strlen (name)
					+ sizeof ("__LIBGCC__FUNC_EXT__"));
	  sprintf (macro_name, "__LIBGCC_%s_FUNC_EXT__", name);
	  char suffix[20] = "";
	  if (mode == TYPE_MODE (double_type_node))
	    ; /* Empty suffix correct.  */
	  else if (mode == TYPE_MODE (float_type_node))
	    suffix[0] = 'f';
	  else if (mode == TYPE_MODE (long_double_type_node))
	    suffix[0] = 'l';
	  else
	    {
	      bool found_suffix = false;
	      for (int i = 0; i < NUM_FLOATN_NX_TYPES; i++)
		if (FLOATN_NX_TYPE_NODE (i) != NULL_TREE
		    && mode == TYPE_MODE (FLOATN_NX_TYPE_NODE (i)))
		  {
		    sprintf (suffix, "f%d%s", floatn_nx_types[i].n,
			     floatn_nx_types[i].extended ? "x" : "");
		    found_suffix = true;
		    break;
		  }
	      gcc_assert (found_suffix);
	    }
	  builtin_define_with_value (macro_name, suffix, 0);

	  /* The way __LIBGCC_*_EXCESS_PRECISION__ is used is about
	     eliminating excess precision from results assigned to
	     variables - meaning it should be about the implicit excess
	     precision only.  */
	  bool excess_precision = false;
	  machine_mode float16_type_mode = (float16_type_node
					    ? TYPE_MODE (float16_type_node)
					    : VOIDmode);
	  switch (targetm.c.excess_precision
		    (EXCESS_PRECISION_TYPE_IMPLICIT))
	    {
	    case FLT_EVAL_METHOD_UNPREDICTABLE:
	    case FLT_EVAL_METHOD_PROMOTE_TO_LONG_DOUBLE:
	      excess_precision = (mode == float16_type_mode
				  || mode == TYPE_MODE (float_type_node)
				  || mode == TYPE_MODE (double_type_node));
	      break;

	    case FLT_EVAL_METHOD_PROMOTE_TO_DOUBLE:
	      excess_precision = (mode == float16_type_mode
				  || mode == TYPE_MODE (float_type_node));
	      break;
	    case FLT_EVAL_METHOD_PROMOTE_TO_FLOAT:
	      excess_precision = mode == float16_type_mode;
	      break;
	    case FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16:
	      excess_precision = false;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  macro_name = (char *) alloca (strlen (name)
					+ sizeof ("__LIBGCC__EXCESS_"
						  "PRECISION__"));
	  sprintf (macro_name, "__LIBGCC_%s_EXCESS_PRECISION__", name);
	  builtin_define_with_int_value (macro_name, excess_precision);
	}

      /* For libgcc crtstuff.c and libgcc2.c.  */
      builtin_define_with_int_value ("__LIBGCC_EH_TABLES_CAN_BE_READ_ONLY__",
				     EH_TABLES_CAN_BE_READ_ONLY);
#ifdef EH_FRAME_SECTION_NAME
      builtin_define_with_value ("__LIBGCC_EH_FRAME_SECTION_NAME__",
				 EH_FRAME_SECTION_NAME, 1);
#endif
#ifdef CTORS_SECTION_ASM_OP
      builtin_define_with_value ("__LIBGCC_CTORS_SECTION_ASM_OP__",
				 CTORS_SECTION_ASM_OP, 1);
#endif
#ifdef DTORS_SECTION_ASM_OP
      builtin_define_with_value ("__LIBGCC_DTORS_SECTION_ASM_OP__",
				 DTORS_SECTION_ASM_OP, 1);
#endif
#ifdef TEXT_SECTION_ASM_OP
      builtin_define_with_value ("__LIBGCC_TEXT_SECTION_ASM_OP__",
				 TEXT_SECTION_ASM_OP, 1);
#endif
#ifdef INIT_SECTION_ASM_OP
      builtin_define_with_value ("__LIBGCC_INIT_SECTION_ASM_OP__",
				 INIT_SECTION_ASM_OP, 1);
#endif
#ifdef INIT_ARRAY_SECTION_ASM_OP
      /* Despite the name of this target macro, the expansion is not
	 actually used, and may be empty rather than a string
	 constant.  */
      cpp_define (pfile, "__LIBGCC_INIT_ARRAY_SECTION_ASM_OP__");
#endif

      /* For libgcc enable-execute-stack.c.  */
      builtin_define_with_int_value ("__LIBGCC_TRAMPOLINE_SIZE__",
				     TRAMPOLINE_SIZE);

      /* For libgcc generic-morestack.c and unwinder code.  */
      if (STACK_GROWS_DOWNWARD)
	cpp_define (pfile, "__LIBGCC_STACK_GROWS_DOWNWARD__");

      /* For libgcc unwinder code.  */
#ifdef DONT_USE_BUILTIN_SETJMP
      cpp_define (pfile, "__LIBGCC_DONT_USE_BUILTIN_SETJMP__");
#endif
#ifdef DWARF_ALT_FRAME_RETURN_COLUMN
      builtin_define_with_int_value ("__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__",
				     DWARF_ALT_FRAME_RETURN_COLUMN);
#endif
      builtin_define_with_int_value ("__LIBGCC_DWARF_FRAME_REGISTERS__",
				     DWARF_FRAME_REGISTERS);
#ifdef EH_RETURN_STACKADJ_RTX
      cpp_define (pfile, "__LIBGCC_EH_RETURN_STACKADJ_RTX__");
#endif
#ifdef JMP_BUF_SIZE
      builtin_define_with_int_value ("__LIBGCC_JMP_BUF_SIZE__",
				     JMP_BUF_SIZE);
#endif
      builtin_define_with_int_value ("__LIBGCC_STACK_POINTER_REGNUM__",
				     STACK_POINTER_REGNUM);

      /* For libgcov.  */
      builtin_define_with_int_value ("__LIBGCC_VTABLE_USES_DESCRIPTORS__",
				     TARGET_VTABLE_USES_DESCRIPTORS);
    }

  /* For use in assembly language.  */
  builtin_define_with_value ("__REGISTER_PREFIX__", REGISTER_PREFIX, 0);
  builtin_define_with_value ("__USER_LABEL_PREFIX__", user_label_prefix, 0);

  /* Misc.  */
  if (flag_gnu89_inline)
    cpp_define (pfile, "__GNUC_GNU_INLINE__");
  else
    cpp_define (pfile, "__GNUC_STDC_INLINE__");

  if (flag_no_inline)
    cpp_define (pfile, "__NO_INLINE__");

  if (flag_iso)
    cpp_define (pfile, "__STRICT_ANSI__");

  if (!flag_signed_char)
    cpp_define (pfile, "__CHAR_UNSIGNED__");

  if (c_dialect_cxx () && TYPE_UNSIGNED (wchar_type_node))
    cpp_define (pfile, "__WCHAR_UNSIGNED__");

  cpp_atomic_builtins (pfile);
    
#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_cfi_asm ())
    cpp_define (pfile, "__GCC_HAVE_DWARF2_CFI_ASM");
#endif

  /* Make the choice of ObjC runtime visible to source code.  */
  if (c_dialect_objc () && flag_next_runtime)
    cpp_define (pfile, "__NEXT_RUNTIME__");

  /* Show the availability of some target pragmas.  */
  cpp_define (pfile, "__PRAGMA_REDEFINE_EXTNAME");

  /* Make the choice of the stack protector runtime visible to source code.
     The macro names and values here were chosen for compatibility with an
     earlier implementation, i.e. ProPolice.  */
  if (flag_stack_protect == 4)
    cpp_define (pfile, "__SSP_EXPLICIT__=4");
  if (flag_stack_protect == 3)
    cpp_define (pfile, "__SSP_STRONG__=3");
  if (flag_stack_protect == 2)
    cpp_define (pfile, "__SSP_ALL__=2");
  else if (flag_stack_protect == 1)
    cpp_define (pfile, "__SSP__=1");

  if (flag_openacc)
    cpp_define (pfile, "_OPENACC=201306");

  if (flag_openmp)
    cpp_define (pfile, "_OPENMP=201511");

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i])
      {
	char buf[15+20];
	sprintf(buf, "__SIZEOF_INT%d__", int_n_data[i].bitsize);
	builtin_define_type_sizeof (buf,
				    int_n_trees[i].signed_type);
      }
  builtin_define_type_sizeof ("__SIZEOF_WCHAR_T__", wchar_type_node);
  builtin_define_type_sizeof ("__SIZEOF_WINT_T__", wint_type_node);
  builtin_define_type_sizeof ("__SIZEOF_PTRDIFF_T__",
			      unsigned_ptrdiff_type_node);

  /* A straightforward target hook doesn't work, because of problems
     linking that hook's body when part of non-C front ends.  */
# define preprocessing_asm_p() (cpp_get_options (pfile)->lang == CLK_ASM)
# define preprocessing_trad_p() (cpp_get_options (pfile)->traditional)
# define builtin_define(TXT) cpp_define (pfile, TXT)
# define builtin_assert(TXT) cpp_assert (pfile, TXT)
  TARGET_CPU_CPP_BUILTINS ();
  TARGET_OS_CPP_BUILTINS ();
  TARGET_OBJFMT_CPP_BUILTINS ();

  /* Support the __declspec keyword by turning them into attributes.
     Note that the current way we do this may result in a collision
     with predefined attributes later on.  This can be solved by using
     one attribute, say __declspec__, and passing args to it.  The
     problem with that approach is that args are not accumulated: each
     new appearance would clobber any existing args.  */
  if (TARGET_DECLSPEC)
    builtin_define ("__declspec(x)=__attribute__((x))");

  /* If decimal floating point is supported, tell the user if the
     alternate format (BID) is used instead of the standard (DPD)
     format.  */
  if (ENABLE_DECIMAL_FLOAT && ENABLE_DECIMAL_BID_FORMAT)
    cpp_define (pfile, "__DECIMAL_BID_FORMAT__");
}

/* Pass an object-like macro.  If it doesn't lie in the user's
   namespace, defines it unconditionally.  Otherwise define a version
   with two leading underscores, and another version with two leading
   and trailing underscores, and define the original only if an ISO
   standard was not nominated.

   e.g. passing "unix" defines "__unix", "__unix__" and possibly
   "unix".  Passing "_mips" defines "__mips", "__mips__" and possibly
   "_mips".  */
void
builtin_define_std (const char *macro)
{
  size_t len = strlen (macro);
  char *buff = (char *) alloca (len + 5);
  char *p = buff + 2;
  char *q = p + len;

  /* prepend __ (or maybe just _) if in user's namespace.  */
  memcpy (p, macro, len + 1);
  if (!( *p == '_' && (p[1] == '_' || ISUPPER (p[1]))))
    {
      if (*p != '_')
	*--p = '_';
      if (p[1] != '_')
	*--p = '_';
    }
  cpp_define (parse_in, p);

  /* If it was in user's namespace...  */
  if (p != buff + 2)
    {
      /* Define the macro with leading and following __.  */
      if (q[-1] != '_')
	*q++ = '_';
      if (q[-2] != '_')
	*q++ = '_';
      *q = '\0';
      cpp_define (parse_in, p);

      /* Finally, define the original macro if permitted.  */
      if (!flag_iso)
	cpp_define (parse_in, macro);
    }
}

/* Pass an object-like macro and a value to define it to.  The third
   parameter says whether or not to turn the value into a string
   constant.  */
void
builtin_define_with_value (const char *macro, const char *expansion, int is_str)
{
  char *buf;
  size_t mlen = strlen (macro);
  size_t elen = strlen (expansion);
  size_t extra = 2;  /* space for an = and a NUL */

  if (is_str)
    {
      char *quoted_expansion = (char *) alloca (elen * 4 + 1);
      const char *p;
      char *q;
      extra += 2;  /* space for two quote marks */
      for (p = expansion, q = quoted_expansion; *p; p++)
	{
	  switch (*p)
	    {
	    case '\n':
	      *q++ = '\\';
	      *q++ = 'n';
	      break;

	    case '\t':
	      *q++ = '\\';
	      *q++ = 't';
	      break;

	    case '\\':
	      *q++ = '\\';
	      *q++ = '\\';
	      break;

	    case '"':
	      *q++ = '\\';
	      *q++ = '"';
	      break;

	    default:
	      if (ISPRINT ((unsigned char) *p))
		*q++ = *p;
	      else
		{
		  sprintf (q, "\\%03o", (unsigned char) *p);
		  q += 4;
		}
	    }
	}
      *q = '\0';
      expansion = quoted_expansion;
      elen = q - expansion;
    }

  buf = (char *) alloca (mlen + elen + extra);
  if (is_str)
    sprintf (buf, "%s=\"%s\"", macro, expansion);
  else
    sprintf (buf, "%s=%s", macro, expansion);

  cpp_define (parse_in, buf);
}


/* Pass an object-like macro and an integer value to define it to.  */
void
builtin_define_with_int_value (const char *macro, HOST_WIDE_INT value)
{
  char *buf;
  size_t mlen = strlen (macro);
  size_t vlen = 18;
  size_t extra = 2; /* space for = and NUL.  */

  buf = (char *) alloca (mlen + vlen + extra);
  memcpy (buf, macro, mlen);
  buf[mlen] = '=';
  sprintf (buf + mlen + 1, HOST_WIDE_INT_PRINT_DEC, value);

  cpp_define (parse_in, buf);
}

/* builtin_define_with_hex_fp_value is very expensive, so the following
   array and function allows it to be done lazily when __DBL_MAX__
   etc. is first used.  */

struct GTY(()) lazy_hex_fp_value_struct
{
  const char *hex_str;
  cpp_macro *macro;
  machine_mode mode;
  int digits;
  const char *fp_suffix;
};
static GTY(()) struct lazy_hex_fp_value_struct lazy_hex_fp_values[12];
static GTY(()) int lazy_hex_fp_value_count;

static bool
lazy_hex_fp_value (cpp_reader *pfile ATTRIBUTE_UNUSED,
		   cpp_hashnode *node)
{
  REAL_VALUE_TYPE real;
  char dec_str[64], buf1[256];
  unsigned int idx;
  if (node->value.builtin < BT_FIRST_USER
      || (int) node->value.builtin >= BT_FIRST_USER + lazy_hex_fp_value_count)
    return false;

  idx = node->value.builtin - BT_FIRST_USER;
  real_from_string (&real, lazy_hex_fp_values[idx].hex_str);
  real_to_decimal_for_mode (dec_str, &real, sizeof (dec_str),
			    lazy_hex_fp_values[idx].digits, 0,
			    lazy_hex_fp_values[idx].mode);

  sprintf (buf1, "%s%s", dec_str, lazy_hex_fp_values[idx].fp_suffix);
  node->flags &= ~(NODE_BUILTIN | NODE_USED);
  node->value.macro = lazy_hex_fp_values[idx].macro;
  for (idx = 0; idx < node->value.macro->count; idx++)
    if (node->value.macro->exp.tokens[idx].type == CPP_NUMBER)
      break;
  gcc_assert (idx < node->value.macro->count);
  node->value.macro->exp.tokens[idx].val.str.len = strlen (buf1);
  node->value.macro->exp.tokens[idx].val.str.text
    = (const unsigned char *) ggc_strdup (buf1);
  return true;
}

/* Pass an object-like macro a hexadecimal floating-point value.  */
static void
builtin_define_with_hex_fp_value (const char *macro,
				  tree type, int digits,
				  const char *hex_str,
				  const char *fp_suffix,
				  const char *fp_cast)
{
  REAL_VALUE_TYPE real;
  char dec_str[64], buf1[256], buf2[256];

  /* This is very expensive, so if possible expand them lazily.  */
  if (lazy_hex_fp_value_count < 12
      && flag_dump_macros == 0
      && !cpp_get_options (parse_in)->traditional)
    {
      struct cpp_hashnode *node;
      if (lazy_hex_fp_value_count == 0)
	cpp_get_callbacks (parse_in)->user_builtin_macro = lazy_hex_fp_value;
      sprintf (buf2, fp_cast, "1.1");
      sprintf (buf1, "%s=%s", macro, buf2);
      cpp_define (parse_in, buf1);
      node = C_CPP_HASHNODE (get_identifier (macro));
      lazy_hex_fp_values[lazy_hex_fp_value_count].hex_str
	= ggc_strdup (hex_str);
      lazy_hex_fp_values[lazy_hex_fp_value_count].mode = TYPE_MODE (type);
      lazy_hex_fp_values[lazy_hex_fp_value_count].digits = digits;
      lazy_hex_fp_values[lazy_hex_fp_value_count].fp_suffix = fp_suffix;
      lazy_hex_fp_values[lazy_hex_fp_value_count].macro = node->value.macro;
      node->flags |= NODE_BUILTIN;
      node->value.builtin
	= (enum cpp_builtin_type) (BT_FIRST_USER + lazy_hex_fp_value_count);
      lazy_hex_fp_value_count++;
      return;
    }

  /* Hex values are really cool and convenient, except that they're
     not supported in strict ISO C90 mode.  First, the "p-" sequence
     is not valid as part of a preprocessor number.  Second, we get a
     pedwarn from the preprocessor, which has no context, so we can't
     suppress the warning with __extension__.

     So instead what we do is construct the number in hex (because
     it's easy to get the exact correct value), parse it as a real,
     then print it back out as decimal.  */

  real_from_string (&real, hex_str);
  real_to_decimal_for_mode (dec_str, &real, sizeof (dec_str), digits, 0,
			    TYPE_MODE (type));

  /* Assemble the macro in the following fashion
     macro = fp_cast [dec_str fp_suffix] */
  sprintf (buf1, "%s%s", dec_str, fp_suffix);
  sprintf (buf2, fp_cast, buf1);
  sprintf (buf1, "%s=%s", macro, buf2);

  cpp_define (parse_in, buf1);
}

/* Return a string constant for the suffix for a value of type TYPE
   promoted according to the integer promotions.  The type must be one
   of the standard integer type nodes.  */

static const char *
type_suffix (tree type)
{
  static const char *const suffixes[] = { "", "U", "L", "UL", "LL", "ULL" };
  int unsigned_suffix;
  int is_long;
  int tp = TYPE_PRECISION (type);

  if (type == long_long_integer_type_node
      || type == long_long_unsigned_type_node
      || tp > TYPE_PRECISION (long_integer_type_node))
    is_long = 2;
  else if (type == long_integer_type_node
	   || type == long_unsigned_type_node
	   || tp > TYPE_PRECISION (integer_type_node))
    is_long = 1;
  else if (type == integer_type_node
	   || type == unsigned_type_node
	   || type == short_integer_type_node
	   || type == short_unsigned_type_node
	   || type == signed_char_type_node
	   || type == unsigned_char_type_node
	   /* ??? "char" is not a signed or unsigned integer type and
	      so is not permitted for the standard typedefs, but some
	      systems use it anyway.  */
	   || type == char_type_node)
    is_long = 0;
  else
    gcc_unreachable ();

  unsigned_suffix = TYPE_UNSIGNED (type);
  if (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
    unsigned_suffix = 0;
  return suffixes[is_long * 2 + unsigned_suffix];
}

/* Define MACRO as a <stdint.h> constant-suffix macro for TYPE.  */
static void
builtin_define_constants (const char *macro, tree type)
{
  const char *suffix;
  char *buf;

  suffix = type_suffix (type);

  if (suffix[0] == 0)
    {
      buf = (char *) alloca (strlen (macro) + 6);
      sprintf (buf, "%s(c)=c", macro);
    }
  else
    {
      buf = (char *) alloca (strlen (macro) + 9 + strlen (suffix) + 1);
      sprintf (buf, "%s(c)=c ## %s", macro, suffix);
    }

  cpp_define (parse_in, buf);
}

/* Define MAX for TYPE based on the precision of the type.  */

static void
builtin_define_type_max (const char *macro, tree type)
{
  builtin_define_type_minmax (NULL, macro, type);
}

/* Given a value with COUNT LSBs set, fill BUF with a hexidecimal
   representation of that value.  For example, a COUNT of 10 would
   return "0x3ff".  */

static void
print_bits_of_hex (char *buf, int bufsz, int count)
{
  gcc_assert (bufsz > 3);
  *buf++ = '0';
  *buf++ = 'x';
  bufsz -= 2;

  gcc_assert (count > 0);

  switch (count % 4) {
  case 0:
    break;
  case 1:
    *buf++ = '1';
    bufsz --;
    count -= 1;
    break;
  case 2:
    *buf++ = '3';
    bufsz --;
    count -= 2;
    break;
  case 3:
    *buf++ = '7';
    bufsz --;
    count -= 3;
    break;
  }
  while (count >= 4)
    {
      gcc_assert (bufsz > 1);
      *buf++ = 'f';
      bufsz --;
      count -= 4;
    }
  gcc_assert (bufsz > 0);
  *buf++ = 0;
}

/* Define MIN_MACRO (if not NULL) and MAX_MACRO for TYPE based on the
   precision of the type.  */

static void
builtin_define_type_minmax (const char *min_macro, const char *max_macro,
			    tree type)
{
#define PBOH_SZ (MAX_BITSIZE_MODE_ANY_INT/4+4)
  char value[PBOH_SZ];

  const char *suffix;
  char *buf;
  int bits;

  bits = TYPE_PRECISION (type) + (TYPE_UNSIGNED (type) ? 0 : -1);

  print_bits_of_hex (value, PBOH_SZ, bits);

  suffix = type_suffix (type);

  buf = (char *) alloca (strlen (max_macro) + 1 + strlen (value)
                         + strlen (suffix) + 1);
  sprintf (buf, "%s=%s%s", max_macro, value, suffix);

  cpp_define (parse_in, buf);

  if (min_macro)
    {
      if (TYPE_UNSIGNED (type))
	{
	  buf = (char *) alloca (strlen (min_macro) + 2 + strlen (suffix) + 1);
	  sprintf (buf, "%s=0%s", min_macro, suffix);
	}
      else
	{
	  buf = (char *) alloca (strlen (min_macro) + 3
				 + strlen (max_macro) + 6);
	  sprintf (buf, "%s=(-%s - 1)", min_macro, max_macro);
	}
      cpp_define (parse_in, buf);
    }
}

/* Define WIDTH_MACRO for the width of TYPE.  If TYPE2 is not NULL,
   both types must have the same width.  */

static void
builtin_define_type_width (const char *width_macro, tree type, tree type2)
{
  if (type2 != NULL_TREE)
    gcc_assert (TYPE_PRECISION (type) == TYPE_PRECISION (type2));
  builtin_define_with_int_value (width_macro, TYPE_PRECISION (type));
}

#include "gt-c-family-c-cppbuiltin.h"
