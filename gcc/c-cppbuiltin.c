/* Define builtin-in macros for the C family front ends.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "version.h"
#include "flags.h"
#include "real.h"
#include "c-common.h"
#include "c-pragma.h"
#include "output.h"
#include "except.h"		/* For USING_SJLJ_EXCEPTIONS.  */
#include "debug.h"		/* For dwarf2out_do_frame.  */
#include "toplev.h"
#include "tm_p.h"		/* Target prototypes.  */
#include "target.h"

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
void builtin_define_std (const char *) ATTRIBUTE_UNUSED;
static void builtin_define_with_int_value (const char *, HOST_WIDE_INT);
static void builtin_define_with_hex_fp_value (const char *, tree,
					      int, const char *,
					      const char *,
					      const char *);
static void builtin_define_stdint_macros (void);
static void builtin_define_constants (const char *, tree);
static void builtin_define_type_max (const char *, tree);
static void builtin_define_type_minmax (const char *, const char *, tree);
static void builtin_define_type_precision (const char *, tree);
static void builtin_define_type_sizeof (const char *, tree);
static void builtin_define_float_constants (const char *,
					    const char *,
					    const char *,
					    tree);
static void define__GNUC__ (void);

/* Define NAME with value TYPE precision.  */
static void
builtin_define_type_precision (const char *name, tree type)
{
  builtin_define_with_int_value (name, TYPE_PRECISION (type));
}

/* Define NAME with value TYPE size_unit.  */
static void
builtin_define_type_sizeof (const char *name, tree type)
{
  builtin_define_with_int_value (name,
				 tree_low_cst (TYPE_SIZE_UNIT (type), 1));
}

/* Define the float.h constants for TYPE using NAME_PREFIX, FP_SUFFIX,
   and FP_CAST. */
static void
builtin_define_float_constants (const char *name_prefix,
		                const char *fp_suffix,
				const char *fp_cast,
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
  const struct real_format *ldfmt;

  char name[64], buf[128];
  int dig, min_10_exp, max_10_exp;
  int decimal_dig;

  fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
  gcc_assert (fmt->b != 10);
  ldfmt = REAL_MODE_FORMAT (TYPE_MODE (long_double_type_node));
  gcc_assert (ldfmt->b != 10);

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
      = 1 + (fmt->p < ldfmt->p ? ldfmt->p : fmt->p) * log10_b;
    decimal_dig = d_decimal_dig;
    if (decimal_dig < d_decimal_dig)
      decimal_dig++;
  }
  if (type == long_double_type_node)
    builtin_define_with_int_value ("__DECIMAL_DIG__", decimal_dig);

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

  /* For C++ std::numeric_limits<T>::denorm_min.  The minimum denormalized
     positive floating-point number, b**(emin-p).  Zero for formats that
     don't support denormals.  */
  sprintf (name, "__%s_DENORM_MIN__", name_prefix);
  if (fmt->has_denorm)
    {
      sprintf (buf, "0x1p%d", fmt->emin - fmt->p);
      builtin_define_with_hex_fp_value (name, type, decimal_dig,
					buf, fp_suffix, fp_cast);
    }
  else
    {
      sprintf (buf, "0.0%s", fp_suffix);
      builtin_define_with_value (name, buf, 0);
    }

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

/* Define __GNUC__, __GNUC_MINOR__ and __GNUC_PATCHLEVEL__.  */
static void
define__GNUC__ (void)
{
  int major, minor, patchlevel;

  if (sscanf (BASEVER, "%d.%d.%d", &major, &minor, &patchlevel) != 3)
    {
      sscanf (BASEVER, "%d.%d", &major, &minor);
      patchlevel = 0;
    }
  cpp_define_formatted (parse_in, "__GNUC__=%d", major);
  cpp_define_formatted (parse_in, "__GNUC_MINOR__=%d", minor);
  cpp_define_formatted (parse_in, "__GNUC_PATCHLEVEL__=%d", patchlevel);

  if (c_dialect_cxx ())
    cpp_define_formatted (parse_in, "__GNUG__=%d", major);
}

/* Define macros used by <stdint.h>.  */
static void
builtin_define_stdint_macros (void)
{
  builtin_define_type_max ("__INTMAX_MAX__", intmax_type_node);
  builtin_define_constants ("__INTMAX_C", intmax_type_node);
  builtin_define_type_max ("__UINTMAX_MAX__", uintmax_type_node);
  builtin_define_constants ("__UINTMAX_C", uintmax_type_node);
  if (sig_atomic_type_node)
    builtin_define_type_minmax ("__SIG_ATOMIC_MIN__", "__SIG_ATOMIC_MAX__",
				sig_atomic_type_node);
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
  if (uint16_type_node)
    builtin_define_type_max ("__UINT16_MAX__", uint16_type_node);
  if (c_uint32_type_node)
    builtin_define_type_max ("__UINT32_MAX__", c_uint32_type_node);
  if (c_uint64_type_node)
    builtin_define_type_max ("__UINT64_MAX__", c_uint64_type_node);
  if (int_least8_type_node)
    {
      builtin_define_type_max ("__INT_LEAST8_MAX__", int_least8_type_node);
      builtin_define_constants ("__INT8_C", int_least8_type_node);
    }
  if (int_least16_type_node)
    {
      builtin_define_type_max ("__INT_LEAST16_MAX__", int_least16_type_node);
      builtin_define_constants ("__INT16_C", int_least16_type_node);
    }
  if (int_least32_type_node)
    {
      builtin_define_type_max ("__INT_LEAST32_MAX__", int_least32_type_node);
      builtin_define_constants ("__INT32_C", int_least32_type_node);
    }
  if (int_least64_type_node)
    {
      builtin_define_type_max ("__INT_LEAST64_MAX__", int_least64_type_node);
      builtin_define_constants ("__INT64_C", int_least64_type_node);
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
    builtin_define_type_max ("__INT_FAST8_MAX__", int_fast8_type_node);
  if (int_fast16_type_node)
    builtin_define_type_max ("__INT_FAST16_MAX__", int_fast16_type_node);
  if (int_fast32_type_node)
    builtin_define_type_max ("__INT_FAST32_MAX__", int_fast32_type_node);
  if (int_fast64_type_node)
    builtin_define_type_max ("__INT_FAST64_MAX__", int_fast64_type_node);
  if (uint_fast8_type_node)
    builtin_define_type_max ("__UINT_FAST8_MAX__", uint_fast8_type_node);
  if (uint_fast16_type_node)
    builtin_define_type_max ("__UINT_FAST16_MAX__", uint_fast16_type_node);
  if (uint_fast32_type_node)
    builtin_define_type_max ("__UINT_FAST32_MAX__", uint_fast32_type_node);
  if (uint_fast64_type_node)
    builtin_define_type_max ("__UINT_FAST64_MAX__", uint_fast64_type_node);
  if (intptr_type_node)
    builtin_define_type_max ("__INTPTR_MAX__", intptr_type_node);
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
  if (!prev->optimize_size && cur->optimize_size)
    cpp_define (pfile, "__OPTIMIZE_SIZE__");
  else if (prev->optimize_size && !cur->optimize_size)
    cpp_undef (pfile, "__OPTIMIZE_SIZE__");

  if (!prev->optimize && cur->optimize)
    cpp_define (pfile, "__OPTIMIZE__");
  else if (prev->optimize && !cur->optimize)
    cpp_undef (pfile, "__OPTIMIZE__");

  prev_fast_math = fast_math_flags_struct_set_p (prev);
  cur_fast_math  = fast_math_flags_struct_set_p (cur);
  if (!prev_fast_math && cur_fast_math)
    cpp_define (pfile, "__FAST_MATH__");
  else if (prev_fast_math && !cur_fast_math)
    cpp_undef (pfile, "__FAST_MATH__");

  if (!prev->flag_signaling_nans && cur->flag_signaling_nans)
    cpp_define (pfile, "__SUPPORT_SNAN__");
  else if (prev->flag_signaling_nans && !cur->flag_signaling_nans)
    cpp_undef (pfile, "__SUPPORT_SNAN__");

  if (!prev->flag_finite_math_only && cur->flag_finite_math_only)
    {
      cpp_undef (pfile, "__FINITE_MATH_ONLY__");
      cpp_define (pfile, "__FINITE_MATH_ONLY__=1");
    }
  else if (!prev->flag_finite_math_only && cur->flag_finite_math_only)
    {
      cpp_undef (pfile, "__FINITE_MATH_ONLY__");
      cpp_define (pfile, "__FINITE_MATH_ONLY__=0");
    }
}


/* Hook that registers front end and target-specific built-ins.  */
void
c_cpp_builtins (cpp_reader *pfile)
{
  /* -undef turns off target-specific built-ins.  */
  if (flag_undef)
    return;

  define__GNUC__ ();

  /* For stddef.h.  They require macros defined in c-common.c.  */
  c_stddef_cpp_builtins ();

  if (c_dialect_cxx ())
    {
      if (flag_weak && SUPPORTS_ONE_ONLY)
	cpp_define (pfile, "__GXX_WEAK__=1");
      else
	cpp_define (pfile, "__GXX_WEAK__=0");
      if (warn_deprecated)
	cpp_define (pfile, "__DEPRECATED");
      if (flag_rtti)
	cpp_define (pfile, "__GXX_RTTI");
      if (cxx_dialect == cxx0x)
        cpp_define (pfile, "__GXX_EXPERIMENTAL_CXX0X__");
    }
  /* Note that we define this for C as well, so that we know if
     __attribute__((cleanup)) will interface with EH.  */
  if (flag_exceptions)
    cpp_define (pfile, "__EXCEPTIONS");

  /* Represents the C++ ABI version, always defined so it can be used while
     preprocessing C and assembler.  */
  if (flag_abi_version == 0)
    /* Use a very large value so that:

	 #if __GXX_ABI_VERSION >= <value for version X>

       will work whether the user explicitly says "-fabi-version=x" or
       "-fabi-version=0".  Do not use INT_MAX because that will be
       different from system to system.  */
    builtin_define_with_int_value ("__GXX_ABI_VERSION", 999999);
  else if (flag_abi_version == 1)
    /* Due to a historical accident, this version had the value
       "102".  */
    builtin_define_with_int_value ("__GXX_ABI_VERSION", 102);
  else
    /* Newer versions have values 1002, 1003, ....  */
    builtin_define_with_int_value ("__GXX_ABI_VERSION",
				   1000 + flag_abi_version);

  /* libgcc needs to know this.  */
  if (USING_SJLJ_EXCEPTIONS)
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

  builtin_define_type_precision ("__CHAR_BIT__", char_type_node);

  /* stdint.h and the testsuite need to know these.  */
  builtin_define_stdint_macros ();

  /* float.h needs to know these.  */

  builtin_define_with_int_value ("__FLT_EVAL_METHOD__",
				 TARGET_FLT_EVAL_METHOD);

  /* And decfloat.h needs this.  */
  builtin_define_with_int_value ("__DEC_EVAL_METHOD__",
                                 TARGET_DEC_EVAL_METHOD);

  builtin_define_float_constants ("FLT", "F", "%s", float_type_node);
  /* Cast the double precision constants.  This is needed when single
     precision constants are specified or when pragma FLOAT_CONST_DECIMAL64
     is used.  The correct result is computed by the compiler when using
     macros that include a cast.  */
  builtin_define_float_constants ("DBL", "L", "((double)%s)", double_type_node);
  builtin_define_float_constants ("LDBL", "L", "%s", long_double_type_node);

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

  /* For use in assembly language.  */
  builtin_define_with_value ("__REGISTER_PREFIX__", REGISTER_PREFIX, 0);
  builtin_define_with_value ("__USER_LABEL_PREFIX__", user_label_prefix, 0);

  /* Misc.  */
  builtin_define_with_value ("__VERSION__", version_string, 1);

  if (flag_gnu89_inline)
    cpp_define (pfile, "__GNUC_GNU_INLINE__");
  else
    cpp_define (pfile, "__GNUC_STDC_INLINE__");

  /* Definitions for LP64 model.  */
  if (TYPE_PRECISION (long_integer_type_node) == 64
      && POINTER_SIZE == 64
      && TYPE_PRECISION (integer_type_node) == 32)
    {
      cpp_define (pfile, "_LP64");
      cpp_define (pfile, "__LP64__");
    }

  /* Other target-independent built-ins determined by command-line
     options.  */
  if (optimize_size)
    cpp_define (pfile, "__OPTIMIZE_SIZE__");
  if (optimize)
    cpp_define (pfile, "__OPTIMIZE__");

  if (fast_math_flags_set_p ())
    cpp_define (pfile, "__FAST_MATH__");
  if (flag_no_inline)
    cpp_define (pfile, "__NO_INLINE__");
  if (flag_signaling_nans)
    cpp_define (pfile, "__SUPPORT_SNAN__");
  if (flag_finite_math_only)
    cpp_define (pfile, "__FINITE_MATH_ONLY__=1");
  else
    cpp_define (pfile, "__FINITE_MATH_ONLY__=0");
  if (flag_pic)
    {
      builtin_define_with_int_value ("__pic__", flag_pic);
      builtin_define_with_int_value ("__PIC__", flag_pic);
    }
  if (flag_pie)
    {
      builtin_define_with_int_value ("__pie__", flag_pie);
      builtin_define_with_int_value ("__PIE__", flag_pie);
    }

  if (flag_iso)
    cpp_define (pfile, "__STRICT_ANSI__");

  if (!flag_signed_char)
    cpp_define (pfile, "__CHAR_UNSIGNED__");

  if (c_dialect_cxx () && TYPE_UNSIGNED (wchar_type_node))
    cpp_define (pfile, "__WCHAR_UNSIGNED__");

  /* Tell source code if the compiler makes sync_compare_and_swap
     builtins available.  */
#ifdef HAVE_sync_compare_and_swapqi
  if (HAVE_sync_compare_and_swapqi)
    cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1");
#endif

#ifdef HAVE_sync_compare_and_swaphi
  if (HAVE_sync_compare_and_swaphi)
    cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2");
#endif

#ifdef HAVE_sync_compare_and_swapsi
  if (HAVE_sync_compare_and_swapsi)
    cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4");
#endif

#ifdef HAVE_sync_compare_and_swapdi
  if (HAVE_sync_compare_and_swapdi)
    cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8");
#endif

#ifdef HAVE_sync_compare_and_swapti
  if (HAVE_sync_compare_and_swapti)
    cpp_define (pfile, "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16");
#endif

#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_cfi_asm ())
    cpp_define (pfile, "__GCC_HAVE_DWARF2_CFI_ASM");
#endif

  /* Make the choice of ObjC runtime visible to source code.  */
  if (c_dialect_objc () && flag_next_runtime)
    cpp_define (pfile, "__NEXT_RUNTIME__");

  /* Show the availability of some target pragmas.  */
  cpp_define (pfile, "__PRAGMA_REDEFINE_EXTNAME");

  if (targetm.handle_pragma_extern_prefix)
    cpp_define (pfile, "__PRAGMA_EXTERN_PREFIX");

  /* Make the choice of the stack protector runtime visible to source code.
     The macro names and values here were chosen for compatibility with an
     earlier implementation, i.e. ProPolice.  */
  if (flag_stack_protect == 2)
    cpp_define (pfile, "__SSP_ALL__=2");
  else if (flag_stack_protect == 1)
    cpp_define (pfile, "__SSP__=1");

  if (flag_openmp)
    cpp_define (pfile, "_OPENMP=200805");

  builtin_define_type_sizeof ("__SIZEOF_INT__", integer_type_node);
  builtin_define_type_sizeof ("__SIZEOF_LONG__", long_integer_type_node);
  builtin_define_type_sizeof ("__SIZEOF_LONG_LONG__",
			      long_long_integer_type_node);
  builtin_define_type_sizeof ("__SIZEOF_SHORT__", short_integer_type_node);
  builtin_define_type_sizeof ("__SIZEOF_FLOAT__", float_type_node);
  builtin_define_type_sizeof ("__SIZEOF_DOUBLE__", double_type_node);
  builtin_define_type_sizeof ("__SIZEOF_LONG_DOUBLE__", long_double_type_node);
  builtin_define_type_sizeof ("__SIZEOF_SIZE_T__", size_type_node);
  builtin_define_type_sizeof ("__SIZEOF_WCHAR_T__", wchar_type_node);
  builtin_define_type_sizeof ("__SIZEOF_WINT_T__", wint_type_node);
  builtin_define_type_sizeof ("__SIZEOF_PTRDIFF_T__",
			      unsigned_ptrdiff_type_node);
  /* ptr_type_node can't be used here since ptr_mode is only set when
     toplev calls backend_init which is not done with -E switch.  */
  builtin_define_with_int_value ("__SIZEOF_POINTER__",
				 POINTER_SIZE / BITS_PER_UNIT);

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

  builtin_define_with_int_value ("__BIGGEST_ALIGNMENT__",
				 BIGGEST_ALIGNMENT / BITS_PER_UNIT);
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
    extra += 2;  /* space for two quote marks */

  buf = (char *) alloca (mlen + elen + extra);
  if (is_str)
    sprintf (buf, "%s=\"%s\"", macro, expansion);
  else
    sprintf (buf, "%s=%s", macro, expansion);

  cpp_define (parse_in, buf);
}


/* Pass an object-like macro and an integer value to define it to.  */
static void
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

  if (type == long_long_integer_type_node
      || type == long_long_unsigned_type_node)
    is_long = 2;
  else if (type == long_integer_type_node
	   || type == long_unsigned_type_node)
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

/* Define MIN_MACRO (if not NULL) and MAX_MACRO for TYPE based on the
   precision of the type.  */

static void
builtin_define_type_minmax (const char *min_macro, const char *max_macro,
			    tree type)
{
  static const char *const values[]
    = { "127", "255",
	"32767", "65535",
	"2147483647", "4294967295",
	"9223372036854775807", "18446744073709551615",
	"170141183460469231731687303715884105727",
	"340282366920938463463374607431768211455" };

  const char *value, *suffix;
  char *buf;
  size_t idx;

  /* Pre-rendering the values mean we don't have to futz with printing a
     multi-word decimal value.  There are also a very limited number of
     precisions that we support, so it's really a waste of time.  */
  switch (TYPE_PRECISION (type))
    {
    case 8:	idx = 0; break;
    case 16:	idx = 2; break;
    case 32:	idx = 4; break;
    case 64:	idx = 6; break;
    case 128:	idx = 8; break;
    default:    gcc_unreachable ();
    }

  value = values[idx + TYPE_UNSIGNED (type)];
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
