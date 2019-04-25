/* Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Martin Sebor <msebor@redhat.com>.

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

/* This file implements the printf-return-value pass.  The pass does
   two things: 1) it analyzes calls to formatted output functions like
   sprintf looking for possible buffer overflows and calls to bounded
   functions like snprintf for early truncation (and under the control
   of the -Wformat-length option issues warnings), and 2) under the
   control of the -fprintf-return-value option it folds the return
   value of safe calls into constants, making it possible to eliminate
   code that depends on the value of those constants.

   For all functions (bounded or not) the pass uses the size of the
   destination object.  That means that it will diagnose calls to
   snprintf not on the basis of the size specified by the function's
   second argument but rathger on the basis of the size the first
   argument points to (if possible).  For bound-checking built-ins
   like __builtin___snprintf_chk the pass uses the size typically
   determined by __builtin_object_size and passed to the built-in
   by the Glibc inline wrapper.

   The pass handles all forms standard sprintf format directives,
   including character, integer, floating point, pointer, and strings,
   with the standard C flags, widths, and precisions.  For integers
   and strings it computes the length of output itself.  For floating
   point it uses MPFR to fornmat known constants with up and down
   rounding and uses the resulting range of output lengths.  For
   strings it uses the length of string literals and the sizes of
   character arrays that a character pointer may point to as a bound
   on the longest string.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-fold.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa.h"
#include "tree-object-size.h"
#include "params.h"
#include "tree-cfg.h"
#include "tree-ssa-propagate.h"
#include "calls.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop.h"
#include "intl.h"
#include "langhooks.h"

#include "attribs.h"
#include "builtins.h"
#include "stor-layout.h"

#include "realmpfr.h"
#include "target.h"

#include "cpplib.h"
#include "input.h"
#include "toplev.h"
#include "substring-locations.h"
#include "diagnostic.h"
#include "domwalk.h"
#include "alloc-pool.h"
#include "vr-values.h"
#include "gimple-ssa-evrp-analyze.h"

/* The likely worst case value of MB_LEN_MAX for the target, large enough
   for UTF-8.  Ideally, this would be obtained by a target hook if it were
   to be used for optimization but it's good enough as is for warnings.  */
#define target_mb_len_max()   6

/* The maximum number of bytes a single non-string directive can result
   in.  This is the result of printf("%.*Lf", INT_MAX, -LDBL_MAX) for
   LDBL_MAX_10_EXP of 4932.  */
#define IEEE_MAX_10_EXP    4932
#define target_dir_max()   (target_int_max () + IEEE_MAX_10_EXP + 2)

namespace {

const pass_data pass_data_sprintf_length = {
  GIMPLE_PASS,             // pass type
  "printf-return-value",   // pass name
  OPTGROUP_NONE,           // optinfo_flags
  TV_NONE,                 // tv_id
  PROP_cfg,                // properties_required
  0,	                   // properties_provided
  0,	                   // properties_destroyed
  0,	                   // properties_start
  0,	                   // properties_finish
};

/* Set to the warning level for the current function which is equal
   either to warn_format_trunc for bounded functions or to
   warn_format_overflow otherwise.  */

static int warn_level;

struct format_result;

class sprintf_dom_walker : public dom_walker
{
 public:
  sprintf_dom_walker ()
    : dom_walker (CDI_DOMINATORS),
      evrp_range_analyzer (false) {}
  ~sprintf_dom_walker () {}

  edge before_dom_children (basic_block) FINAL OVERRIDE;
  void after_dom_children (basic_block) FINAL OVERRIDE;
  bool handle_gimple_call (gimple_stmt_iterator *);

  struct call_info;
  bool compute_format_length (call_info &, format_result *);
  class evrp_range_analyzer evrp_range_analyzer;
};

class pass_sprintf_length : public gimple_opt_pass
{
  bool fold_return_value;

public:
  pass_sprintf_length (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sprintf_length, ctxt),
    fold_return_value (false)
  { }

  opt_pass * clone () { return new pass_sprintf_length (m_ctxt); }

  virtual bool gate (function *);

  virtual unsigned int execute (function *);

  void set_pass_param (unsigned int n, bool param)
    {
      gcc_assert (n == 0);
      fold_return_value = param;
    }

};

bool
pass_sprintf_length::gate (function *)
{
  /* Run the pass iff -Warn-format-overflow or -Warn-format-truncation
     is specified and either not optimizing and the pass is being invoked
     early, or when optimizing and the pass is being invoked during
     optimization (i.e., "late").  */
  return ((warn_format_overflow > 0
	   || warn_format_trunc > 0
	   || flag_printf_return_value)
	  && (optimize > 0) == fold_return_value);
}

/* The minimum, maximum, likely, and unlikely maximum number of bytes
   of output either a formatting function or an individual directive
   can result in.  */

struct result_range
{
  /* The absolute minimum number of bytes.  The result of a successful
     conversion is guaranteed to be no less than this.  (An erroneous
     conversion can be indicated by MIN > HOST_WIDE_INT_MAX.)  */
  unsigned HOST_WIDE_INT min;
  /* The likely maximum result that is used in diagnostics.  In most
     cases MAX is the same as the worst case UNLIKELY result.  */
  unsigned HOST_WIDE_INT max;
  /* The likely result used to trigger diagnostics.  For conversions
     that result in a range of bytes [MIN, MAX], LIKELY is somewhere
     in that range.  */
  unsigned HOST_WIDE_INT likely;
  /* In rare cases (e.g., for nultibyte characters) UNLIKELY gives
     the worst cases maximum result of a directive.  In most cases
     UNLIKELY == MAX.  UNLIKELY is used to control the return value
     optimization but not in diagnostics.  */
  unsigned HOST_WIDE_INT unlikely;
};

/* The result of a call to a formatted function.  */

struct format_result
{
  /* Range of characters written by the formatted function.
     Setting the minimum to HOST_WIDE_INT_MAX disables all
     length tracking for the remainder of the format string.  */
  result_range range;

  /* True when the range above is obtained from known values of
     directive arguments, or bounds on the amount of output such
     as width and precision, and not the result of  heuristics that
     depend on warning levels.  It's used to issue stricter diagnostics
     in cases where strings of unknown lengths are bounded by the arrays
     they are determined to refer to.  KNOWNRANGE must not be used for
     the return value optimization.  */
  bool knownrange;

  /* True if no individual directive could fail or result in more than
     4095 bytes of output (the total NUMBER_CHARS_{MIN,MAX} might be
     greater).  Implementations are not required to handle directives
     that produce more than 4K bytes (leading to undefined behavior)
     and so when one is found it disables the return value optimization.
     Similarly, directives that can fail (such as wide character
     directives) disable the optimization.  */
  bool posunder4k;

  /* True when a floating point directive has been seen in the format
     string.  */
  bool floating;

  /* True when an intermediate result has caused a warning.  Used to
     avoid issuing duplicate warnings while finishing the processing
     of a call.  WARNED also disables the return value optimization.  */
  bool warned;

  /* Preincrement the number of output characters by 1.  */
  format_result& operator++ ()
  {
    return *this += 1;
  }

  /* Postincrement the number of output characters by 1.  */
  format_result operator++ (int)
  {
    format_result prev (*this);
    *this += 1;
    return prev;
  }

  /* Increment the number of output characters by N.  */
  format_result& operator+= (unsigned HOST_WIDE_INT);
};

format_result&
format_result::operator+= (unsigned HOST_WIDE_INT n)
{
  gcc_assert (n < HOST_WIDE_INT_MAX);

  if (range.min < HOST_WIDE_INT_MAX)
    range.min += n;

  if (range.max < HOST_WIDE_INT_MAX)
    range.max += n;

  if (range.likely < HOST_WIDE_INT_MAX)
    range.likely += n;

  if (range.unlikely < HOST_WIDE_INT_MAX)
    range.unlikely += n;

  return *this;
}

/* Return the value of INT_MIN for the target.  */

static inline HOST_WIDE_INT
target_int_min ()
{
  return tree_to_shwi (TYPE_MIN_VALUE (integer_type_node));
}

/* Return the value of INT_MAX for the target.  */

static inline unsigned HOST_WIDE_INT
target_int_max ()
{
  return tree_to_uhwi (TYPE_MAX_VALUE (integer_type_node));
}

/* Return the value of SIZE_MAX for the target.  */

static inline unsigned HOST_WIDE_INT
target_size_max ()
{
  return tree_to_uhwi (TYPE_MAX_VALUE (size_type_node));
}

/* A straightforward mapping from the execution character set to the host
   character set indexed by execution character.  */

static char target_to_host_charmap[256];

/* Initialize a mapping from the execution character set to the host
   character set.  */

static bool
init_target_to_host_charmap ()
{
  /* If the percent sign is non-zero the mapping has already been
     initialized.  */
  if (target_to_host_charmap['%'])
    return true;

  /* Initialize the target_percent character (done elsewhere).  */
  if (!init_target_chars ())
    return false;

  /* The subset of the source character set used by printf conversion
     specifications (strictly speaking, not all letters are used but
     they are included here for the sake of simplicity).  The dollar
     sign must be included even though it's not in the basic source
     character set.  */
  const char srcset[] = " 0123456789!\"#%&'()*+,-./:;<=>?[\\]^_{|}~$"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

  /* Set the mapping for all characters to some ordinary value (i,e.,
     not none used in printf conversion specifications) and overwrite
     those that are used by conversion specifications with their
     corresponding values.  */
  memset (target_to_host_charmap + 1, '?', sizeof target_to_host_charmap - 1);

  /* Are the two sets of characters the same?  */
  bool all_same_p = true;

  for (const char *pc = srcset; *pc; ++pc)
    {
      /* Slice off the high end bits in case target characters are
	 signed.  All values are expected to be non-nul, otherwise
	 there's a problem.  */
      if (unsigned char tc = lang_hooks.to_target_charset (*pc))
	{
	  target_to_host_charmap[tc] = *pc;
	  if (tc != *pc)
	    all_same_p = false;
	}
      else
	return false;

    }

  /* Set the first element to a non-zero value if the mapping
     is 1-to-1, otherwise leave it clear (NUL is assumed to be
     the same in both character sets).  */
  target_to_host_charmap[0] = all_same_p;

  return true;
}

/* Return the host source character corresponding to the character
   CH in the execution character set if one exists, or some innocuous
   (non-special, non-nul) source character otherwise.  */

static inline unsigned char
target_to_host (unsigned char ch)
{
  return target_to_host_charmap[ch];
}

/* Convert an initial substring of the string TARGSTR consisting of
   characters in the execution character set into a string in the
   source character set on the host and store up to HOSTSZ characters
   in the buffer pointed to by HOSTR.  Return HOSTR.  */

static const char*
target_to_host (char *hostr, size_t hostsz, const char *targstr)
{
  /* Make sure the buffer is reasonably big.  */
  gcc_assert (hostsz > 4);

  /* The interesting subset of source and execution characters are
     the same so no conversion is necessary.  However, truncate
     overlong strings just like the translated strings are.  */
  if (target_to_host_charmap['\0'] == 1)
    {
      size_t len = strlen (targstr);
      if (len >= hostsz)
	{
	  memcpy (hostr, targstr, hostsz - 4);
	  strcpy (hostr + hostsz - 4, "...");
	}
      else
	memcpy (hostr, targstr, len + 1);
      return hostr;
    }

  /* Convert the initial substring of TARGSTR to the corresponding
     characters in the host set, appending "..." if TARGSTR is too
     long to fit.  Using the static buffer assumes the function is
     not called in between sequence points (which it isn't).  */
  for (char *ph = hostr; ; ++targstr)
    {
      *ph++ = target_to_host (*targstr);
      if (!*targstr)
	break;

      if (size_t (ph - hostr) == hostsz)
	{
	  strcpy (ph - 4, "...");
	  break;
	}
    }

  return hostr;
}

/* Convert the sequence of decimal digits in the execution character
   starting at *PS to a HOST_WIDE_INT, analogously to strtol.  Return
   the result and set *PS to one past the last converted character.
   On range error set ERANGE to the digit that caused it.  */

static inline HOST_WIDE_INT
target_strtowi (const char **ps, const char **erange)
{
  unsigned HOST_WIDE_INT val = 0;
  for ( ; ; ++*ps)
    {
      unsigned char c = target_to_host (**ps);
      if (ISDIGIT (c))
	{
	  c -= '0';

	  /* Check for overflow.  */
	  if (val > ((unsigned HOST_WIDE_INT) HOST_WIDE_INT_MAX - c) / 10LU)
	    {
	      val = HOST_WIDE_INT_MAX;
	      *erange = *ps;

	      /* Skip the remaining digits.  */
	      do
		c = target_to_host (*++*ps);
	      while (ISDIGIT (c));
	      break;
	    }
	  else
	    val = val * 10 + c;
	}
      else
	break;
    }

  return val;
}

/* Given FORMAT, set *PLOC to the source location of the format string
   and return the format string if it is known or null otherwise.  */

static const char*
get_format_string (tree format, location_t *ploc)
{
  *ploc = EXPR_LOC_OR_LOC (format, input_location);

  return c_getstr (format);
}

/* For convenience and brevity, shorter named entrypoints of
   format_string_diagnostic_t::emit_warning_va and
   format_string_diagnostic_t::emit_warning_n_va.
   These have to be functions with the attribute so that exgettext
   works properly.  */

static bool
ATTRIBUTE_GCC_DIAG (5, 6)
fmtwarn (const substring_loc &fmt_loc, location_t param_loc,
	 const char *corrected_substring, int opt, const char *gmsgid, ...)
{
  format_string_diagnostic_t diag (fmt_loc, NULL, param_loc, NULL,
				   corrected_substring);
  va_list ap;
  va_start (ap, gmsgid);
  bool warned = diag.emit_warning_va (opt, gmsgid, &ap);
  va_end (ap);

  return warned;
}

static bool
ATTRIBUTE_GCC_DIAG (6, 8) ATTRIBUTE_GCC_DIAG (7, 8)
fmtwarn_n (const substring_loc &fmt_loc, location_t param_loc,
	   const char *corrected_substring, int opt, unsigned HOST_WIDE_INT n,
	   const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  format_string_diagnostic_t diag (fmt_loc, NULL, param_loc, NULL,
				   corrected_substring);
  va_list ap;
  va_start (ap, plural_gmsgid);
  bool warned = diag.emit_warning_n_va (opt, n, singular_gmsgid, plural_gmsgid,
					&ap);
  va_end (ap);

  return warned;
}

/* Format length modifiers.  */

enum format_lengths
{
  FMT_LEN_none,
  FMT_LEN_hh,    // char argument
  FMT_LEN_h,     // short
  FMT_LEN_l,     // long
  FMT_LEN_ll,    // long long
  FMT_LEN_L,     // long double (and GNU long long)
  FMT_LEN_z,     // size_t
  FMT_LEN_t,     // ptrdiff_t
  FMT_LEN_j      // intmax_t
};


/* Description of the result of conversion either of a single directive
   or the whole format string.  */

struct fmtresult
{
  /* Construct a FMTRESULT object with all counters initialized
     to MIN.  KNOWNRANGE is set when MIN is valid.  */
  fmtresult (unsigned HOST_WIDE_INT min = HOST_WIDE_INT_MAX)
  : argmin (), argmax (), nonstr (),
    knownrange (min < HOST_WIDE_INT_MAX),
    mayfail (), nullp ()
  {
    range.min = min;
    range.max = min;
    range.likely = min;
    range.unlikely = min;
  }

  /* Construct a FMTRESULT object with MIN, MAX, and LIKELY counters.
     KNOWNRANGE is set when both MIN and MAX are valid.   */
  fmtresult (unsigned HOST_WIDE_INT min, unsigned HOST_WIDE_INT max,
	     unsigned HOST_WIDE_INT likely = HOST_WIDE_INT_MAX)
  : argmin (), argmax (), nonstr (),
    knownrange (min < HOST_WIDE_INT_MAX && max < HOST_WIDE_INT_MAX),
    mayfail (), nullp ()
  {
    range.min = min;
    range.max = max;
    range.likely = max < likely ? min : likely;
    range.unlikely = max;
  }

  /* Adjust result upward to reflect the RANGE of values the specified
     width or precision is known to be in.  */
  fmtresult& adjust_for_width_or_precision (const HOST_WIDE_INT[2],
					    tree = NULL_TREE,
					    unsigned = 0, unsigned = 0);

  /* Return the maximum number of decimal digits a value of TYPE
     formats as on output.  */
  static unsigned type_max_digits (tree, int);

  /* The range a directive's argument is in.  */
  tree argmin, argmax;

  /* The minimum and maximum number of bytes that a directive
     results in on output for an argument in the range above.  */
  result_range range;

  /* Non-nul when the argument of a string directive is not a nul
     terminated string.  */
  tree nonstr;

  /* True when the range above is obtained from a known value of
     a directive's argument or its bounds and not the result of
     heuristics that depend on warning levels.  */
  bool knownrange;

  /* True for a directive that may fail (such as wide character
     directives).  */
  bool mayfail;

  /* True when the argument is a null pointer.  */
  bool nullp;
};

/* Adjust result upward to reflect the range ADJUST of values the
   specified width or precision is known to be in.  When non-null,
   TYPE denotes the type of the directive whose result is being
   adjusted, BASE gives the base of the directive (octal, decimal,
   or hex), and ADJ denotes the additional adjustment to the LIKELY
   counter that may need to be added when ADJUST is a range.  */

fmtresult&
fmtresult::adjust_for_width_or_precision (const HOST_WIDE_INT adjust[2],
					  tree type /* = NULL_TREE */,
					  unsigned base /* = 0 */,
					  unsigned adj /* = 0 */)
{
  bool minadjusted = false;

  /* Adjust the minimum and likely counters.  */
  if (adjust[0] >= 0)
    {
      if (range.min < (unsigned HOST_WIDE_INT)adjust[0])
	{
	  range.min = adjust[0];
	  minadjusted = true;
	}

      /* Adjust the likely counter.  */
      if (range.likely < range.min)
	range.likely = range.min;
    }
  else if (adjust[0] == target_int_min ()
	   && (unsigned HOST_WIDE_INT)adjust[1] == target_int_max ())
    knownrange = false;

  /* Adjust the maximum counter.  */
  if (adjust[1] > 0)
    {
      if (range.max < (unsigned HOST_WIDE_INT)adjust[1])
	{
	  range.max = adjust[1];

	  /* Set KNOWNRANGE if both the minimum and maximum have been
	     adjusted.  Otherwise leave it at what it was before.  */
	  knownrange = minadjusted;
	}
    }

  if (warn_level > 1 && type)
    {
      /* For large non-constant width or precision whose range spans
	 the maximum number of digits produced by the directive for
	 any argument, set the likely number of bytes to be at most
	 the number digits plus other adjustment determined by the
	 caller (one for sign or two for the hexadecimal "0x"
	 prefix).  */
      unsigned dirdigs = type_max_digits (type, base);
      if (adjust[0] < dirdigs && dirdigs < adjust[1]
	  && range.likely < dirdigs)
	range.likely = dirdigs + adj;
    }
  else if (range.likely < (range.min ? range.min : 1))
    {
      /* Conservatively, set LIKELY to at least MIN but no less than
	 1 unless MAX is zero.  */
      range.likely = (range.min
		      ? range.min
		      : range.max && (range.max < HOST_WIDE_INT_MAX
				      || warn_level > 1) ? 1 : 0);
    }

  /* Finally adjust the unlikely counter to be at least as large as
     the maximum.  */
  if (range.unlikely < range.max)
    range.unlikely = range.max;

  return *this;
}

/* Return the maximum number of digits a value of TYPE formats in
   BASE on output, not counting base prefix .  */

unsigned
fmtresult::type_max_digits (tree type, int base)
{
  unsigned prec = TYPE_PRECISION (type);
  switch (base)
    {
    case 8:
      return (prec + 2) / 3;
    case 10:
      /* Decimal approximation: yields 3, 5, 10, and 20 for precision
	 of 8, 16, 32, and 64 bits.  */
      return prec * 301 / 1000 + 1;
    case 16:
      return prec / 4;
    }

  gcc_unreachable ();
}

static bool
get_int_range (tree, HOST_WIDE_INT *, HOST_WIDE_INT *, bool, HOST_WIDE_INT,
	       class vr_values *vr_values);

/* Description of a format directive.  A directive is either a plain
   string or a conversion specification that starts with '%'.  */

struct directive
{
  /* The 1-based directive number (for debugging).  */
  unsigned dirno;

  /* The first character of the directive and its length.  */
  const char *beg;
  size_t len;

  /* A bitmap of flags, one for each character.  */
  unsigned flags[256 / sizeof (int)];

  /* The range of values of the specified width, or -1 if not specified.  */
  HOST_WIDE_INT width[2];
  /* The range of values of the specified precision, or -1 if not
     specified.  */
  HOST_WIDE_INT prec[2];

  /* Length modifier.  */
  format_lengths modifier;

  /* Format specifier character.  */
  char specifier;

  /* The argument of the directive or null when the directive doesn't
     take one or when none is available (such as for vararg functions).  */
  tree arg;

  /* Format conversion function that given a directive and an argument
     returns the formatting result.  */
  fmtresult (*fmtfunc) (const directive &, tree, vr_values *);

  /* Return True when a the format flag CHR has been used.  */
  bool get_flag (char chr) const
  {
    unsigned char c = chr & 0xff;
    return (flags[c / (CHAR_BIT * sizeof *flags)]
	    & (1U << (c % (CHAR_BIT * sizeof *flags))));
  }

  /* Make a record of the format flag CHR having been used.  */
  void set_flag (char chr)
  {
    unsigned char c = chr & 0xff;
    flags[c / (CHAR_BIT * sizeof *flags)]
      |= (1U << (c % (CHAR_BIT * sizeof *flags)));
  }

  /* Reset the format flag CHR.  */
  void clear_flag (char chr)
  {
    unsigned char c = chr & 0xff;
    flags[c / (CHAR_BIT * sizeof *flags)]
      &= ~(1U << (c % (CHAR_BIT * sizeof *flags)));
  }

  /* Set both bounds of the width range to VAL.  */
  void set_width (HOST_WIDE_INT val)
  {
    width[0] = width[1] = val;
  }

  /* Set the width range according to ARG, with both bounds being
     no less than 0.  For a constant ARG set both bounds to its value
     or 0, whichever is greater.  For a non-constant ARG in some range
     set width to its range adjusting each bound to -1 if it's less.
     For an indeterminate ARG set width to [0, INT_MAX].  */
  void set_width (tree arg, vr_values *vr_values)
  {
    get_int_range (arg, width, width + 1, true, 0, vr_values);
  }

  /* Set both bounds of the precision range to VAL.  */
  void set_precision (HOST_WIDE_INT val)
  {
    prec[0] = prec[1] = val;
  }

  /* Set the precision range according to ARG, with both bounds being
     no less than -1.  For a constant ARG set both bounds to its value
     or -1 whichever is greater.  For a non-constant ARG in some range
     set precision to its range adjusting each bound to -1 if it's less.
     For an indeterminate ARG set precision to [-1, INT_MAX].  */
  void set_precision (tree arg, vr_values *vr_values)
  {
    get_int_range (arg, prec, prec + 1, false, -1, vr_values);
  }

  /* Return true if both width and precision are known to be
     either constant or in some range, false otherwise.  */
  bool known_width_and_precision () const
  {
    return ((width[1] < 0
	     || (unsigned HOST_WIDE_INT)width[1] <= target_int_max ())
	    && (prec[1] < 0
		|| (unsigned HOST_WIDE_INT)prec[1] < target_int_max ()));
  }
};

/* Return the logarithm of X in BASE.  */

static int
ilog (unsigned HOST_WIDE_INT x, int base)
{
  int res = 0;
  do
    {
      ++res;
      x /= base;
    } while (x);
  return res;
}

/* Return the number of bytes resulting from converting into a string
   the INTEGER_CST tree node X in BASE with a minimum of PREC digits.
   PLUS indicates whether 1 for a plus sign should be added for positive
   numbers, and PREFIX whether the length of an octal ('O') or hexadecimal
   ('0x') prefix should be added for nonzero numbers.  Return -1 if X cannot
   be represented.  */

static HOST_WIDE_INT
tree_digits (tree x, int base, HOST_WIDE_INT prec, bool plus, bool prefix)
{
  unsigned HOST_WIDE_INT absval;

  HOST_WIDE_INT res;

  if (TYPE_UNSIGNED (TREE_TYPE (x)))
    {
      if (tree_fits_uhwi_p (x))
	{
	  absval = tree_to_uhwi (x);
	  res = plus;
	}
      else
	return -1;
    }
  else
    {
      if (tree_fits_shwi_p (x))
	{
	  HOST_WIDE_INT i = tree_to_shwi (x);
         if (HOST_WIDE_INT_MIN == i)
           {
             /* Avoid undefined behavior due to negating a minimum.  */
             absval = HOST_WIDE_INT_MAX;
             res = 1;
           }
         else if (i < 0)
	   {
	     absval = -i;
	     res = 1;
	   }
	 else
	   {
	     absval = i;
	     res = plus;
	   }
	}
      else
	return -1;
    }

  int ndigs = ilog (absval, base);

  res += prec < ndigs ? ndigs : prec;

  /* Adjust a non-zero value for the base prefix, either hexadecimal,
     or, unless precision has resulted in a leading zero, also octal.  */
  if (prefix && absval && (base == 16 || prec <= ndigs))
    {
      if (base == 8)
	res += 1;
      else if (base == 16)
	res += 2;
    }

  return res;
}

/* Given the formatting result described by RES and NAVAIL, the number
   of available in the destination, return the range of bytes remaining
   in the destination.  */

static inline result_range
bytes_remaining (unsigned HOST_WIDE_INT navail, const format_result &res)
{
  result_range range;

  if (HOST_WIDE_INT_MAX <= navail)
    {
      range.min = range.max = range.likely = range.unlikely = navail;
      return range;
    }

  /* The lower bound of the available range is the available size
     minus the maximum output size, and the upper bound is the size
     minus the minimum.  */
  range.max = res.range.min < navail ? navail - res.range.min : 0;

  range.likely = res.range.likely < navail ? navail - res.range.likely : 0;

  if (res.range.max < HOST_WIDE_INT_MAX)
    range.min = res.range.max < navail ? navail - res.range.max : 0;
  else
    range.min = range.likely;

  range.unlikely = (res.range.unlikely < navail
		    ? navail - res.range.unlikely : 0);

  return range;
}

/* Description of a call to a formatted function.  */

struct sprintf_dom_walker::call_info
{
  /* Function call statement.  */
  gimple *callstmt;

  /* Function called.  */
  tree func;

  /* Called built-in function code.  */
  built_in_function fncode;

  /* Format argument and format string extracted from it.  */
  tree format;
  const char *fmtstr;

  /* The location of the format argument.  */
  location_t fmtloc;

  /* The destination object size for __builtin___xxx_chk functions
     typically determined by __builtin_object_size, or -1 if unknown.  */
  unsigned HOST_WIDE_INT objsize;

  /* Number of the first variable argument.  */
  unsigned HOST_WIDE_INT argidx;

  /* True for functions like snprintf that specify the size of
     the destination, false for others like sprintf that don't.  */
  bool bounded;

  /* True for bounded functions like snprintf that specify a zero-size
     buffer as a request to compute the size of output without actually
     writing any.  NOWRITE is cleared in response to the %n directive
     which has side-effects similar to writing output.  */
  bool nowrite;

  /* Return true if the called function's return value is used.  */
  bool retval_used () const
  {
    return gimple_get_lhs (callstmt);
  }

  /* Return the warning option corresponding to the called function.  */
  int warnopt () const
  {
    return bounded ? OPT_Wformat_truncation_ : OPT_Wformat_overflow_;
  }

  /* Return true for calls to file formatted functions.  */
  bool is_file_func () const
  {
    return (fncode == BUILT_IN_FPRINTF
	    || fncode == BUILT_IN_FPRINTF_CHK
	    || fncode == BUILT_IN_FPRINTF_UNLOCKED
	    || fncode == BUILT_IN_VFPRINTF
	    || fncode == BUILT_IN_VFPRINTF_CHK);
  }

  /* Return true for calls to string formatted functions.  */
  bool is_string_func () const
  {
    return (fncode == BUILT_IN_SPRINTF
	    || fncode == BUILT_IN_SPRINTF_CHK
	    || fncode == BUILT_IN_SNPRINTF
	    || fncode == BUILT_IN_SNPRINTF_CHK
	    || fncode == BUILT_IN_VSPRINTF
	    || fncode == BUILT_IN_VSPRINTF_CHK
	    || fncode == BUILT_IN_VSNPRINTF
	    || fncode == BUILT_IN_VSNPRINTF_CHK);
  }
};

/* Return the result of formatting a no-op directive (such as '%n').  */

static fmtresult
format_none (const directive &, tree, vr_values *)
{
  fmtresult res (0);
  return res;
}

/* Return the result of formatting the '%%' directive.  */

static fmtresult
format_percent (const directive &, tree, vr_values *)
{
  fmtresult res (1);
  return res;
}


/* Compute intmax_type_node and uintmax_type_node similarly to how
   tree.c builds size_type_node.  */

static void
build_intmax_type_nodes (tree *pintmax, tree *puintmax)
{
  if (strcmp (UINTMAX_TYPE, "unsigned int") == 0)
    {
      *pintmax = integer_type_node;
      *puintmax = unsigned_type_node;
    }
  else if (strcmp (UINTMAX_TYPE, "long unsigned int") == 0)
    {
      *pintmax = long_integer_type_node;
      *puintmax = long_unsigned_type_node;
    }
  else if (strcmp (UINTMAX_TYPE, "long long unsigned int") == 0)
    {
      *pintmax = long_long_integer_type_node;
      *puintmax = long_long_unsigned_type_node;
    }
  else
    {
      for (int i = 0; i < NUM_INT_N_ENTS; i++)
	if (int_n_enabled_p[i])
	  {
	    char name[50];
	    sprintf (name, "__int%d unsigned", int_n_data[i].bitsize);

	    if (strcmp (name, UINTMAX_TYPE) == 0)
	      {
	        *pintmax = int_n_trees[i].signed_type;
	        *puintmax = int_n_trees[i].unsigned_type;
		return;
	      }
	  }
      gcc_unreachable ();
    }
}

/* Determine the range [*PMIN, *PMAX] that the expression ARG is
   in and that is representable in type int.
   Return true when the range is a subrange of that of int.
   When ARG is null it is as if it had the full range of int.
   When ABSOLUTE is true the range reflects the absolute value of
   the argument.  When ABSOLUTE is false, negative bounds of
   the determined range are replaced with NEGBOUND.  */

static bool
get_int_range (tree arg, HOST_WIDE_INT *pmin, HOST_WIDE_INT *pmax,
	       bool absolute, HOST_WIDE_INT negbound,
	       class vr_values *vr_values)
{
  /* The type of the result.  */
  const_tree type = integer_type_node;

  bool knownrange = false;

  if (!arg)
    {
      *pmin = tree_to_shwi (TYPE_MIN_VALUE (type));
      *pmax = tree_to_shwi (TYPE_MAX_VALUE (type));
    }
  else if (TREE_CODE (arg) == INTEGER_CST
	   && TYPE_PRECISION (TREE_TYPE (arg)) <= TYPE_PRECISION (type))
    {
      /* For a constant argument return its value adjusted as specified
	 by NEGATIVE and NEGBOUND and return true to indicate that the
	 result is known.  */
      *pmin = tree_fits_shwi_p (arg) ? tree_to_shwi (arg) : tree_to_uhwi (arg);
      *pmax = *pmin;
      knownrange = true;
    }
  else
    {
      /* True if the argument's range cannot be determined.  */
      bool unknown = true;

      tree argtype = TREE_TYPE (arg);

      /* Ignore invalid arguments with greater precision that that
	 of the expected type (e.g., in sprintf("%*i", 12LL, i)).
	 They will have been detected and diagnosed by -Wformat and
	 so it's not important to complicate this code to try to deal
	 with them again.  */
      if (TREE_CODE (arg) == SSA_NAME
	  && INTEGRAL_TYPE_P (argtype)
	  && TYPE_PRECISION (argtype) <= TYPE_PRECISION (type))
	{
	  /* Try to determine the range of values of the integer argument.  */
	  value_range *vr = vr_values->get_value_range (arg);
	  if (range_int_cst_p (vr))
	    {
	      HOST_WIDE_INT type_min
		= (TYPE_UNSIGNED (argtype)
		   ? tree_to_uhwi (TYPE_MIN_VALUE (argtype))
		   : tree_to_shwi (TYPE_MIN_VALUE (argtype)));

	      HOST_WIDE_INT type_max = tree_to_uhwi (TYPE_MAX_VALUE (argtype));

	      *pmin = TREE_INT_CST_LOW (vr->min ());
	      *pmax = TREE_INT_CST_LOW (vr->max ());

	      if (*pmin < *pmax)
		{
		  /* Return true if the adjusted range is a subrange of
		     the full range of the argument's type.  *PMAX may
		     be less than *PMIN when the argument is unsigned
		     and its upper bound is in excess of TYPE_MAX.  In
		     that (invalid) case disregard the range and use that
		     of the expected type instead.  */
		  knownrange = type_min < *pmin || *pmax < type_max;

		  unknown = false;
		}
	    }
	}

      /* Handle an argument with an unknown range as if none had been
	 provided.  */
      if (unknown)
	return get_int_range (NULL_TREE, pmin, pmax, absolute,
			      negbound, vr_values);
    }

  /* Adjust each bound as specified by ABSOLUTE and NEGBOUND.  */
  if (absolute)
    {
      if (*pmin < 0)
	{
	  if (*pmin == *pmax)
	    *pmin = *pmax = -*pmin;
	  else
	    {
	      /* Make sure signed overlow is avoided.  */
	      gcc_assert (*pmin != HOST_WIDE_INT_MIN);

	      HOST_WIDE_INT tmp = -*pmin;
	      *pmin = 0;
	      if (*pmax < tmp)
		*pmax = tmp;
	    }
	}
    }
  else if (*pmin < negbound)
    *pmin = negbound;

  return knownrange;
}

/* With the range [*ARGMIN, *ARGMAX] of an integer directive's actual
   argument, due to the conversion from either *ARGMIN or *ARGMAX to
   the type of the directive's formal argument it's possible for both
   to result in the same number of bytes or a range of bytes that's
   less than the number of bytes that would result from formatting
   some other value in the range [*ARGMIN, *ARGMAX].  This can be
   determined by checking for the actual argument being in the range
   of the type of the directive.  If it isn't it must be assumed to
   take on the full range of the directive's type.
   Return true when the range has been adjusted to the full range
   of DIRTYPE, and false otherwise.  */

static bool
adjust_range_for_overflow (tree dirtype, tree *argmin, tree *argmax)
{
  tree argtype = TREE_TYPE (*argmin);
  unsigned argprec = TYPE_PRECISION (argtype);
  unsigned dirprec = TYPE_PRECISION (dirtype);

  /* If the actual argument and the directive's argument have the same
     precision and sign there can be no overflow and so there is nothing
     to adjust.  */
  if (argprec == dirprec && TYPE_SIGN (argtype) == TYPE_SIGN (dirtype))
    return false;

  /* The logic below was inspired/lifted from the CONVERT_EXPR_CODE_P
     branch in the extract_range_from_unary_expr function in tree-vrp.c.  */

  if (TREE_CODE (*argmin) == INTEGER_CST
      && TREE_CODE (*argmax) == INTEGER_CST
      && (dirprec >= argprec
	  || integer_zerop (int_const_binop (RSHIFT_EXPR,
					     int_const_binop (MINUS_EXPR,
							      *argmax,
							      *argmin),
					     size_int (dirprec)))))
    {
      *argmin = force_fit_type (dirtype, wi::to_widest (*argmin), 0, false);
      *argmax = force_fit_type (dirtype, wi::to_widest (*argmax), 0, false);

      /* If *ARGMIN is still less than *ARGMAX the conversion above
	 is safe.  Otherwise, it has overflowed and would be unsafe.  */
      if (tree_int_cst_le (*argmin, *argmax))
	return false;
    }

  *argmin = TYPE_MIN_VALUE (dirtype);
  *argmax = TYPE_MAX_VALUE (dirtype);
  return true;
}

/* Return a range representing the minimum and maximum number of bytes
   that the format directive DIR will output for any argument given
   the WIDTH and PRECISION (extracted from DIR).  This function is
   used when the directive argument or its value isn't known.  */

static fmtresult
format_integer (const directive &dir, tree arg, vr_values *vr_values)
{
  tree intmax_type_node;
  tree uintmax_type_node;

  /* Base to format the number in.  */
  int base;

  /* True when a conversion is preceded by a prefix indicating the base
     of the argument (octal or hexadecimal).  */
  bool maybebase = dir.get_flag ('#');

  /* True when a signed conversion is preceded by a sign or space.  */
  bool maybesign = false;

  /* True for signed conversions (i.e., 'd' and 'i').  */
  bool sign = false;

  switch (dir.specifier)
    {
    case 'd':
    case 'i':
      /* Space and '+' are  only meaningful for signed conversions.  */
      maybesign = dir.get_flag (' ') | dir.get_flag ('+');
      sign = true;
      base = 10;
      break;
    case 'u':
      base = 10;
      break;
    case 'o':
      base = 8;
      break;
    case 'X':
    case 'x':
      base = 16;
      break;
    default:
      gcc_unreachable ();
    }

  /* The type of the "formal" argument expected by the directive.  */
  tree dirtype = NULL_TREE;

  /* Determine the expected type of the argument from the length
     modifier.  */
  switch (dir.modifier)
    {
    case FMT_LEN_none:
      if (dir.specifier == 'p')
	dirtype = ptr_type_node;
      else
	dirtype = sign ? integer_type_node : unsigned_type_node;
      break;

    case FMT_LEN_h:
      dirtype = sign ? short_integer_type_node : short_unsigned_type_node;
      break;

    case FMT_LEN_hh:
      dirtype = sign ? signed_char_type_node : unsigned_char_type_node;
      break;

    case FMT_LEN_l:
      dirtype = sign ? long_integer_type_node : long_unsigned_type_node;
      break;

    case FMT_LEN_L:
    case FMT_LEN_ll:
      dirtype = (sign
		 ? long_long_integer_type_node
		 : long_long_unsigned_type_node);
      break;

    case FMT_LEN_z:
      dirtype = signed_or_unsigned_type_for (!sign, size_type_node);
      break;

    case FMT_LEN_t:
      dirtype = signed_or_unsigned_type_for (!sign, ptrdiff_type_node);
      break;

    case FMT_LEN_j:
      build_intmax_type_nodes (&intmax_type_node, &uintmax_type_node);
      dirtype = sign ? intmax_type_node : uintmax_type_node;
      break;

    default:
      return fmtresult ();
    }

  /* The type of the argument to the directive, either deduced from
     the actual non-constant argument if one is known, or from
     the directive itself when none has been provided because it's
     a va_list.  */
  tree argtype = NULL_TREE;

  if (!arg)
    {
      /* When the argument has not been provided, use the type of
	 the directive's argument as an approximation.  This will
	 result in false positives for directives like %i with
	 arguments with smaller precision (such as short or char).  */
      argtype = dirtype;
    }
  else if (TREE_CODE (arg) == INTEGER_CST)
    {
      /* When a constant argument has been provided use its value
	 rather than type to determine the length of the output.  */
      fmtresult res;

      if ((dir.prec[0] <= 0 && dir.prec[1] >= 0) && integer_zerop (arg))
	{
	  /* As a special case, a precision of zero with a zero argument
	     results in zero bytes except in base 8 when the '#' flag is
	     specified, and for signed conversions in base 8 and 10 when
	     either the space or '+' flag has been specified and it results
	     in just one byte (with width having the normal effect).  This
	     must extend to the case of a specified precision with
	     an unknown value because it can be zero.  */
	  res.range.min = ((base == 8 && dir.get_flag ('#')) || maybesign);
	  if (res.range.min == 0 && dir.prec[0] != dir.prec[1])
	    {
	      res.range.max = 1;
	      res.range.likely = 1;
	    }
	  else
	    {
	      res.range.max = res.range.min;
	      res.range.likely = res.range.min;
	    }
	}
      else
	{
	  /* Convert the argument to the type of the directive.  */
	  arg = fold_convert (dirtype, arg);

	  res.range.min = tree_digits (arg, base, dir.prec[0],
				       maybesign, maybebase);
	  if (dir.prec[0] == dir.prec[1])
	    res.range.max = res.range.min;
	  else
	    res.range.max = tree_digits (arg, base, dir.prec[1],
					 maybesign, maybebase);
	  res.range.likely = res.range.min;
	  res.knownrange = true;
	}

      res.range.unlikely = res.range.max;

      /* Bump up the counters if WIDTH is greater than LEN.  */
      res.adjust_for_width_or_precision (dir.width, dirtype, base,
					 (sign | maybebase) + (base == 16));
      /* Bump up the counters again if PRECision is greater still.  */
      res.adjust_for_width_or_precision (dir.prec, dirtype, base,
					 (sign | maybebase) + (base == 16));

      return res;
    }
  else if (INTEGRAL_TYPE_P (TREE_TYPE (arg))
	   || TREE_CODE (TREE_TYPE (arg)) == POINTER_TYPE)
    /* Determine the type of the provided non-constant argument.  */
    argtype = TREE_TYPE (arg);
  else
    /* Don't bother with invalid arguments since they likely would
       have already been diagnosed, and disable any further checking
       of the format string by returning [-1, -1].  */
    return fmtresult ();

  fmtresult res;

  /* Using either the range the non-constant argument is in, or its
     type (either "formal" or actual), create a range of values that
     constrain the length of output given the warning level.  */
  tree argmin = NULL_TREE;
  tree argmax = NULL_TREE;

  if (arg
      && TREE_CODE (arg) == SSA_NAME
      && INTEGRAL_TYPE_P (argtype))
    {
      /* Try to determine the range of values of the integer argument
	 (range information is not available for pointers).  */
      value_range *vr = vr_values->get_value_range (arg);
      if (range_int_cst_p (vr))
	{
	  argmin = vr->min ();
	  argmax = vr->max ();

	  /* Set KNOWNRANGE if the argument is in a known subrange
	     of the directive's type and neither width nor precision
	     is unknown.  (KNOWNRANGE may be reset below).  */
	  res.knownrange
	    = ((!tree_int_cst_equal (TYPE_MIN_VALUE (dirtype), argmin)
		|| !tree_int_cst_equal (TYPE_MAX_VALUE (dirtype), argmax))
	       && dir.known_width_and_precision ());

	  res.argmin = argmin;
	  res.argmax = argmax;
	}
      else if (vr->kind () == VR_ANTI_RANGE)
	{
	  /* Handle anti-ranges if/when bug 71690 is resolved.  */
	}
      else if (vr->varying_p () || vr->undefined_p ())
	{
	  /* The argument here may be the result of promoting the actual
	     argument to int.  Try to determine the type of the actual
	     argument before promotion and narrow down its range that
	     way.  */
	  gimple *def = SSA_NAME_DEF_STMT (arg);
	  if (is_gimple_assign (def))
	    {
	      tree_code code = gimple_assign_rhs_code (def);
	      if (code == INTEGER_CST)
		{
		  arg = gimple_assign_rhs1 (def);
		  return format_integer (dir, arg, vr_values);
		}

	      if (code == NOP_EXPR)
		{
		  tree type = TREE_TYPE (gimple_assign_rhs1 (def));
		  if (INTEGRAL_TYPE_P (type)
		      || TREE_CODE (type) == POINTER_TYPE)
		    argtype = type;
		}
	    }
	}
    }

  if (!argmin)
    {
      if (TREE_CODE (argtype) == POINTER_TYPE)
	{
	  argmin = build_int_cst (pointer_sized_int_node, 0);
	  argmax = build_all_ones_cst (pointer_sized_int_node);
	}
      else
	{
	  argmin = TYPE_MIN_VALUE (argtype);
	  argmax = TYPE_MAX_VALUE (argtype);
	}
    }

  /* Clear KNOWNRANGE if the range has been adjusted to the maximum
     of the directive.  If it has been cleared then since ARGMIN and/or
     ARGMAX have been adjusted also adjust the corresponding ARGMIN and
     ARGMAX in the result to include in diagnostics.  */
  if (adjust_range_for_overflow (dirtype, &argmin, &argmax))
    {
      res.knownrange = false;
      res.argmin = argmin;
      res.argmax = argmax;
    }

  /* Recursively compute the minimum and maximum from the known range.  */
  if (TYPE_UNSIGNED (dirtype) || tree_int_cst_sgn (argmin) >= 0)
    {
      /* For unsigned conversions/directives or signed when
	 the minimum is positive, use the minimum and maximum to compute
	 the shortest and longest output, respectively.  */
      res.range.min = format_integer (dir, argmin, vr_values).range.min;
      res.range.max = format_integer (dir, argmax, vr_values).range.max;
    }
  else if (tree_int_cst_sgn (argmax) < 0)
    {
      /* For signed conversions/directives if maximum is negative,
	 use the minimum as the longest output and maximum as the
	 shortest output.  */
      res.range.min = format_integer (dir, argmax, vr_values).range.min;
      res.range.max = format_integer (dir, argmin, vr_values).range.max;
    }
  else
    {
      /* Otherwise, 0 is inside of the range and minimum negative.  Use 0
	 as the shortest output and for the longest output compute the
	 length of the output of both minimum and maximum and pick the
	 longer.  */
      unsigned HOST_WIDE_INT max1
	= format_integer (dir, argmin, vr_values).range.max;
      unsigned HOST_WIDE_INT max2
	= format_integer (dir, argmax, vr_values).range.max;
      res.range.min
	= format_integer (dir, integer_zero_node, vr_values).range.min;
      res.range.max = MAX (max1, max2);
    }

  /* If the range is known, use the maximum as the likely length.  */
  if (res.knownrange)
    res.range.likely = res.range.max;
  else
    {
      /* Otherwise, use the minimum.  Except for the case where for %#x or
         %#o the minimum is just for a single value in the range (0) and
         for all other values it is something longer, like 0x1 or 01.
	  Use the length for value 1 in that case instead as the likely
	  length.  */
      res.range.likely = res.range.min;
      if (maybebase
	  && base != 10
	  && (tree_int_cst_sgn (argmin) < 0 || tree_int_cst_sgn (argmax) > 0))
	{
	  if (res.range.min == 1)
	    res.range.likely += base == 8 ? 1 : 2;
	  else if (res.range.min == 2
		   && base == 16
		   && (dir.width[0] == 2 || dir.prec[0] == 2))
	    ++res.range.likely;
	}
    }

  res.range.unlikely = res.range.max;
  res.adjust_for_width_or_precision (dir.width, dirtype, base,
				     (sign | maybebase) + (base == 16));
  res.adjust_for_width_or_precision (dir.prec, dirtype, base,
				     (sign | maybebase) + (base == 16));

  return res;
}

/* Return the number of bytes that a format directive consisting of FLAGS,
   PRECision, format SPECification, and MPFR rounding specifier RNDSPEC,
   would result for argument X under ideal conditions (i.e., if PREC
   weren't excessive).  MPFR 3.1 allocates large amounts of memory for
   values of PREC with large magnitude and can fail (see MPFR bug #21056).
   This function works around those problems.  */

static unsigned HOST_WIDE_INT
get_mpfr_format_length (mpfr_ptr x, const char *flags, HOST_WIDE_INT prec,
			char spec, char rndspec)
{
  char fmtstr[40];

  HOST_WIDE_INT len = strlen (flags);

  fmtstr[0] = '%';
  memcpy (fmtstr + 1, flags, len);
  memcpy (fmtstr + 1 + len, ".*R", 3);
  fmtstr[len + 4] = rndspec;
  fmtstr[len + 5] = spec;
  fmtstr[len + 6] = '\0';

  spec = TOUPPER (spec);
  if (spec == 'E' || spec == 'F')
    {
      /* For %e, specify the precision explicitly since mpfr_sprintf
	 does its own thing just to be different (see MPFR bug 21088).  */
      if (prec < 0)
	prec = 6;
    }
  else
    {
      /* Avoid passing negative precisions with larger magnitude to MPFR
	 to avoid exposing its bugs.  (A negative precision is supposed
	 to be ignored.)  */
      if (prec < 0)
	prec = -1;
    }

  HOST_WIDE_INT p = prec;

  if (spec == 'G' && !strchr (flags, '#'))
    {
      /* For G/g without the pound flag, precision gives the maximum number
	 of significant digits which is bounded by LDBL_MAX_10_EXP, or, for
	 a 128 bit IEEE extended precision, 4932.  Using twice as much here
	 should be more than sufficient for any real format.  */
      if ((IEEE_MAX_10_EXP * 2) < prec)
	prec = IEEE_MAX_10_EXP * 2;
      p = prec;
    }
  else
    {
      /* Cap precision arbitrarily at 1KB and add the difference
	 (if any) to the MPFR result.  */
      if (prec > 1024)
	p = 1024;
    }

  len = mpfr_snprintf (NULL, 0, fmtstr, (int)p, x);

  /* Handle the unlikely (impossible?) error by returning more than
     the maximum dictated by the function's return type.  */
  if (len < 0)
    return target_dir_max () + 1;

  /* Adjust the return value by the difference.  */
  if (p < prec)
    len += prec - p;

  return len;
}

/* Return the number of bytes to format using the format specifier
   SPEC and the precision PREC the largest value in the real floating
   TYPE.  */

static unsigned HOST_WIDE_INT
format_floating_max (tree type, char spec, HOST_WIDE_INT prec)
{
  machine_mode mode = TYPE_MODE (type);

  /* IBM Extended mode.  */
  if (MODE_COMPOSITE_P (mode))
    mode = DFmode;

  /* Get the real type format desription for the target.  */
  const real_format *rfmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE rv;

  real_maxval (&rv, 0, mode);

  /* Convert the GCC real value representation with the precision
     of the real type to the mpfr_t format with the GCC default
     round-to-nearest mode.  */
  mpfr_t x;
  mpfr_init2 (x, rfmt->p);
  mpfr_from_real (x, &rv, GMP_RNDN);

  /* Return a value one greater to account for the leading minus sign.  */
  unsigned HOST_WIDE_INT r
    = 1 + get_mpfr_format_length (x, "", prec, spec, 'D');
  mpfr_clear (x);
  return r;
}

/* Return a range representing the minimum and maximum number of bytes
   that the directive DIR will output for any argument.  PREC gives
   the adjusted precision range to account for negative precisions
   meaning the default 6.  This function is used when the directive
   argument or its value isn't known.  */

static fmtresult
format_floating (const directive &dir, const HOST_WIDE_INT prec[2])
{
  tree type;

  switch (dir.modifier)
    {
    case FMT_LEN_l:
    case FMT_LEN_none:
      type = double_type_node;
      break;

    case FMT_LEN_L:
      type = long_double_type_node;
      break;

    case FMT_LEN_ll:
      type = long_double_type_node;
      break;

    default:
      return fmtresult ();
    }

  /* The minimum and maximum number of bytes produced by the directive.  */
  fmtresult res;

  /* The minimum output as determined by flags.  It's always at least 1.
     When plus or space are set the output is preceded by either a sign
     or a space.  */
  unsigned flagmin = (1 /* for the first digit */
		      + (dir.get_flag ('+') | dir.get_flag (' ')));

  /* The minimum is 3 for "inf" and "nan" for all specifiers, plus 1
     for the plus sign/space with the '+' and ' ' flags, respectively,
     unless reduced below.  */
  res.range.min = 2 + flagmin;

  /* When the pound flag is set the decimal point is included in output
     regardless of precision.  Whether or not a decimal point is included
     otherwise depends on the specification and precision.  */
  bool radix = dir.get_flag ('#');

  switch (dir.specifier)
    {
    case 'A':
    case 'a':
      {
	HOST_WIDE_INT minprec = 6 + !radix /* decimal point */;
	if (dir.prec[0] <= 0)
	  minprec = 0;
	else if (dir.prec[0] > 0)
	  minprec = dir.prec[0] + !radix /* decimal point */;

	res.range.likely = (2 /* 0x */
			    + flagmin
			    + radix
			    + minprec
			    + 3 /* p+0 */);

	res.range.max = format_floating_max (type, 'a', prec[1]);

	/* The unlikely maximum accounts for the longest multibyte
	   decimal point character.  */
	res.range.unlikely = res.range.max;
	if (dir.prec[1] > 0)
	  res.range.unlikely += target_mb_len_max () - 1;

	break;
      }

    case 'E':
    case 'e':
      {
	/* Minimum output attributable to precision and, when it's
	   non-zero, decimal point.  */
	HOST_WIDE_INT minprec = prec[0] ? prec[0] + !radix : 0;

	/* The likely minimum output is "[-+]1.234567e+00" regardless
	   of the value of the actual argument.  */
	res.range.likely = (flagmin
			    + radix
			    + minprec
			    + 2 /* e+ */ + 2);

	res.range.max = format_floating_max (type, 'e', prec[1]);

	/* The unlikely maximum accounts for the longest multibyte
	   decimal point character.  */
	if (dir.prec[0] != dir.prec[1]
	    || dir.prec[0] == -1 || dir.prec[0] > 0)
	  res.range.unlikely = res.range.max + target_mb_len_max () -1;
	else
	  res.range.unlikely = res.range.max;
	break;
      }

    case 'F':
    case 'f':
      {
	/* Minimum output attributable to precision and, when it's non-zero,
	   decimal point.  */
	HOST_WIDE_INT minprec = prec[0] ? prec[0] + !radix : 0;

	/* For finite numbers (i.e., not infinity or NaN) the lower bound
	   when precision isn't specified is 8 bytes ("1.23456" since
	   precision is taken to be 6).  When precision is zero, the lower
	   bound is 1 byte (e.g., "1").  Otherwise, when precision is greater
	   than zero, then the lower bound is 2 plus precision (plus flags).
	   But in all cases, the lower bound is no greater than 3.  */
	unsigned HOST_WIDE_INT min = flagmin + radix + minprec;
	if (min < res.range.min)
	  res.range.min = min;

	/* Compute the upper bound for -TYPE_MAX.  */
	res.range.max = format_floating_max (type, 'f', prec[1]);

	/* The minimum output with unknown precision is a single byte
	   (e.g., "0") but the more likely output is 3 bytes ("0.0").  */
	if (dir.prec[0] < 0 && dir.prec[1] > 0)
	  res.range.likely = 3;
	else
	  res.range.likely = min;

	/* The unlikely maximum accounts for the longest multibyte
	   decimal point character.  */
	if (dir.prec[0] != dir.prec[1]
	    || dir.prec[0] == -1 || dir.prec[0] > 0)
	  res.range.unlikely = res.range.max + target_mb_len_max () - 1;
	break;
      }

    case 'G':
    case 'g':
      {
	/* The %g output depends on precision and the exponent of
	   the argument.  Since the value of the argument isn't known
	   the lower bound on the range of bytes (not counting flags
	   or width) is 1 plus radix (i.e., either "0" or "0." for
	   "%g" and "%#g", respectively, with a zero argument).  */
	unsigned HOST_WIDE_INT min = flagmin + radix;
	if (min < res.range.min)
	  res.range.min = min;

	char spec = 'g';
	HOST_WIDE_INT maxprec = dir.prec[1];
	if (radix && maxprec)
	  {
	    /* When the pound flag (radix) is set, trailing zeros aren't
	       trimmed and so the longest output is the same as for %e,
	       except with precision minus 1 (as specified in C11).  */
	    spec = 'e';
	    if (maxprec > 0)
	      --maxprec;
	    else if (maxprec < 0)
	      maxprec = 5;
	  }
	else
	  maxprec = prec[1];

	res.range.max = format_floating_max (type, spec, maxprec);

	/* The likely output is either the maximum computed above
	   minus 1 (assuming the maximum is positive) when precision
	   is known (or unspecified), or the same minimum as for %e
	   (which is computed for a non-negative argument).  Unlike
	   for the other specifiers above the likely output isn't
	   the minimum because for %g that's 1 which is unlikely.  */
	if (dir.prec[1] < 0
	    || (unsigned HOST_WIDE_INT)dir.prec[1] < target_int_max ())
	  res.range.likely = res.range.max - 1;
	else
	  {
	    HOST_WIDE_INT minprec = 6 + !radix /* decimal point */;
	    res.range.likely = (flagmin
				+ radix
				+ minprec
				+ 2 /* e+ */ + 2);
	  }

	/* The unlikely maximum accounts for the longest multibyte
	   decimal point character.  */
	res.range.unlikely = res.range.max + target_mb_len_max () - 1;
	break;
      }

    default:
      return fmtresult ();
    }

  /* Bump up the byte counters if WIDTH is greater.  */
  res.adjust_for_width_or_precision (dir.width);
  return res;
}

/* Return a range representing the minimum and maximum number of bytes
   that the directive DIR will write on output for the floating argument
   ARG.  */

static fmtresult
format_floating (const directive &dir, tree arg, vr_values *)
{
  HOST_WIDE_INT prec[] = { dir.prec[0], dir.prec[1] };
  tree type = (dir.modifier == FMT_LEN_L || dir.modifier == FMT_LEN_ll
	       ? long_double_type_node : double_type_node);

  /* For an indeterminate precision the lower bound must be assumed
     to be zero.  */
  if (TOUPPER (dir.specifier) == 'A')
    {
      /* Get the number of fractional decimal digits needed to represent
	 the argument without a loss of accuracy.  */
      unsigned fmtprec
	= REAL_MODE_FORMAT (TYPE_MODE (type))->p;

      /* The precision of the IEEE 754 double format is 53.
	 The precision of all other GCC binary double formats
	 is 56 or less.  */
      unsigned maxprec = fmtprec <= 56 ? 13 : 15;

      /* For %a, leave the minimum precision unspecified to let
	 MFPR trim trailing zeros (as it and many other systems
	 including Glibc happen to do) and set the maximum
	 precision to reflect what it would be with trailing zeros
	 present (as Solaris and derived systems do).  */
      if (dir.prec[1] < 0)
	{
	  /* Both bounds are negative implies that precision has
	     not been specified.  */
	  prec[0] = maxprec;
	  prec[1] = -1;
	}
      else if (dir.prec[0] < 0)
	{
	  /* With a negative lower bound and a non-negative upper
	     bound set the minimum precision to zero and the maximum
	     to the greater of the maximum precision (i.e., with
	     trailing zeros present) and the specified upper bound.  */
	  prec[0] = 0;
	  prec[1] = dir.prec[1] < maxprec ? maxprec : dir.prec[1];
	}
    }
  else if (dir.prec[0] < 0)
    {
      if (dir.prec[1] < 0)
	{
	  /* A precision in a strictly negative range is ignored and
	     the default of 6 is used instead.  */
	  prec[0] = prec[1] = 6;
	}
      else
	{
	  /* For a precision in a partly negative range, the lower bound
	     must be assumed to be zero and the new upper bound is the
	     greater of 6 (the default precision used when the specified
	     precision is negative) and the upper bound of the specified
	     range.  */
	  prec[0] = 0;
	  prec[1] = dir.prec[1] < 6 ? 6 : dir.prec[1];
	}
    }

  if (!arg
      || TREE_CODE (arg) != REAL_CST
      || !useless_type_conversion_p (type, TREE_TYPE (arg)))
    return format_floating (dir, prec);

  /* The minimum and maximum number of bytes produced by the directive.  */
  fmtresult res;

  /* Get the real type format desription for the target.  */
  const REAL_VALUE_TYPE *rvp = TREE_REAL_CST_PTR (arg);
  const real_format *rfmt = REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (arg)));

  if (!real_isfinite (rvp))
    {
      /* The format for Infinity and NaN is "[-]inf"/"[-]infinity"
	 and "[-]nan" with the choice being implementation-defined
	 but not locale dependent.  */
      bool sign = dir.get_flag ('+') || real_isneg (rvp);
      res.range.min = 3 + sign;

      res.range.likely = res.range.min;
      res.range.max = res.range.min;
      /* The unlikely maximum is "[-/+]infinity" or "[-/+][qs]nan".
	 For NaN, the C/POSIX standards specify two formats:
	   "[-/+]nan"
	 and
	   "[-/+]nan(n-char-sequence)"
	 No known printf implementation outputs the latter format but AIX
	 outputs QNaN and SNaN for quiet and signalling NaN, respectively,
	 so the unlikely maximum reflects that.  */
      res.range.unlikely = sign + (real_isinf (rvp) ? 8 : 4);

      /* The range for infinity and NaN is known unless either width
	 or precision is unknown.  Width has the same effect regardless
	 of whether the argument is finite.  Precision is either ignored
	 (e.g., Glibc) or can have an effect on the short vs long format
	 such as inf/infinity (e.g., Solaris).  */
      res.knownrange = dir.known_width_and_precision ();

      /* Adjust the range for width but ignore precision.  */
      res.adjust_for_width_or_precision (dir.width);

      return res;
    }

  char fmtstr [40];
  char *pfmt = fmtstr;

  /* Append flags.  */
  for (const char *pf = "-+ #0"; *pf; ++pf)
    if (dir.get_flag (*pf))
      *pfmt++ = *pf;

  *pfmt = '\0';

  {
    /* Set up an array to easily iterate over.  */
    unsigned HOST_WIDE_INT* const minmax[] = {
      &res.range.min, &res.range.max
    };

    for (int i = 0; i != sizeof minmax / sizeof *minmax; ++i)
      {
	/* Convert the GCC real value representation with the precision
	   of the real type to the mpfr_t format rounding down in the
	   first iteration that computes the minimm and up in the second
	   that computes the maximum.  This order is arbibtrary because
	   rounding in either direction can result in longer output.  */
	mpfr_t mpfrval;
	mpfr_init2 (mpfrval, rfmt->p);
	mpfr_from_real (mpfrval, rvp, i ? GMP_RNDU : GMP_RNDD);

	/* Use the MPFR rounding specifier to round down in the first
	   iteration and then up.  In most but not all cases this will
	   result in the same number of bytes.  */
	char rndspec = "DU"[i];

	/* Format it and store the result in the corresponding member
	   of the result struct.  */
	*minmax[i] = get_mpfr_format_length (mpfrval, fmtstr, prec[i],
					     dir.specifier, rndspec);
	mpfr_clear (mpfrval);
      }
  }

  /* Make sure the minimum is less than the maximum (MPFR rounding
     in the call to mpfr_snprintf can result in the reverse.  */
  if (res.range.max < res.range.min)
    {
      unsigned HOST_WIDE_INT tmp = res.range.min;
      res.range.min = res.range.max;
      res.range.max = tmp;
    }

  /* The range is known unless either width or precision is unknown.  */
  res.knownrange = dir.known_width_and_precision ();

  /* For the same floating point constant, unless width or precision
     is unknown, use the longer output as the likely maximum since
     with round to nearest either is equally likely.  Otheriwse, when
     precision is unknown, use the greater of the minimum and 3 as
     the likely output (for "0.0" since zero precision is unlikely).  */
  if (res.knownrange)
    res.range.likely = res.range.max;
  else if (res.range.min < 3
	   && dir.prec[0] < 0
	   && (unsigned HOST_WIDE_INT)dir.prec[1] == target_int_max ())
    res.range.likely = 3;
  else
    res.range.likely = res.range.min;

  res.range.unlikely = res.range.max;

  if (res.range.max > 2 && (prec[0] != 0 || prec[1] != 0))
    {
      /* Unless the precision is zero output longer than 2 bytes may
	 include the decimal point which must be a single character
	 up to MB_LEN_MAX in length.  This is overly conservative
	 since in some conversions some constants result in no decimal
	 point (e.g., in %g).  */
      res.range.unlikely += target_mb_len_max () - 1;
    }

  res.adjust_for_width_or_precision (dir.width);
  return res;
}

/* Return a FMTRESULT struct set to the lengths of the shortest and longest
   strings referenced by the expression STR, or (-1, -1) when not known.
   Used by the format_string function below.  */

static fmtresult
get_string_length (tree str, unsigned eltsize)
{
  if (!str)
    return fmtresult ();

  /* Determine the length of the shortest and longest string referenced
     by STR.  Strings of unknown lengths are bounded by the sizes of
     arrays that subexpressions of STR may refer to.  Pointers that
     aren't known to point any such arrays result in LENDATA.MAXLEN
     set to SIZE_MAX.  */
  c_strlen_data lendata = { };
  get_range_strlen (str, &lendata, eltsize);

  /* Return the default result when nothing is known about the string. */
  if (integer_all_onesp (lendata.maxbound)
      && integer_all_onesp (lendata.maxlen))
    return fmtresult ();

  HOST_WIDE_INT min
    = (tree_fits_uhwi_p (lendata.minlen)
       ? tree_to_uhwi (lendata.minlen)
       : 0);

  HOST_WIDE_INT max
    = (tree_fits_uhwi_p (lendata.maxbound)
       ? tree_to_uhwi (lendata.maxbound)
       : HOST_WIDE_INT_M1U);

  const bool unbounded = integer_all_onesp (lendata.maxlen);

  /* Set the max/likely counters to unbounded when a minimum is known
     but the maximum length isn't bounded.  This implies that STR is
     a conditional expression involving a string of known length and
     and an expression of unknown/unbounded length.  */
  if (min
      && (unsigned HOST_WIDE_INT)min < HOST_WIDE_INT_M1U
      && unbounded)
    max = HOST_WIDE_INT_M1U;

  /* get_range_strlen() returns the target value of SIZE_MAX for
     strings of unknown length.  Bump it up to HOST_WIDE_INT_M1U
     which may be bigger.  */
  if ((unsigned HOST_WIDE_INT)min == target_size_max ())
    min = HOST_WIDE_INT_M1U;
  if ((unsigned HOST_WIDE_INT)max == target_size_max ())
    max = HOST_WIDE_INT_M1U;

  fmtresult res (min, max);
  res.nonstr = lendata.decl;

  /* Set RES.KNOWNRANGE to true if and only if all strings referenced
     by STR are known to be bounded (though not necessarily by their
     actual length but perhaps by their maximum possible length).  */
  if (res.range.max < target_int_max ())
    {
      res.knownrange = true;
      /* When the the length of the longest string is known and not
	 excessive use it as the likely length of the string(s).  */
      res.range.likely = res.range.max;
    }
  else
    {
      /* When the upper bound is unknown (it can be zero or excessive)
	 set the likely length to the greater of 1 and the length of
	 the shortest string and reset the lower bound to zero.  */
      res.range.likely = res.range.min ? res.range.min : warn_level > 1;
      res.range.min = 0;
    }

  res.range.unlikely = unbounded ? HOST_WIDE_INT_MAX : res.range.max;

  return res;
}

/* Return the minimum and maximum number of characters formatted
   by the '%c' format directives and its wide character form for
   the argument ARG.  ARG can be null (for functions such as
   vsprinf).  */

static fmtresult
format_character (const directive &dir, tree arg, vr_values *vr_values)
{
  fmtresult res;

  res.knownrange = true;

  if (dir.specifier == 'C'
      || dir.modifier == FMT_LEN_l)
    {
      /* A wide character can result in as few as zero bytes.  */
      res.range.min = 0;

      HOST_WIDE_INT min, max;
      if (get_int_range (arg, &min, &max, false, 0, vr_values))
	{
	  if (min == 0 && max == 0)
	    {
	      /* The NUL wide character results in no bytes.  */
	      res.range.max = 0;
	      res.range.likely = 0;
	      res.range.unlikely = 0;
	    }
	  else if (min >= 0 && min < 128)
	    {
	      /* Be conservative if the target execution character set
		 is not a 1-to-1 mapping to the source character set or
		 if the source set is not ASCII.  */
	      bool one_2_one_ascii
		= (target_to_host_charmap[0] == 1 && target_to_host ('a') == 97);

	      /* A wide character in the ASCII range most likely results
		 in a single byte, and only unlikely in up to MB_LEN_MAX.  */
	      res.range.max = one_2_one_ascii ? 1 : target_mb_len_max ();;
	      res.range.likely = 1;
	      res.range.unlikely = target_mb_len_max ();
	      res.mayfail = !one_2_one_ascii;
	    }
	  else
	    {
	      /* A wide character outside the ASCII range likely results
		 in up to two bytes, and only unlikely in up to MB_LEN_MAX.  */
	      res.range.max = target_mb_len_max ();
	      res.range.likely = 2;
	      res.range.unlikely = res.range.max;
	      /* Converting such a character may fail.  */
	      res.mayfail = true;
	    }
	}
      else
	{
	  /* An unknown wide character is treated the same as a wide
	     character outside the ASCII range.  */
	  res.range.max = target_mb_len_max ();
	  res.range.likely = 2;
	  res.range.unlikely = res.range.max;
	  res.mayfail = true;
	}
    }
  else
    {
      /* A plain '%c' directive.  Its ouput is exactly 1.  */
      res.range.min = res.range.max = 1;
      res.range.likely = res.range.unlikely = 1;
      res.knownrange = true;
    }

  /* Bump up the byte counters if WIDTH is greater.  */
  return res.adjust_for_width_or_precision (dir.width);
}

/* Return the minimum and maximum number of characters formatted
   by the '%s' format directive and its wide character form for
   the argument ARG.  ARG can be null (for functions such as
   vsprinf).  */

static fmtresult
format_string (const directive &dir, tree arg, vr_values *)
{
  fmtresult res;

  /* Compute the range the argument's length can be in.  */
  int count_by = 1;
  if (dir.specifier == 'S' || dir.modifier == FMT_LEN_l)
    {
      /* Get a node for a C type that will be the same size
	 as a wchar_t on the target.  */
      tree node = get_typenode_from_name (MODIFIED_WCHAR_TYPE);

      /* Now that we have a suitable node, get the number of
	 bytes it occupies.  */
      count_by = int_size_in_bytes (node); 
      gcc_checking_assert (count_by == 2 || count_by == 4);
    }

  fmtresult slen = get_string_length (arg, count_by);
  if (slen.range.min == slen.range.max
      && slen.range.min < HOST_WIDE_INT_MAX)
    {
      /* The argument is either a string constant or it refers
	 to one of a number of strings of the same length.  */

      /* A '%s' directive with a string argument with constant length.  */
      res.range = slen.range;

      if (dir.specifier == 'S'
	  || dir.modifier == FMT_LEN_l)
	{
	  /* In the worst case the length of output of a wide string S
	     is bounded by MB_LEN_MAX * wcslen (S).  */
	  res.range.max *= target_mb_len_max ();
	  res.range.unlikely = res.range.max;
	  /* It's likely that the the total length is not more that
	     2 * wcslen (S).*/
	  res.range.likely = res.range.min * 2;

	  if (dir.prec[1] >= 0
	      && (unsigned HOST_WIDE_INT)dir.prec[1] < res.range.max)
	    {
	      res.range.max = dir.prec[1];
	      res.range.likely = dir.prec[1];
	      res.range.unlikely = dir.prec[1];
	    }

	  if (dir.prec[0] < 0 && dir.prec[1] > -1)
	    res.range.min = 0;
	  else if (dir.prec[0] >= 0)
	    res.range.likely = dir.prec[0];

	  /* Even a non-empty wide character string need not convert into
	     any bytes.  */
	  res.range.min = 0;

	  /* A non-empty wide character conversion may fail.  */
	  if (slen.range.max > 0)
	    res.mayfail = true;
	}
      else
	{
	  res.knownrange = true;

	  if (dir.prec[0] < 0 && dir.prec[1] > -1)
	    res.range.min = 0;
	  else if ((unsigned HOST_WIDE_INT)dir.prec[0] < res.range.min)
	    res.range.min = dir.prec[0];

	  if ((unsigned HOST_WIDE_INT)dir.prec[1] < res.range.max)
	    {
	      res.range.max = dir.prec[1];
	      res.range.likely = dir.prec[1];
	      res.range.unlikely = dir.prec[1];
	    }
	}
    }
  else if (arg && integer_zerop (arg))
    {
      /* Handle null pointer argument.  */

      fmtresult res (0);
      res.nullp = true;
      return res;
    }
  else
    {
      /* For a '%s' and '%ls' directive with a non-constant string (either
	 one of a number of strings of known length or an unknown string)
	 the minimum number of characters is lesser of PRECISION[0] and
	 the length of the shortest known string or zero, and the maximum
	 is the lessser of the length of the longest known string or
	 PTRDIFF_MAX and PRECISION[1].  The likely length is either
	 the minimum at level 1 and the greater of the minimum and 1
	 at level 2.  This result is adjust upward for width (if it's
	 specified).  */

      if (dir.specifier == 'S'
	  || dir.modifier == FMT_LEN_l)
	{
	  /* A wide character converts to as few as zero bytes.  */
	  slen.range.min = 0;
	  if (slen.range.max < target_int_max ())
	    slen.range.max *= target_mb_len_max ();

	  if (slen.range.likely < target_int_max ())
	    slen.range.likely *= 2;

	  if (slen.range.likely < target_int_max ())
	    slen.range.unlikely *= target_mb_len_max ();

	  /* A non-empty wide character conversion may fail.  */
	  if (slen.range.max > 0)
	    res.mayfail = true;
	}

      res.range = slen.range;

      if (dir.prec[0] >= 0)
	{
	  /* Adjust the minimum to zero if the string length is unknown,
	     or at most the lower bound of the precision otherwise.  */
	  if (slen.range.min >= target_int_max ())
	    res.range.min = 0;
	  else if ((unsigned HOST_WIDE_INT)dir.prec[0] < slen.range.min)
	    res.range.min = dir.prec[0];

	  /* Make both maxima no greater than the upper bound of precision.  */
	  if ((unsigned HOST_WIDE_INT)dir.prec[1] < slen.range.max
	      || slen.range.max >= target_int_max ())
	    {
	      res.range.max = dir.prec[1];
	      res.range.unlikely = dir.prec[1];
	    }

	  /* If precision is constant, set the likely counter to the lesser
	     of it and the maximum string length.  Otherwise, if the lower
	     bound of precision is greater than zero, set the likely counter
	     to the minimum.  Otherwise set it to zero or one based on
	     the warning level.  */
	  if (dir.prec[0] == dir.prec[1])
	    res.range.likely
	      = ((unsigned HOST_WIDE_INT)dir.prec[0] < slen.range.max
		 ? dir.prec[0] : slen.range.max);
	  else if (dir.prec[0] > 0)
	    res.range.likely = res.range.min;
	  else
	    res.range.likely = warn_level > 1;
	}
      else if (dir.prec[1] >= 0)
	{
	  res.range.min = 0;
	  if ((unsigned HOST_WIDE_INT)dir.prec[1] < slen.range.max)
	    res.range.max = dir.prec[1];
	  res.range.likely = dir.prec[1] ? warn_level > 1 : 0;
	  if ((unsigned HOST_WIDE_INT)dir.prec[1] < slen.range.unlikely)
	    res.range.unlikely = dir.prec[1];
	}
      else if (slen.range.min >= target_int_max ())
	{
	  res.range.min = 0;
	  res.range.max = HOST_WIDE_INT_MAX;
	  /* At level 1 strings of unknown length are assumed to be
	     empty, while at level 1 they are assumed to be one byte
	     long.  */
	  res.range.likely = warn_level > 1;
	  res.range.unlikely = HOST_WIDE_INT_MAX;
	}
      else
	{
	  /* A string of unknown length unconstrained by precision is
	     assumed to be empty at level 1 and just one character long
	     at higher levels.  */
	  if (res.range.likely >= target_int_max ())
	    res.range.likely = warn_level > 1;
	}
    }

  /* If the argument isn't a nul-terminated string and the number
     of bytes on output isn't bounded by precision, set NONSTR.  */
  if (slen.nonstr && slen.range.min < (unsigned HOST_WIDE_INT)dir.prec[0])
    res.nonstr = slen.nonstr;

  /* Bump up the byte counters if WIDTH is greater.  */
  return res.adjust_for_width_or_precision (dir.width);
}

/* Format plain string (part of the format string itself).  */

static fmtresult
format_plain (const directive &dir, tree, vr_values *)
{
  fmtresult res (dir.len);
  return res;
}

/* Return true if the RESULT of a directive in a call describe by INFO
   should be diagnosed given the AVAILable space in the destination.  */

static bool
should_warn_p (const sprintf_dom_walker::call_info &info,
	       const result_range &avail, const result_range &result)
{
  if (result.max <= avail.min)
    {
      /* The least amount of space remaining in the destination is big
	 enough for the longest output.  */
      return false;
    }

  if (info.bounded)
    {
      if (warn_format_trunc == 1 && result.min <= avail.max
	  && info.retval_used ())
	{
	  /* The likely amount of space remaining in the destination is big
	     enough for the least output and the return value is used.  */
	  return false;
	}

      if (warn_format_trunc == 1 && result.likely <= avail.likely
	  && !info.retval_used ())
	{
	  /* The likely amount of space remaining in the destination is big
	     enough for the likely output and the return value is unused.  */
	  return false;
	}

      if (warn_format_trunc == 2
	  && result.likely <= avail.min
	  && (result.max <= avail.min
	      || result.max > HOST_WIDE_INT_MAX))
	{
	  /* The minimum amount of space remaining in the destination is big
	     enough for the longest output.  */
	  return false;
	}
    }
  else
    {
      if (warn_level == 1 && result.likely <= avail.likely)
	{
	  /* The likely amount of space remaining in the destination is big
	     enough for the likely output.  */
	  return false;
	}

      if (warn_level == 2
	  && result.likely <= avail.min
	  && (result.max <= avail.min
	      || result.max > HOST_WIDE_INT_MAX))
	{
	  /* The minimum amount of space remaining in the destination is big
	     enough for the longest output.  */
	  return false;
	}
    }

  return true;
}

/* At format string location describe by DIRLOC in a call described
   by INFO, issue a warning for a directive DIR whose output may be
   in excess of the available space AVAIL_RANGE in the destination
   given the formatting result FMTRES.  This function does nothing
   except decide whether to issue a warning for a possible write
   past the end or truncation and, if so, format the warning.
   Return true if a warning has been issued.  */

static bool
maybe_warn (substring_loc &dirloc, location_t argloc,
	    const sprintf_dom_walker::call_info &info,
	    const result_range &avail_range, const result_range &res,
	    const directive &dir)
{
  if (!should_warn_p (info, avail_range, res))
    return false;

  /* A warning will definitely be issued below.  */

  /* The maximum byte count to reference in the warning.  Larger counts
     imply that the upper bound is unknown (and could be anywhere between
     RES.MIN + 1 and SIZE_MAX / 2) are printed as "N or more bytes" rather
     than "between N and X" where X is some huge number.  */
  unsigned HOST_WIDE_INT maxbytes = target_dir_max ();

  /* True when there is enough room in the destination for the least
     amount of a directive's output but not enough for its likely or
     maximum output.  */
  bool maybe = (res.min <= avail_range.max
		&& (avail_range.min < res.likely
		    || (res.max < HOST_WIDE_INT_MAX
			&& avail_range.min < res.max)));

  /* Buffer for the directive in the host character set (used when
     the source character set is different).  */
  char hostdir[32];

  if (avail_range.min == avail_range.max)
    {
      /* The size of the destination region is exact.  */
      unsigned HOST_WIDE_INT navail = avail_range.max;

      if (target_to_host (*dir.beg) != '%')
	{
	  /* For plain character directives (i.e., the format string itself)
	     but not others, point the caret at the first character that's
	     past the end of the destination.  */
	  if (navail < dir.len)
	    dirloc.set_caret_index (dirloc.get_caret_idx () + navail);
	}

      if (*dir.beg == '\0')
	{
	  /* This is the terminating nul.  */
	  gcc_assert (res.min == 1 && res.min == res.max);

	  return fmtwarn (dirloc, UNKNOWN_LOCATION, NULL, info.warnopt (),
			  info.bounded
			  ? (maybe
			     ? G_("%qE output may be truncated before the "
				  "last format character")
			     : G_("%qE output truncated before the last "
				  "format character"))
			  : (maybe
			     ? G_("%qE may write a terminating nul past the "
				  "end of the destination")
			     : G_("%qE writing a terminating nul past the "
				  "end of the destination")),
			  info.func);
	}

      if (res.min == res.max)
	{
	  const char *d = target_to_host (hostdir, sizeof hostdir, dir.beg);
	  if (!info.bounded)
	    return fmtwarn_n (dirloc, argloc, NULL, info.warnopt (), res.min,
			      "%<%.*s%> directive writing %wu byte into a "
			      "region of size %wu",
			      "%<%.*s%> directive writing %wu bytes into a "
			      "region of size %wu",
			      (int) dir.len, d, res.min, navail);
	  else if (maybe)
	    return fmtwarn_n (dirloc, argloc, NULL, info.warnopt (), res.min,
			      "%<%.*s%> directive output may be truncated "
			      "writing %wu byte into a region of size %wu",
			      "%<%.*s%> directive output may be truncated "
			      "writing %wu bytes into a region of size %wu",
			      (int) dir.len, d, res.min, navail);
	  else
	    return fmtwarn_n (dirloc, argloc, NULL, info.warnopt (), res.min,
			      "%<%.*s%> directive output truncated writing "
			      "%wu byte into a region of size %wu",
			      "%<%.*s%> directive output truncated writing "
			      "%wu bytes into a region of size %wu",
			      (int) dir.len, d, res.min, navail);
	}
      if (res.min == 0 && res.max < maxbytes)
	return fmtwarn (dirloc, argloc, NULL,
			info.warnopt (),
			info.bounded
			? (maybe
			   ? G_("%<%.*s%> directive output may be truncated "
				"writing up to %wu bytes into a region of "
				"size %wu")
			   : G_("%<%.*s%> directive output truncated writing "
				"up to %wu bytes into a region of size %wu"))
			: G_("%<%.*s%> directive writing up to %wu bytes "
			     "into a region of size %wu"), (int) dir.len,
			target_to_host (hostdir, sizeof hostdir, dir.beg),
			res.max, navail);

      if (res.min == 0 && maxbytes <= res.max)
	/* This is a special case to avoid issuing the potentially
	   confusing warning:
	     writing 0 or more bytes into a region of size 0.  */
	return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			info.bounded
			? (maybe
			   ? G_("%<%.*s%> directive output may be truncated "
				"writing likely %wu or more bytes into a "
				"region of size %wu")
			   : G_("%<%.*s%> directive output truncated writing "
				"likely %wu or more bytes into a region of "
				"size %wu"))
			: G_("%<%.*s%> directive writing likely %wu or more "
			     "bytes into a region of size %wu"), (int) dir.len,
			target_to_host (hostdir, sizeof hostdir, dir.beg),
			res.likely, navail);

      if (res.max < maxbytes)
	return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			info.bounded
			? (maybe
			   ? G_("%<%.*s%> directive output may be truncated "
				"writing between %wu and %wu bytes into a "
				"region of size %wu")
			   : G_("%<%.*s%> directive output truncated "
				"writing between %wu and %wu bytes into a "
				"region of size %wu"))
			: G_("%<%.*s%> directive writing between %wu and "
			     "%wu bytes into a region of size %wu"),
			(int) dir.len,
			target_to_host (hostdir, sizeof hostdir, dir.beg),
			res.min, res.max, navail);

      return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
		      info.bounded
		      ? (maybe
			 ? G_("%<%.*s%> directive output may be truncated "
			      "writing %wu or more bytes into a region of "
			      "size %wu")
			 : G_("%<%.*s%> directive output truncated writing "
			      "%wu or more bytes into a region of size %wu"))
		      : G_("%<%.*s%> directive writing %wu or more bytes "
			   "into a region of size %wu"), (int) dir.len,
		      target_to_host (hostdir, sizeof hostdir, dir.beg),
		      res.min, navail);
    }

  /* The size of the destination region is a range.  */

  if (target_to_host (*dir.beg) != '%')
    {
      unsigned HOST_WIDE_INT navail = avail_range.max;

      /* For plain character directives (i.e., the format string itself)
	 but not others, point the caret at the first character that's
	 past the end of the destination.  */
      if (navail < dir.len)
	dirloc.set_caret_index (dirloc.get_caret_idx () + navail);
    }

  if (*dir.beg == '\0')
    {
      gcc_assert (res.min == 1 && res.min == res.max);

      return fmtwarn (dirloc, UNKNOWN_LOCATION, NULL, info.warnopt (),
		      info.bounded
		      ? (maybe
			 ? G_("%qE output may be truncated before the last "
			      "format character")
			 : G_("%qE output truncated before the last format "
			      "character"))
		      : (maybe
			 ? G_("%qE may write a terminating nul past the end "
			      "of the destination")
			 : G_("%qE writing a terminating nul past the end "
			      "of the destination")), info.func);
    }

  if (res.min == res.max)
    {
      const char *d = target_to_host (hostdir, sizeof hostdir, dir.beg);
      if (!info.bounded)
	return fmtwarn_n (dirloc, argloc, NULL, info.warnopt (), res.min,
			  "%<%.*s%> directive writing %wu byte into a region "
			  "of size between %wu and %wu",
			  "%<%.*s%> directive writing %wu bytes into a region "
			  "of size between %wu and %wu", (int) dir.len, d,
			  res.min, avail_range.min, avail_range.max);
      else if (maybe)
	return fmtwarn_n (dirloc, argloc, NULL, info.warnopt (), res.min,
			  "%<%.*s%> directive output may be truncated writing "
			  "%wu byte into a region of size between %wu and %wu",
			  "%<%.*s%> directive output may be truncated writing "
			  "%wu bytes into a region of size between %wu and "
			  "%wu", (int) dir.len, d, res.min, avail_range.min,
			  avail_range.max);
      else
	return fmtwarn_n (dirloc, argloc, NULL, info.warnopt (), res.min,
			  "%<%.*s%> directive output truncated writing %wu "
			  "byte into a region of size between %wu and %wu",
			  "%<%.*s%> directive output truncated writing %wu "
			  "bytes into a region of size between %wu and %wu",
			  (int) dir.len, d, res.min, avail_range.min,
			  avail_range.max);
    }

  if (res.min == 0 && res.max < maxbytes)
    return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
		    info.bounded
		    ? (maybe
		       ? G_("%<%.*s%> directive output may be truncated "
			    "writing up to %wu bytes into a region of size "
			    "between %wu and %wu")
		       : G_("%<%.*s%> directive output truncated writing "
			    "up to %wu bytes into a region of size between "
			    "%wu and %wu"))
		    : G_("%<%.*s%> directive writing up to %wu bytes "
			 "into a region of size between %wu and %wu"),
		    (int) dir.len,
		    target_to_host (hostdir, sizeof hostdir, dir.beg),
		    res.max, avail_range.min, avail_range.max);

  if (res.min == 0 && maxbytes <= res.max)
    /* This is a special case to avoid issuing the potentially confusing
       warning:
	 writing 0 or more bytes into a region of size between 0 and N.  */
    return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
		    info.bounded
		    ? (maybe
		       ? G_("%<%.*s%> directive output may be truncated "
			    "writing likely %wu or more bytes into a region "
			    "of size between %wu and %wu")
		       : G_("%<%.*s%> directive output truncated writing "
			    "likely %wu or more bytes into a region of size "
			    "between %wu and %wu"))
		    : G_("%<%.*s%> directive writing likely %wu or more bytes "
			 "into a region of size between %wu and %wu"),
		    (int) dir.len,
		    target_to_host (hostdir, sizeof hostdir, dir.beg),
		    res.likely, avail_range.min, avail_range.max);

  if (res.max < maxbytes)
    return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
		    info.bounded
		    ? (maybe
		       ? G_("%<%.*s%> directive output may be truncated "
			    "writing between %wu and %wu bytes into a region "
			    "of size between %wu and %wu")
		       : G_("%<%.*s%> directive output truncated writing "
			    "between %wu and %wu bytes into a region of size "
			    "between %wu and %wu"))
		    : G_("%<%.*s%> directive writing between %wu and "
			 "%wu bytes into a region of size between %wu and "
			 "%wu"), (int) dir.len,
		    target_to_host (hostdir, sizeof hostdir, dir.beg),
		    res.min, res.max, avail_range.min, avail_range.max);

  return fmtwarn (dirloc, argloc, NULL, info.warnopt (),
		  info.bounded
		  ? (maybe
		     ? G_("%<%.*s%> directive output may be truncated writing "
			  "%wu or more bytes into a region of size between "
			  "%wu and %wu")
		     : G_("%<%.*s%> directive output truncated writing "
			  "%wu or more bytes into a region of size between "
			  "%wu and %wu"))
		  : G_("%<%.*s%> directive writing %wu or more bytes "
		       "into a region of size between %wu and %wu"),
		  (int) dir.len,
		  target_to_host (hostdir, sizeof hostdir, dir.beg),
		  res.min, avail_range.min, avail_range.max);
}

/* Compute the length of the output resulting from the directive DIR
   in a call described by INFO and update the overall result of the call
   in *RES.  Return true if the directive has been handled.  */

static bool
format_directive (const sprintf_dom_walker::call_info &info,
		  format_result *res, const directive &dir,
		  class vr_values *vr_values)
{
  /* Offset of the beginning of the directive from the beginning
     of the format string.  */
  size_t offset = dir.beg - info.fmtstr;
  size_t start = offset;
  size_t length = offset + dir.len - !!dir.len;

  /* Create a location for the whole directive from the % to the format
     specifier.  */
  substring_loc dirloc (info.fmtloc, TREE_TYPE (info.format),
			offset, start, length);

  /* Also get the location of the argument if possible.
     This doesn't work for integer literals or function calls.  */
  location_t argloc = UNKNOWN_LOCATION;
  if (dir.arg)
    argloc = EXPR_LOCATION (dir.arg);

  /* Bail when there is no function to compute the output length,
     or when minimum length checking has been disabled.   */
  if (!dir.fmtfunc || res->range.min >= HOST_WIDE_INT_MAX)
    return false;

  /* Compute the range of lengths of the formatted output.  */
  fmtresult fmtres = dir.fmtfunc (dir, dir.arg, vr_values);

  /* Record whether the output of all directives is known to be
     bounded by some maximum, implying that their arguments are
     either known exactly or determined to be in a known range
     or, for strings, limited by the upper bounds of the arrays
     they refer to.  */
  res->knownrange &= fmtres.knownrange;

  if (!fmtres.knownrange)
    {
      /* Only when the range is known, check it against the host value
	 of INT_MAX + (the number of bytes of the "%.*Lf" directive with
	 INT_MAX precision, which is the longest possible output of any
	 single directive).  That's the largest valid byte count (though
	 not valid call to a printf-like function because it can never
	 return such a count).  Otherwise, the range doesn't correspond
	 to known values of the argument.  */
      if (fmtres.range.max > target_dir_max ())
	{
	  /* Normalize the MAX counter to avoid having to deal with it
	     later.  The counter can be less than HOST_WIDE_INT_M1U
	     when compiling for an ILP32 target on an LP64 host.  */
	  fmtres.range.max = HOST_WIDE_INT_M1U;
	  /* Disable exact and maximum length checking after a failure
	     to determine the maximum number of characters (for example
	     for wide characters or wide character strings) but continue
	     tracking the minimum number of characters.  */
	  res->range.max = HOST_WIDE_INT_M1U;
	}

      if (fmtres.range.min > target_dir_max ())
	{
	  /* Disable exact length checking after a failure to determine
	     even the minimum number of characters (it shouldn't happen
	     except in an error) but keep tracking the minimum and maximum
	     number of characters.  */
	  return true;
	}
    }

  /* Buffer for the directive in the host character set (used when
     the source character set is different).  */
  char hostdir[32];

  int dirlen = dir.len;

  if (fmtres.nullp)
    {
      fmtwarn (dirloc, argloc, NULL, info.warnopt (),
	       "%G%<%.*s%> directive argument is null",
	       info.callstmt, dirlen,
	       target_to_host (hostdir, sizeof hostdir, dir.beg));

      /* Don't bother processing the rest of the format string.  */
      res->warned = true;
      res->range.min = HOST_WIDE_INT_M1U;
      res->range.max = HOST_WIDE_INT_M1U;
      return false;
    }

  /* Compute the number of available bytes in the destination.  There
     must always be at least one byte of space for the terminating
     NUL that's appended after the format string has been processed.  */
  result_range avail_range = bytes_remaining (info.objsize, *res);

  bool warned = res->warned;

  if (!warned)
    warned = maybe_warn (dirloc, argloc, info, avail_range,
			 fmtres.range, dir);

  /* Bump up the total maximum if it isn't too big.  */
  if (res->range.max < HOST_WIDE_INT_MAX
      && fmtres.range.max < HOST_WIDE_INT_MAX)
    res->range.max += fmtres.range.max;

  /* Raise the total unlikely maximum by the larger of the maximum
     and the unlikely maximum.  */
  unsigned HOST_WIDE_INT save = res->range.unlikely;
  if (fmtres.range.max < fmtres.range.unlikely)
    res->range.unlikely += fmtres.range.unlikely;
  else
    res->range.unlikely += fmtres.range.max;

  if (res->range.unlikely < save)
    res->range.unlikely = HOST_WIDE_INT_M1U;

  res->range.min += fmtres.range.min;
  res->range.likely += fmtres.range.likely;

  /* Has the minimum directive output length exceeded the maximum
     of 4095 bytes required to be supported?  */
  bool minunder4k = fmtres.range.min < 4096;
  bool maxunder4k = fmtres.range.max < 4096;
  /* Clear POSUNDER4K in the overall result if the maximum has exceeded
     the 4k (this is necessary to avoid the return value optimization
     that may not be safe in the maximum case).  */
  if (!maxunder4k)
    res->posunder4k = false;
  /* Also clear POSUNDER4K if the directive may fail.  */
  if (fmtres.mayfail)
    res->posunder4k = false;

  if (!warned
      /* Only warn at level 2.  */
      && warn_level > 1
      /* Only warn for string functions.  */
      && info.is_string_func ()
      && (!minunder4k
	  || (!maxunder4k && fmtres.range.max < HOST_WIDE_INT_MAX)))
    {
      /* The directive output may be longer than the maximum required
	 to be handled by an implementation according to 7.21.6.1, p15
	 of C11.  Warn on this only at level 2 but remember this and
	 prevent folding the return value when done.  This allows for
	 the possibility of the actual libc call failing due to ENOMEM
	 (like Glibc does with very large precision or width).
	 Issue the "may exceed" warning only for string functions and
	 not for fprintf or printf.  */

      if (fmtres.range.min == fmtres.range.max)
	warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			  "%<%.*s%> directive output of %wu bytes exceeds "
			  "minimum required size of 4095", dirlen,
			  target_to_host (hostdir, sizeof hostdir, dir.beg),
			  fmtres.range.min);
      else if (!minunder4k)
	warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			  "%<%.*s%> directive output between %wu and %wu "
			  "bytes exceeds minimum required size of 4095",
			  dirlen,
			  target_to_host (hostdir, sizeof hostdir, dir.beg),
			  fmtres.range.min, fmtres.range.max);
      else if (!info.retval_used () && info.is_string_func ())
	warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			  "%<%.*s%> directive output between %wu and %wu "
			  "bytes may exceed minimum required size of "
			  "4095",
			  dirlen,
			  target_to_host (hostdir, sizeof hostdir, dir.beg),
			  fmtres.range.min, fmtres.range.max);
    }

  /* Has the likely and maximum directive output exceeded INT_MAX?  */
  bool likelyximax = *dir.beg && res->range.likely > target_int_max ();
  /* Don't consider the maximum to be in excess when it's the result
     of a string of unknown length (i.e., whose maximum has been set
     to be greater than or equal to HOST_WIDE_INT_MAX.  */
  bool maxximax = (*dir.beg
		   && res->range.max > target_int_max ()
		   && res->range.max < HOST_WIDE_INT_MAX);

  if (!warned
      /* Warn for the likely output size at level 1.  */
      && (likelyximax
	  /* But only warn for the maximum at level 2.  */
	  || (warn_level > 1
	      && maxximax
	      && fmtres.range.max < HOST_WIDE_INT_MAX)))
    {
      if (fmtres.range.min > target_int_max ())
	{
	  /* The directive output exceeds INT_MAX bytes.  */
	  if (fmtres.range.min == fmtres.range.max)
	    warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			      "%<%.*s%> directive output of %wu bytes exceeds "
			      "%<INT_MAX%>", dirlen,
			      target_to_host (hostdir, sizeof hostdir, dir.beg),
			      fmtres.range.min);
	  else
	    warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			      "%<%.*s%> directive output between %wu and "
			      "%wu bytes exceeds %<INT_MAX%>", dirlen,
			      target_to_host (hostdir, sizeof hostdir, dir.beg),
			      fmtres.range.min, fmtres.range.max);
	}
      else if (res->range.min > target_int_max ())
	{
	  /* The directive output is under INT_MAX but causes the result
	     to exceed INT_MAX bytes.  */
	  if (fmtres.range.min == fmtres.range.max)
	    warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			      "%<%.*s%> directive output of %wu bytes causes "
			      "result to exceed %<INT_MAX%>", dirlen,
			      target_to_host (hostdir, sizeof hostdir, dir.beg),
			      fmtres.range.min);
	  else
	    warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			      "%<%.*s%> directive output between %wu and "
			      "%wu bytes causes result to exceed %<INT_MAX%>",
			      dirlen,
			      target_to_host (hostdir, sizeof hostdir, dir.beg),
			      fmtres.range.min, fmtres.range.max);
	}
      else if ((!info.retval_used () || !info.bounded)
	       && (info.is_string_func ()))
	/* Warn for calls to string functions that either aren't bounded
	   (sprintf) or whose return value isn't used.  */
	warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			  "%<%.*s%> directive output between %wu and "
			  "%wu bytes may cause result to exceed "
			  "%<INT_MAX%>", dirlen,
			  target_to_host (hostdir, sizeof hostdir, dir.beg),
			  fmtres.range.min, fmtres.range.max);
    }

  if (!warned && fmtres.nonstr)
    {
      warned = fmtwarn (dirloc, argloc, NULL, info.warnopt (),
			"%<%.*s%> directive argument is not a nul-terminated "
			"string",
			dirlen,
			target_to_host (hostdir, sizeof hostdir, dir.beg));
      if (warned && DECL_P (fmtres.nonstr))
	inform (DECL_SOURCE_LOCATION (fmtres.nonstr),
		"referenced argument declared here");
      return false;
    }

  if (warned && fmtres.range.min < fmtres.range.likely
      && fmtres.range.likely < fmtres.range.max)
    inform_n (info.fmtloc, fmtres.range.likely,
	      "assuming directive output of %wu byte",
	      "assuming directive output of %wu bytes",
	      fmtres.range.likely);

  if (warned && fmtres.argmin)
    {
      if (fmtres.argmin == fmtres.argmax)
	inform (info.fmtloc, "directive argument %qE", fmtres.argmin);
      else if (fmtres.knownrange)
	inform (info.fmtloc, "directive argument in the range [%E, %E]",
		fmtres.argmin, fmtres.argmax);
      else
	inform (info.fmtloc,
		"using the range [%E, %E] for directive argument",
		fmtres.argmin, fmtres.argmax);
    }

  res->warned |= warned;

  if (!dir.beg[0] && res->warned)
    {
      location_t callloc = gimple_location (info.callstmt);

      unsigned HOST_WIDE_INT min = res->range.min;
      unsigned HOST_WIDE_INT max = res->range.max;

      if (info.objsize < HOST_WIDE_INT_MAX)
	{
	  /* If a warning has been issued for buffer overflow or truncation
	     help the user figure out how big a buffer they need.  */

	  if (min == max)
	    inform_n (callloc, min,
		      "%qE output %wu byte into a destination of size %wu",
		      "%qE output %wu bytes into a destination of size %wu",
		      info.func, min, info.objsize);
	  else if (max < HOST_WIDE_INT_MAX)
	    inform (callloc,
		    "%qE output between %wu and %wu bytes into "
		    "a destination of size %wu",
		    info.func, min, max, info.objsize);
	  else if (min < res->range.likely && res->range.likely < max)
	    inform (callloc,
		    "%qE output %wu or more bytes (assuming %wu) into "
		    "a destination of size %wu",
		    info.func, min, res->range.likely, info.objsize);
	  else
	    inform (callloc,
		    "%qE output %wu or more bytes into a destination of size "
		    "%wu",
		    info.func, min, info.objsize);
	}
      else if (!info.is_string_func ())
	{
	  /* If the warning is for a file function function like fprintf
	     of printf with no destination size just print the computed
	     result.  */
	  if (min == max)
	    inform_n (callloc, min,
		      "%qE output %wu byte", "%qE output %wu bytes",
		      info.func, min);
	  else if (max < HOST_WIDE_INT_MAX)
	    inform (callloc,
		    "%qE output between %wu and %wu bytes",
		    info.func, min, max);
	  else if (min < res->range.likely && res->range.likely < max)
	    inform (callloc,
		    "%qE output %wu or more bytes (assuming %wu)",
		    info.func, min, res->range.likely);
	  else
	    inform (callloc,
		    "%qE output %wu or more bytes",
		    info.func, min);
	}
    }

  if (dump_file && *dir.beg)
    {
      fprintf (dump_file,
	       "    Result: "
	       HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC ", "
	       HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC " ("
	       HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC ", "
	       HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC ")\n",
	       fmtres.range.min, fmtres.range.likely,
	       fmtres.range.max, fmtres.range.unlikely,
	       res->range.min, res->range.likely,
	       res->range.max, res->range.unlikely);
    }

  return true;
}

/* Parse a format directive in function call described by INFO starting
   at STR and populate DIR structure.  Bump up *ARGNO by the number of
   arguments extracted for the directive.  Return the length of
   the directive.  */

static size_t
parse_directive (sprintf_dom_walker::call_info &info,
		 directive &dir, format_result *res,
		 const char *str, unsigned *argno,
		 vr_values *vr_values)
{
  const char *pcnt = strchr (str, target_percent);
  dir.beg = str;

  if (size_t len = pcnt ? pcnt - str : *str ? strlen (str) : 1)
    {
      /* This directive is either a plain string or the terminating nul
	 (which isn't really a directive but it simplifies things to
	 handle it as if it were).  */
      dir.len = len;
      dir.fmtfunc = format_plain;

      if (dump_file)
	{
	  fprintf (dump_file, "  Directive %u at offset "
		   HOST_WIDE_INT_PRINT_UNSIGNED ": \"%.*s\", "
		   "length = " HOST_WIDE_INT_PRINT_UNSIGNED "\n",
		   dir.dirno,
		   (unsigned HOST_WIDE_INT)(size_t)(dir.beg - info.fmtstr),
		   (int)dir.len, dir.beg, (unsigned HOST_WIDE_INT) dir.len);
	}

      return len - !*str;
    }

  const char *pf = pcnt + 1;

    /* POSIX numbered argument index or zero when none.  */
  HOST_WIDE_INT dollar = 0;

  /* With and precision.  -1 when not specified, HOST_WIDE_INT_MIN
     when given by a va_list argument, and a non-negative value
     when specified in the format string itself.  */
  HOST_WIDE_INT width = -1;
  HOST_WIDE_INT precision = -1;

  /* Pointers to the beginning of the width and precision decimal
     string (if any) within the directive.  */
  const char *pwidth = 0;
  const char *pprec = 0;

  /* When the value of the decimal string that specifies width or
     precision is out of range, points to the digit that causes
     the value to exceed the limit.  */
  const char *werange = NULL;
  const char *perange = NULL;

  /* Width specified via the asterisk.  Need not be INTEGER_CST.
     For vararg functions set to void_node.  */
  tree star_width = NULL_TREE;

  /* Width specified via the asterisk.  Need not be INTEGER_CST.
     For vararg functions set to void_node.  */
  tree star_precision = NULL_TREE;

  if (ISDIGIT (target_to_host (*pf)))
    {
      /* This could be either a POSIX positional argument, the '0'
	 flag, or a width, depending on what follows.  Store it as
	 width and sort it out later after the next character has
	 been seen.  */
      pwidth = pf;
      width = target_strtowi (&pf, &werange);
    }
  else if (target_to_host (*pf) == '*')
    {
      /* Similarly to the block above, this could be either a POSIX
	 positional argument or a width, depending on what follows.  */
      if (*argno < gimple_call_num_args (info.callstmt))
	star_width = gimple_call_arg (info.callstmt, (*argno)++);
      else
	star_width = void_node;
      ++pf;
    }

  if (target_to_host (*pf) == '$')
    {
      /* Handle the POSIX dollar sign which references the 1-based
	 positional argument number.  */
      if (width != -1)
	dollar = width + info.argidx;
      else if (star_width
	       && TREE_CODE (star_width) == INTEGER_CST
	       && (TYPE_PRECISION (TREE_TYPE (star_width))
		   <= TYPE_PRECISION (integer_type_node)))
	dollar = width + tree_to_shwi (star_width);

      /* Bail when the numbered argument is out of range (it will
	 have already been diagnosed by -Wformat).  */
      if (dollar == 0
	  || dollar == (int)info.argidx
	  || dollar > gimple_call_num_args (info.callstmt))
	return false;

      --dollar;

      star_width = NULL_TREE;
      width = -1;
      ++pf;
    }

  if (dollar || !star_width)
    {
      if (width != -1)
	{
	  if (width == 0)
	    {
	      /* The '0' that has been interpreted as a width above is
		 actually a flag.  Reset HAVE_WIDTH, set the '0' flag,
		 and continue processing other flags.  */
	      width = -1;
	      dir.set_flag ('0');
	    }
	  else if (!dollar)
	    {
	      /* (Non-zero) width has been seen.  The next character
		 is either a period or a digit.  */
	      goto start_precision;
	    }
	}
      /* When either '$' has been seen, or width has not been seen,
	 the next field is the optional flags followed by an optional
	 width.  */
      for ( ; ; ) {
	switch (target_to_host (*pf))
	  {
	  case ' ':
	  case '0':
	  case '+':
	  case '-':
	  case '#':
	    dir.set_flag (target_to_host (*pf++));
	    break;

	  default:
	    goto start_width;
	  }
      }

    start_width:
      if (ISDIGIT (target_to_host (*pf)))
	{
	  werange = 0;
	  pwidth = pf;
	  width = target_strtowi (&pf, &werange);
	}
      else if (target_to_host (*pf) == '*')
	{
	  if (*argno < gimple_call_num_args (info.callstmt))
	    star_width = gimple_call_arg (info.callstmt, (*argno)++);
	  else
	    {
	      /* This is (likely) a va_list.  It could also be an invalid
		 call with insufficient arguments.  */
	      star_width = void_node;
	    }
	  ++pf;
	}
      else if (target_to_host (*pf) == '\'')
	{
	  /* The POSIX apostrophe indicating a numeric grouping
	     in the current locale.  Even though it's possible to
	     estimate the upper bound on the size of the output
	     based on the number of digits it probably isn't worth
	     continuing.  */
	  return 0;
	}
    }

 start_precision:
  if (target_to_host (*pf) == '.')
    {
      ++pf;

      if (ISDIGIT (target_to_host (*pf)))
	{
	  pprec = pf;
	  precision = target_strtowi (&pf, &perange);
	}
      else if (target_to_host (*pf) == '*')
	{
	  if (*argno < gimple_call_num_args (info.callstmt))
	    star_precision = gimple_call_arg (info.callstmt, (*argno)++);
	  else
	    {
	      /* This is (likely) a va_list.  It could also be an invalid
		 call with insufficient arguments.  */
	      star_precision = void_node;
	    }
	  ++pf;
	}
      else
	{
	  /* The decimal precision or the asterisk are optional.
	     When neither is dirified it's taken to be zero.  */
	  precision = 0;
	}
    }

  switch (target_to_host (*pf))
    {
    case 'h':
      if (target_to_host (pf[1]) == 'h')
	{
	  ++pf;
	  dir.modifier = FMT_LEN_hh;
	}
      else
	dir.modifier = FMT_LEN_h;
      ++pf;
      break;

    case 'j':
      dir.modifier = FMT_LEN_j;
      ++pf;
      break;

    case 'L':
      dir.modifier = FMT_LEN_L;
      ++pf;
      break;

    case 'l':
      if (target_to_host (pf[1]) == 'l')
	{
	  ++pf;
	  dir.modifier = FMT_LEN_ll;
	}
      else
	dir.modifier = FMT_LEN_l;
      ++pf;
      break;

    case 't':
      dir.modifier = FMT_LEN_t;
      ++pf;
      break;

    case 'z':
      dir.modifier = FMT_LEN_z;
      ++pf;
      break;
    }

  switch (target_to_host (*pf))
    {
      /* Handle a sole '%' character the same as "%%" but since it's
	 undefined prevent the result from being folded.  */
    case '\0':
      --pf;
      res->range.min = res->range.max = HOST_WIDE_INT_M1U;
      /* FALLTHRU */
    case '%':
      dir.fmtfunc = format_percent;
      break;

    case 'a':
    case 'A':
    case 'e':
    case 'E':
    case 'f':
    case 'F':
    case 'g':
    case 'G':
      res->floating = true;
      dir.fmtfunc = format_floating;
      break;

    case 'd':
    case 'i':
    case 'o':
    case 'u':
    case 'x':
    case 'X':
      dir.fmtfunc = format_integer;
      break;

    case 'p':
      /* The %p output is implementation-defined.  It's possible
	 to determine this format but due to extensions (edirially
	 those of the Linux kernel -- see bug 78512) the first %p
	 in the format string disables any further processing.  */
      return false;

    case 'n':
      /* %n has side-effects even when nothing is actually printed to
	 any buffer.  */
      info.nowrite = false;
      dir.fmtfunc = format_none;
      break;

    case 'C':
    case 'c':
      /* POSIX wide character and C/POSIX narrow character.  */
      dir.fmtfunc = format_character;
      break;

    case 'S':
    case 's':
      /* POSIX wide string and C/POSIX narrow character string.  */
      dir.fmtfunc = format_string;
      break;

    default:
      /* Unknown conversion specification.  */
      return 0;
    }

  dir.specifier = target_to_host (*pf++);

  /* Store the length of the format directive.  */
  dir.len = pf - pcnt;

  /* Buffer for the directive in the host character set (used when
     the source character set is different).  */
  char hostdir[32];

  if (star_width)
    {
      if (INTEGRAL_TYPE_P (TREE_TYPE (star_width)))
	dir.set_width (star_width, vr_values);
      else
	{
	  /* Width specified by a va_list takes on the range [0, -INT_MIN]
	     (width is the absolute value of that specified).  */
	  dir.width[0] = 0;
	  dir.width[1] = target_int_max () + 1;
	}
    }
  else
    {
      if (width == HOST_WIDE_INT_MAX && werange)
	{
	  size_t begin = dir.beg - info.fmtstr + (pwidth - pcnt);
	  size_t caret = begin + (werange - pcnt);
	  size_t end = pf - info.fmtstr - 1;

	  /* Create a location for the width part of the directive,
	     pointing the caret at the first out-of-range digit.  */
	  substring_loc dirloc (info.fmtloc, TREE_TYPE (info.format),
				caret, begin, end);

	  fmtwarn (dirloc, UNKNOWN_LOCATION, NULL, info.warnopt (),
		   "%<%.*s%> directive width out of range", (int) dir.len,
		   target_to_host (hostdir, sizeof hostdir, dir.beg));
	}

      dir.set_width (width);
    }

  if (star_precision)
    {
      if (INTEGRAL_TYPE_P (TREE_TYPE (star_precision)))
	dir.set_precision (star_precision, vr_values);
      else
	{
	  /* Precision specified by a va_list takes on the range [-1, INT_MAX]
	     (unlike width, negative precision is ignored).  */
	  dir.prec[0] = -1;
	  dir.prec[1] = target_int_max ();
	}
    }
  else
    {
      if (precision == HOST_WIDE_INT_MAX && perange)
	{
	  size_t begin = dir.beg - info.fmtstr + (pprec - pcnt) - 1;
	  size_t caret = dir.beg - info.fmtstr + (perange - pcnt) - 1;
	  size_t end = pf - info.fmtstr - 2;

	  /* Create a location for the precision part of the directive,
	     including the leading period, pointing the caret at the first
	     out-of-range digit .  */
	  substring_loc dirloc (info.fmtloc, TREE_TYPE (info.format),
				caret, begin, end);

	  fmtwarn (dirloc, UNKNOWN_LOCATION, NULL, info.warnopt (),
		   "%<%.*s%> directive precision out of range", (int) dir.len,
		   target_to_host (hostdir, sizeof hostdir, dir.beg));
	}

      dir.set_precision (precision);
    }

  /* Extract the argument if the directive takes one and if it's
     available (e.g., the function doesn't take a va_list).  Treat
     missing arguments the same as va_list, even though they will
     have likely already been diagnosed by -Wformat.  */
  if (dir.specifier != '%'
      && *argno < gimple_call_num_args (info.callstmt))
    dir.arg = gimple_call_arg (info.callstmt, dollar ? dollar : (*argno)++);

  if (dump_file)
    {
      fprintf (dump_file,
	       "  Directive %u at offset " HOST_WIDE_INT_PRINT_UNSIGNED
	       ": \"%.*s\"",
	       dir.dirno,
	       (unsigned HOST_WIDE_INT)(size_t)(dir.beg - info.fmtstr),
	       (int)dir.len, dir.beg);
      if (star_width)
	{
	  if (dir.width[0] == dir.width[1])
	    fprintf (dump_file, ", width = " HOST_WIDE_INT_PRINT_DEC,
		     dir.width[0]);
	  else
	    fprintf (dump_file,
		     ", width in range [" HOST_WIDE_INT_PRINT_DEC
		     ", " HOST_WIDE_INT_PRINT_DEC "]",
		     dir.width[0], dir.width[1]);
	}

      if (star_precision)
	{
	  if (dir.prec[0] == dir.prec[1])
	    fprintf (dump_file, ", precision = " HOST_WIDE_INT_PRINT_DEC,
		     dir.prec[0]);
	  else
	    fprintf (dump_file,
		     ", precision in range [" HOST_WIDE_INT_PRINT_DEC
		     HOST_WIDE_INT_PRINT_DEC "]",
		     dir.prec[0], dir.prec[1]);
	}
      fputc ('\n', dump_file);
    }

  return dir.len;
}

/* Compute the length of the output resulting from the call to a formatted
   output function described by INFO and store the result of the call in
   *RES.  Issue warnings for detected past the end writes.  Return true
   if the complete format string has been processed and *RES can be relied
   on, false otherwise (e.g., when a unknown or unhandled directive was seen
   that caused the processing to be terminated early).  */

bool
sprintf_dom_walker::compute_format_length (call_info &info,
					   format_result *res)
{
  if (dump_file)
    {
      location_t callloc = gimple_location (info.callstmt);
      fprintf (dump_file, "%s:%i: ",
	       LOCATION_FILE (callloc), LOCATION_LINE (callloc));
      print_generic_expr (dump_file, info.func, dump_flags);

      fprintf (dump_file,
	       ": objsize = " HOST_WIDE_INT_PRINT_UNSIGNED
	       ", fmtstr = \"%s\"\n",
	       info.objsize, info.fmtstr);
    }

  /* Reset the minimum and maximum byte counters.  */
  res->range.min = res->range.max = 0;

  /* No directive has been seen yet so the length of output is bounded
     by the known range [0, 0] (with no conversion resulting in a failure
     or producing more than 4K bytes) until determined otherwise.  */
  res->knownrange = true;
  res->floating = false;
  res->warned = false;

  /* 1-based directive counter.  */
  unsigned dirno = 1;

  /* The variadic argument counter.  */
  unsigned argno = info.argidx;

  for (const char *pf = info.fmtstr; ; ++dirno)
    {
      directive dir = directive ();
      dir.dirno = dirno;

      size_t n = parse_directive (info, dir, res, pf, &argno,
				  evrp_range_analyzer.get_vr_values ());

      /* Return failure if the format function fails.  */
      if (!format_directive (info, res, dir,
			     evrp_range_analyzer.get_vr_values ()))
	return false;

      /* Return success the directive is zero bytes long and it's
	 the last think in the format string (i.e., it's the terminating
	 nul, which isn't really a directive but handling it as one makes
	 things simpler).  */
      if (!n)
	return *pf == '\0';

      pf += n;
    }

  /* The complete format string was processed (with or without warnings).  */
  return true;
}

/* Return the size of the object referenced by the expression DEST if
   available, or the maximum possible size otherwise.  */

static unsigned HOST_WIDE_INT
get_destination_size (tree dest)
{
  /* When there is no destination return the maximum.  */
  if (!dest)
    return HOST_WIDE_INT_MAX;

  /* Initialize object size info before trying to compute it.  */
  init_object_sizes ();

  /* Use __builtin_object_size to determine the size of the destination
     object.  When optimizing, determine the smallest object (such as
     a member array as opposed to the whole enclosing object), otherwise
     use type-zero object size to determine the size of the enclosing
     object (the function fails without optimization in this type).  */
  int ost = optimize > 0;
  unsigned HOST_WIDE_INT size;
  if (compute_builtin_object_size (dest, ost, &size))
    return size;

  return HOST_WIDE_INT_MAX;
}

/* Return true if the call described by INFO with result RES safe to
   optimize (i.e., no undefined behavior), and set RETVAL to the range
   of its return values.  */

static bool
is_call_safe (const sprintf_dom_walker::call_info &info,
	      const format_result &res, bool under4k,
	      unsigned HOST_WIDE_INT retval[2])
{
  if (under4k && !res.posunder4k)
    return false;

  /* The minimum return value.  */
  retval[0] = res.range.min;

  /* The maximum return value is in most cases bounded by RES.RANGE.MAX
     but in cases involving multibyte characters could be as large as
     RES.RANGE.UNLIKELY.  */
  retval[1]
    = res.range.unlikely < res.range.max ? res.range.max : res.range.unlikely;

  /* Adjust the number of bytes which includes the terminating nul
     to reflect the return value of the function which does not.
     Because the valid range of the function is [INT_MIN, INT_MAX],
     a valid range before the adjustment below is [0, INT_MAX + 1]
     (the functions only return negative values on error or undefined
     behavior).  */
  if (retval[0] <= target_int_max () + 1)
    --retval[0];
  if (retval[1] <= target_int_max () + 1)
    --retval[1];

  /* Avoid the return value optimization when the behavior of the call
     is undefined either because any directive may have produced 4K or
     more of output, or the return value exceeds INT_MAX, or because
     the output overflows the destination object (but leave it enabled
     when the function is bounded because then the behavior is well-
     defined).  */
  if (retval[0] == retval[1]
      && (info.bounded || retval[0] < info.objsize)
      && retval[0] <= target_int_max ())
    return true;

  if ((info.bounded || retval[1] < info.objsize)
      && (retval[0] < target_int_max ()
	  && retval[1] < target_int_max ()))
    return true;

  if (!under4k && (info.bounded || retval[0] < info.objsize))
    return true;

  return false;
}

/* Given a suitable result RES of a call to a formatted output function
   described by INFO, substitute the result for the return value of
   the call.  The result is suitable if the number of bytes it represents
   is known and exact.  A result that isn't suitable for substitution may
   have its range set to the range of return values, if that is known.
   Return true if the call is removed and gsi_next should not be performed
   in the caller.  */

static bool
try_substitute_return_value (gimple_stmt_iterator *gsi,
			     const sprintf_dom_walker::call_info &info,
			     const format_result &res)
{
  tree lhs = gimple_get_lhs (info.callstmt);

  /* Set to true when the entire call has been removed.  */
  bool removed = false;

  /* The minimum and maximum return value.  */
  unsigned HOST_WIDE_INT retval[2];
  bool safe = is_call_safe (info, res, true, retval);

  if (safe
      && retval[0] == retval[1]
      /* Not prepared to handle possibly throwing calls here; they shouldn't
	 appear in non-artificial testcases, except when the __*_chk routines
	 are badly declared.  */
      && !stmt_ends_bb_p (info.callstmt))
    {
      tree cst = build_int_cst (lhs ? TREE_TYPE (lhs) : integer_type_node,
				retval[0]);

      if (lhs == NULL_TREE && info.nowrite)
	{
	  /* Remove the call to the bounded function with a zero size
	     (e.g., snprintf(0, 0, "%i", 123)) if there is no lhs.  */
	  unlink_stmt_vdef (info.callstmt);
	  gsi_remove (gsi, true);
	  removed = true;
	}
      else if (info.nowrite)
	{
	  /* Replace the call to the bounded function with a zero size
	     (e.g., snprintf(0, 0, "%i", 123) with the constant result
	     of the function.  */
	  if (!update_call_from_tree (gsi, cst))
	    gimplify_and_update_call_from_tree (gsi, cst);
	  gimple *callstmt = gsi_stmt (*gsi);
	  update_stmt (callstmt);
	}
      else if (lhs)
	{
	  /* Replace the left-hand side of the call with the constant
	     result of the formatted function.  */
	  gimple_call_set_lhs (info.callstmt, NULL_TREE);
	  gimple *g = gimple_build_assign (lhs, cst);
	  gsi_insert_after (gsi, g, GSI_NEW_STMT);
	  update_stmt (info.callstmt);
	}

      if (dump_file)
	{
	  if (removed)
	    fprintf (dump_file, "  Removing call statement.");
	  else
	    {
	      fprintf (dump_file, "  Substituting ");
	      print_generic_expr (dump_file, cst, dump_flags);
	      fprintf (dump_file, " for %s.\n",
		       info.nowrite ? "statement" : "return value");
	    }
	}
    }
  else if (lhs && types_compatible_p (TREE_TYPE (lhs), integer_type_node))
    {
      bool setrange = false;

      if (safe
	  && (info.bounded || retval[1] < info.objsize)
	  && (retval[0] < target_int_max ()
	      && retval[1] < target_int_max ()))
	{
	  /* If the result is in a valid range bounded by the size of
	     the destination set it so that it can be used for subsequent
	     optimizations.  */
	  int prec = TYPE_PRECISION (integer_type_node);

	  wide_int min = wi::shwi (retval[0], prec);
	  wide_int max = wi::shwi (retval[1], prec);
	  set_range_info (lhs, VR_RANGE, min, max);

	  setrange = true;
	}

      if (dump_file)
	{
	  const char *inbounds
	    = (retval[0] < info.objsize
	       ? (retval[1] < info.objsize
		  ? "in" : "potentially out-of")
	       : "out-of");

	  const char *what = setrange ? "Setting" : "Discarding";
	  if (retval[0] != retval[1])
	    fprintf (dump_file,
		     "  %s %s-bounds return value range ["
		     HOST_WIDE_INT_PRINT_UNSIGNED ", "
		     HOST_WIDE_INT_PRINT_UNSIGNED "].\n",
		     what, inbounds, retval[0], retval[1]);
	  else
	    fprintf (dump_file, "  %s %s-bounds return value "
		     HOST_WIDE_INT_PRINT_UNSIGNED ".\n",
		     what, inbounds, retval[0]);
	}
    }

  if (dump_file)
    fputc ('\n', dump_file);

  return removed;
}

/* Try to simplify a s{,n}printf call described by INFO with result
   RES by replacing it with a simpler and presumably more efficient
   call (such as strcpy).  */

static bool
try_simplify_call (gimple_stmt_iterator *gsi,
		   const sprintf_dom_walker::call_info &info,
		   const format_result &res)
{
  unsigned HOST_WIDE_INT dummy[2];
  if (!is_call_safe (info, res, info.retval_used (), dummy))
    return false;

  switch (info.fncode)
    {
    case BUILT_IN_SNPRINTF:
      return gimple_fold_builtin_snprintf (gsi);

    case BUILT_IN_SPRINTF:
      return gimple_fold_builtin_sprintf (gsi);

    default:
      ;
    }

  return false;
}

/* Return the zero-based index of the format string argument of a printf
   like function and set *IDX_ARGS to the first format argument.  When
   no such index exists return UINT_MAX.  */

static unsigned
get_user_idx_format (tree fndecl, unsigned *idx_args)
{
  tree attrs = lookup_attribute ("format", DECL_ATTRIBUTES (fndecl));
  if (!attrs)
    attrs = lookup_attribute ("format", TYPE_ATTRIBUTES (TREE_TYPE (fndecl)));

  if (!attrs)
    return UINT_MAX;

  attrs = TREE_VALUE (attrs);

  tree archetype = TREE_VALUE (attrs);
  if (strcmp ("printf", IDENTIFIER_POINTER (archetype)))
    return UINT_MAX;

  attrs = TREE_CHAIN (attrs);
  tree fmtarg = TREE_VALUE (attrs);

  attrs = TREE_CHAIN (attrs);
  tree elliparg = TREE_VALUE (attrs);

  /* Attribute argument indices are 1-based but we use zero-based.  */
  *idx_args = tree_to_uhwi (elliparg) - 1;
  return tree_to_uhwi (fmtarg) - 1;
}

/* Determine if a GIMPLE CALL is to one of the sprintf-like built-in
   functions and if so, handle it.  Return true if the call is removed
   and gsi_next should not be performed in the caller.  */

bool
sprintf_dom_walker::handle_gimple_call (gimple_stmt_iterator *gsi)
{
  call_info info = call_info ();

  info.callstmt = gsi_stmt (*gsi);
  info.func = gimple_call_fndecl (info.callstmt);
  if (!info.func)
    return false;

  /* Format string argument number (valid for all functions).  */
  unsigned idx_format = UINT_MAX;
  if (gimple_call_builtin_p (info.callstmt, BUILT_IN_NORMAL))
    info.fncode = DECL_FUNCTION_CODE (info.func);
  else
    {
      unsigned idx_args;
      idx_format = get_user_idx_format (info.func, &idx_args);
      if (idx_format == UINT_MAX
	  || idx_format >= gimple_call_num_args (info.callstmt)
	  || idx_args > gimple_call_num_args (info.callstmt)
	  || !POINTER_TYPE_P (TREE_TYPE (gimple_call_arg (info.callstmt,
							  idx_format))))
	return false;
      info.fncode = BUILT_IN_NONE;
      info.argidx = idx_args;
    }

  /* The size of the destination as in snprintf(dest, size, ...).  */
  unsigned HOST_WIDE_INT dstsize = HOST_WIDE_INT_M1U;

  /* The size of the destination determined by __builtin_object_size.  */
  unsigned HOST_WIDE_INT objsize = HOST_WIDE_INT_M1U;

  /* Zero-based buffer size argument number (snprintf and vsnprintf).  */
  unsigned idx_dstsize = UINT_MAX;

  /* Object size argument number (snprintf_chk and vsnprintf_chk).  */
  unsigned idx_objsize = UINT_MAX;

  /* Destinaton argument number (valid for sprintf functions only).  */
  unsigned idx_dstptr = 0;

  switch (info.fncode)
    {
    case BUILT_IN_NONE:
      // User-defined function with attribute format (printf).
      idx_dstptr = -1;
      break;

    case BUILT_IN_FPRINTF:
      // Signature:
      //   __builtin_fprintf (FILE*, format, ...)
      idx_format = 1;
      info.argidx = 2;
      idx_dstptr = -1;
      break;

    case BUILT_IN_FPRINTF_CHK:
      // Signature:
      //   __builtin_fprintf_chk (FILE*, ost, format, ...)
      idx_format = 2;
      info.argidx = 3;
      idx_dstptr = -1;
      break;

    case BUILT_IN_FPRINTF_UNLOCKED:
      // Signature:
      //   __builtin_fprintf_unnlocked (FILE*, format, ...)
      idx_format = 1;
      info.argidx = 2;
      idx_dstptr = -1;
      break;

    case BUILT_IN_PRINTF:
      // Signature:
      //   __builtin_printf (format, ...)
      idx_format = 0;
      info.argidx = 1;
      idx_dstptr = -1;
      break;

    case BUILT_IN_PRINTF_CHK:
      // Signature:
      //   __builtin_printf_chk (ost, format, ...)
      idx_format = 1;
      info.argidx = 2;
      idx_dstptr = -1;
      break;

    case BUILT_IN_PRINTF_UNLOCKED:
      // Signature:
      //   __builtin_printf (format, ...)
      idx_format = 0;
      info.argidx = 1;
      idx_dstptr = -1;
      break;

    case BUILT_IN_SPRINTF:
      // Signature:
      //   __builtin_sprintf (dst, format, ...)
      idx_format = 1;
      info.argidx = 2;
      break;

    case BUILT_IN_SPRINTF_CHK:
      // Signature:
      //   __builtin___sprintf_chk (dst, ost, objsize, format, ...)
      idx_objsize = 2;
      idx_format = 3;
      info.argidx = 4;
      break;

    case BUILT_IN_SNPRINTF:
      // Signature:
      //   __builtin_snprintf (dst, size, format, ...)
      idx_dstsize = 1;
      idx_format = 2;
      info.argidx = 3;
      info.bounded = true;
      break;

    case BUILT_IN_SNPRINTF_CHK:
      // Signature:
      //   __builtin___snprintf_chk (dst, size, ost, objsize, format, ...)
      idx_dstsize = 1;
      idx_objsize = 3;
      idx_format = 4;
      info.argidx = 5;
      info.bounded = true;
      break;

    case BUILT_IN_VFPRINTF:
      // Signature:
      //   __builtin_vprintf (FILE*, format, va_list)
      idx_format = 1;
      info.argidx = -1;
      idx_dstptr = -1;
      break;

    case BUILT_IN_VFPRINTF_CHK:
      // Signature:
      //   __builtin___vfprintf_chk (FILE*, ost, format, va_list)
      idx_format = 2;
      info.argidx = -1;
      idx_dstptr = -1;
      break;

    case BUILT_IN_VPRINTF:
      // Signature:
      //   __builtin_vprintf (format, va_list)
      idx_format = 0;
      info.argidx = -1;
      idx_dstptr = -1;
      break;

    case BUILT_IN_VPRINTF_CHK:
      // Signature:
      //   __builtin___vprintf_chk (ost, format, va_list)
      idx_format = 1;
      info.argidx = -1;
      idx_dstptr = -1;
      break;

    case BUILT_IN_VSNPRINTF:
      // Signature:
      //   __builtin_vsprintf (dst, size, format, va)
      idx_dstsize = 1;
      idx_format = 2;
      info.argidx = -1;
      info.bounded = true;
      break;

    case BUILT_IN_VSNPRINTF_CHK:
      // Signature:
      //   __builtin___vsnprintf_chk (dst, size, ost, objsize, format, va)
      idx_dstsize = 1;
      idx_objsize = 3;
      idx_format = 4;
      info.argidx = -1;
      info.bounded = true;
      break;

    case BUILT_IN_VSPRINTF:
      // Signature:
      //   __builtin_vsprintf (dst, format, va)
      idx_format = 1;
      info.argidx = -1;
      break;

    case BUILT_IN_VSPRINTF_CHK:
      // Signature:
      //   __builtin___vsprintf_chk (dst, ost, objsize, format, va)
      idx_format = 3;
      idx_objsize = 2;
      info.argidx = -1;
      break;

    default:
      return false;
    }

  /* Set the global warning level for this function.  */
  warn_level = info.bounded ? warn_format_trunc : warn_format_overflow;

  /* For all string functions the first argument is a pointer to
     the destination.  */
  tree dstptr = (idx_dstptr < gimple_call_num_args (info.callstmt)
		 ? gimple_call_arg (info.callstmt, 0) : NULL_TREE);

  info.format = gimple_call_arg (info.callstmt, idx_format);

  /* True when the destination size is constant as opposed to the lower
     or upper bound of a range.  */
  bool dstsize_cst_p = true;
  bool posunder4k = true;

  if (idx_dstsize == UINT_MAX)
    {
      /* For non-bounded functions like sprintf, determine the size
	 of the destination from the object or pointer passed to it
	 as the first argument.  */
      dstsize = get_destination_size (dstptr);
    }
  else if (tree size = gimple_call_arg (info.callstmt, idx_dstsize))
    {
      /* For bounded functions try to get the size argument.  */

      if (TREE_CODE (size) == INTEGER_CST)
	{
	  dstsize = tree_to_uhwi (size);
	  /* No object can be larger than SIZE_MAX bytes (half the address
	     space) on the target.
	     The functions are defined only for output of at most INT_MAX
	     bytes.  Specifying a bound in excess of that limit effectively
	     defeats the bounds checking (and on some implementations such
	     as Solaris cause the function to fail with EINVAL).  */
	  if (dstsize > target_size_max () / 2)
	    {
	      /* Avoid warning if -Wstringop-overflow is specified since
		 it also warns for the same thing though only for the
		 checking built-ins.  */
	      if ((idx_objsize == UINT_MAX
		   || !warn_stringop_overflow))
		warning_at (gimple_location (info.callstmt), info.warnopt (),
			    "specified bound %wu exceeds maximum object size "
			    "%wu",
			    dstsize, target_size_max () / 2);
	      /* POSIX requires snprintf to fail if DSTSIZE is greater
		 than INT_MAX.  Even though not all POSIX implementations
		 conform to the requirement, avoid folding in this case.  */
	      posunder4k = false;
	    }
	  else if (dstsize > target_int_max ())
	    {
	      warning_at (gimple_location (info.callstmt), info.warnopt (),
			  "specified bound %wu exceeds %<INT_MAX%>",
			  dstsize);
	      /* POSIX requires snprintf to fail if DSTSIZE is greater
		 than INT_MAX.  Avoid folding in that case.  */
	      posunder4k = false;
	    }
	}
      else if (TREE_CODE (size) == SSA_NAME)
	{
	  /* Try to determine the range of values of the argument
	     and use the greater of the two at level 1 and the smaller
	     of them at level 2.  */
	  value_range *vr = evrp_range_analyzer.get_value_range (size);
	  if (range_int_cst_p (vr))
	    {
	      unsigned HOST_WIDE_INT minsize = TREE_INT_CST_LOW (vr->min ());
	      unsigned HOST_WIDE_INT maxsize = TREE_INT_CST_LOW (vr->max ());
	      dstsize = warn_level < 2 ? maxsize : minsize;

	      if (minsize > target_int_max ())
		warning_at (gimple_location (info.callstmt), info.warnopt (),
			    "specified bound range [%wu, %wu] exceeds "
			    "%<INT_MAX%>",
			    minsize, maxsize);

	      /* POSIX requires snprintf to fail if DSTSIZE is greater
		 than INT_MAX.  Avoid folding if that's possible.  */
	      if (maxsize > target_int_max ())
		posunder4k = false;
	    }
	  else if (vr->varying_p ())
	    {
	      /* POSIX requires snprintf to fail if DSTSIZE is greater
		 than INT_MAX.  Since SIZE's range is unknown, avoid
		 folding.  */
	      posunder4k = false;
	    }

	  /* The destination size is not constant.  If the function is
	     bounded (e.g., snprintf) a lower bound of zero doesn't
	     necessarily imply it can be eliminated.  */
	  dstsize_cst_p = false;
	}
    }

  if (idx_objsize != UINT_MAX)
    if (tree size = gimple_call_arg (info.callstmt, idx_objsize))
      if (tree_fits_uhwi_p (size))
	objsize = tree_to_uhwi (size);

  if (info.bounded && !dstsize)
    {
      /* As a special case, when the explicitly specified destination
	 size argument (to a bounded function like snprintf) is zero
	 it is a request to determine the number of bytes on output
	 without actually producing any.  Pretend the size is
	 unlimited in this case.  */
      info.objsize = HOST_WIDE_INT_MAX;
      info.nowrite = dstsize_cst_p;
    }
  else
    {
      /* For calls to non-bounded functions or to those of bounded
	 functions with a non-zero size, warn if the destination
	 pointer is null.  */
      if (dstptr && integer_zerop (dstptr))
	{
	  /* This is diagnosed with -Wformat only when the null is a constant
	     pointer.  The warning here diagnoses instances where the pointer
	     is not constant.  */
	  location_t loc = gimple_location (info.callstmt);
	  warning_at (EXPR_LOC_OR_LOC (dstptr, loc),
		      info.warnopt (), "%Gnull destination pointer",
		      info.callstmt);
	  return false;
	}

      /* Set the object size to the smaller of the two arguments
	 of both have been specified and they're not equal.  */
      info.objsize = dstsize < objsize ? dstsize : objsize;

      if (info.bounded
	  && dstsize < target_size_max () / 2 && objsize < dstsize
	  /* Avoid warning if -Wstringop-overflow is specified since
	     it also warns for the same thing though only for the
	     checking built-ins.  */
	  && (idx_objsize == UINT_MAX
	      || !warn_stringop_overflow))
	{
	  warning_at (gimple_location (info.callstmt), info.warnopt (),
		      "specified bound %wu exceeds the size %wu "
		      "of the destination object", dstsize, objsize);
	}
    }

  /* Determine if the format argument may be null and warn if not
     and if the argument is null.  */
  if (integer_zerop (info.format)
      && gimple_call_builtin_p (info.callstmt, BUILT_IN_NORMAL))
    {
      location_t loc = gimple_location (info.callstmt);
      warning_at (EXPR_LOC_OR_LOC (info.format, loc),
		  info.warnopt (), "%Gnull format string",
		  info.callstmt);
      return false;
    }

  info.fmtstr = get_format_string (info.format, &info.fmtloc);
  if (!info.fmtstr)
    return false;

  /* The result is the number of bytes output by the formatted function,
     including the terminating NUL.  */
  format_result res = format_result ();

  /* I/O functions with no destination argument (i.e., all forms of fprintf
     and printf) may fail under any conditions.  Others (i.e., all forms of
     sprintf) may only fail under specific conditions determined for each
     directive.  Clear POSUNDER4K for the former set of functions and set
     it to true for the latter (it can only be cleared later, but it is
     never set to true again).  */
  res.posunder4k = posunder4k && dstptr;

  bool success = compute_format_length (info, &res);
  if (res.warned)
    gimple_set_no_warning (info.callstmt, true);

  /* When optimizing and the printf return value optimization is enabled,
     attempt to substitute the computed result for the return value of
     the call.  Avoid this optimization when -frounding-math is in effect
     and the format string contains a floating point directive.  */
  bool call_removed = false;
  if (success && optimize > 0)
    {
      /* Save a copy of the iterator pointing at the call.  The iterator
	 may change to point past the call in try_substitute_return_value
	 but the original value is needed in try_simplify_call.  */
      gimple_stmt_iterator gsi_call = *gsi;

      if (flag_printf_return_value
	  && (!flag_rounding_math || !res.floating))
	call_removed = try_substitute_return_value (gsi, info, res);

      if (!call_removed)
	try_simplify_call (&gsi_call, info, res);
    }

  return call_removed;
}

edge
sprintf_dom_walker::before_dom_children (basic_block bb)
{
  evrp_range_analyzer.enter (bb);
  for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si); )
    {
      /* Iterate over statements, looking for function calls.  */
      gimple *stmt = gsi_stmt (si);

      /* First record ranges generated by this statement.  */
      evrp_range_analyzer.record_ranges_from_stmt (stmt, false);

      if (is_gimple_call (stmt) && handle_gimple_call (&si))
	/* If handle_gimple_call returns true, the iterator is
	   already pointing to the next statement.  */
	continue;

      gsi_next (&si);
    }
  return NULL;
}

void
sprintf_dom_walker::after_dom_children (basic_block bb)
{
  evrp_range_analyzer.leave (bb);
}

/* Execute the pass for function FUN.  */

unsigned int
pass_sprintf_length::execute (function *fun)
{
  init_target_to_host_charmap ();

  calculate_dominance_info (CDI_DOMINATORS);
  bool use_scev = optimize > 0 && flag_printf_return_value;
  if (use_scev)
    {
      loop_optimizer_init (LOOPS_NORMAL);
      scev_initialize ();
    }

  sprintf_dom_walker sprintf_dom_walker;
  sprintf_dom_walker.walk (ENTRY_BLOCK_PTR_FOR_FN (fun));

  if (use_scev)
    {
      scev_finalize ();
      loop_optimizer_finalize ();
    }

  /* Clean up object size info.  */
  fini_object_sizes ();
  return 0;
}

}   /* Unnamed namespace.  */

/* Return a pointer to a pass object newly constructed from the context
   CTXT.  */

gimple_opt_pass *
make_pass_sprintf_length (gcc::context *ctxt)
{
  return new pass_sprintf_length (ctxt);
}
