/* Copyright (C) 2016 Free Software Foundation, Inc.
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
#include "intl.h"

#include "builtins.h"
#include "stor-layout.h"

#include "realmpfr.h"
#include "target.h"

#include "cpplib.h"
#include "input.h"
#include "toplev.h"
#include "substring-locations.h"
#include "diagnostic.h"

/* The likely worst case value of MB_LEN_MAX for the target, large enough
   for UTF-8.  Ideally, this would be obtained by a target hook if it were
   to be used for optimization but it's good enough as is for warnings.  */
#define target_mb_len_max   6

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

struct format_result;

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

  void handle_gimple_call (gimple_stmt_iterator*);

  struct call_info;
  bool compute_format_length (const call_info &, format_result *);
};

bool
pass_sprintf_length::gate (function *)
{
  /* Run the pass iff -Warn-format-length is specified and either
     not optimizing and the pass is being invoked early, or when
     optimizing and the pass is being invoked during optimization
     (i.e., "late").  */
  return ((warn_format_length > 0 || flag_printf_return_value)
	  && (optimize > 0) == fold_return_value);
}

/* The result of a call to a formatted function.  */

struct format_result
{
  /* Number of characters written by the formatted function, exact,
     minimum and maximum when an exact number cannot be determined.
     Setting the minimum to HOST_WIDE_INT_MAX disables all length
     tracking for the remainder of the format string.
     Setting either of the other two members to HOST_WIDE_INT_MAX
     disables the exact or maximum length tracking, respectively,
     but continues to track the maximum.  */
  unsigned HOST_WIDE_INT number_chars;
  unsigned HOST_WIDE_INT number_chars_min;
  unsigned HOST_WIDE_INT number_chars_max;

  /* True when the range given by NUMBER_CHARS_MIN and NUMBER_CHARS_MAX
     can be relied on for value range propagation, false otherwise.
     This means that BOUNDED must not be set if the number of bytes
     produced by any directive is unspecified or implementation-
     defined (unless the implementation's behavior is known and
     determined via a target hook).
     Note that BOUNDED only implies that the length of a function's
     output is known to be within some range, not that it's constant
     and a candidate for string folding.  BOUNDED is a stronger
     guarantee than KNOWNRANGE.  */
  bool bounded;

  /* True when the range above is obtained from known values of
     directive arguments or their bounds and not the result of
     heuristics that depend on warning levels.  It is used to
     issue stricter diagnostics in cases where strings of unknown
     lengths are bounded by the arrays they are determined to
     refer to.  KNOWNRANGE must not be used to set the range of
     the return value of a call.  */
  bool knownrange;

  /* True when the output of the formatted call is constant (and
     thus a candidate for string constant folding).  This is rare
     and typically requires that the arguments of all directives
     are also constant.  CONSTANT implies BOUNDED.  */
  bool constant;

  /* True if no individual directive resulted in more than 4095 bytes
     of output (the total NUMBER_CHARS might be greater).  */
  bool under4k;

  /* True when a floating point directive has been seen in the format
     string.  */
  bool floating;

  /* True when an intermediate result has caused a warning.  Used to
     avoid issuing duplicate warnings while finishing the processing
     of a call.  */
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
  format_result& operator+= (unsigned HOST_WIDE_INT n)
  {
    gcc_assert (n < HOST_WIDE_INT_MAX);

    if (number_chars < HOST_WIDE_INT_MAX)
      number_chars += n;
    if (number_chars_min < HOST_WIDE_INT_MAX)
      number_chars_min += n;
    if (number_chars_max < HOST_WIDE_INT_MAX)
      number_chars_max += n;
    return *this;
  }
};

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

/* Return the constant initial value of DECL if available or DECL
   otherwise.  Same as the synonymous function in c/c-typeck.c.  */

static tree
decl_constant_value (tree decl)
{
  if (/* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  Note that DECL_INITIAL
	 isn't valid for a PARM_DECL.  */
      current_function_decl != 0
      && TREE_CODE (decl) != PARM_DECL
      && !TREE_THIS_VOLATILE (decl)
      && TREE_READONLY (decl)
      && DECL_INITIAL (decl) != 0
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_CONSTANT (DECL_INITIAL (decl))
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR)
    return DECL_INITIAL (decl);
  return decl;
}

/* Given FORMAT, set *PLOC to the source location of the format string
   and return the format string if it is known or null otherwise.  */

static const char*
get_format_string (tree format, location_t *ploc)
{
  if (VAR_P (format))
    {
      /* Pull out a constant value if the front end didn't.  */
      format = decl_constant_value (format);
      STRIP_NOPS (format);
    }

  if (integer_zerop (format))
    {
      /* FIXME: Diagnose null format string if it hasn't been diagnosed
	 by -Wformat (the latter diagnoses only nul pointer constants,
	 this pass can do better).  */
      return NULL;
    }

  HOST_WIDE_INT offset = 0;

  if (TREE_CODE (format) == POINTER_PLUS_EXPR)
    {
      tree arg0 = TREE_OPERAND (format, 0);
      tree arg1 = TREE_OPERAND (format, 1);
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      if (TREE_CODE (arg1) != INTEGER_CST)
	return NULL;

      format = arg0;

      /* POINTER_PLUS_EXPR offsets are to be interpreted signed.  */
      if (!cst_and_fits_in_hwi (arg1))
	return NULL;

      offset = int_cst_value (arg1);
    }

  if (TREE_CODE (format) != ADDR_EXPR)
    return NULL;

  *ploc = EXPR_LOC_OR_LOC (format, input_location);

  format = TREE_OPERAND (format, 0);

  if (TREE_CODE (format) == ARRAY_REF
      && tree_fits_shwi_p (TREE_OPERAND (format, 1))
      && (offset += tree_to_shwi (TREE_OPERAND (format, 1))) >= 0)
    format = TREE_OPERAND (format, 0);

  if (offset < 0)
    return NULL;

  tree array_init;
  tree array_size = NULL_TREE;

  if (VAR_P (format)
      && TREE_CODE (TREE_TYPE (format)) == ARRAY_TYPE
      && (array_init = decl_constant_value (format)) != format
      && TREE_CODE (array_init) == STRING_CST)
    {
      /* Extract the string constant initializer.  Note that this may
	 include a trailing NUL character that is not in the array (e.g.
	 const char a[3] = "foo";).  */
      array_size = DECL_SIZE_UNIT (format);
      format = array_init;
    }

  if (TREE_CODE (format) != STRING_CST)
    return NULL;

  if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (format))) != char_type_node)
    {
      /* Wide format string.  */
      return NULL;
    }

  const char *fmtstr = TREE_STRING_POINTER (format);
  unsigned fmtlen = TREE_STRING_LENGTH (format);

  if (array_size)
    {
      /* Variable length arrays can't be initialized.  */
      gcc_assert (TREE_CODE (array_size) == INTEGER_CST);

      if (tree_fits_shwi_p (array_size))
	{
	  HOST_WIDE_INT array_size_value = tree_to_shwi (array_size);
	  if (array_size_value > 0
	      && array_size_value == (int) array_size_value
	      && fmtlen > array_size_value)
	    fmtlen = array_size_value;
	}
    }
  if (offset)
    {
      if (offset >= fmtlen)
	return NULL;

      fmtstr += offset;
      fmtlen -= offset;
    }

  if (fmtlen < 1 || fmtstr[--fmtlen] != 0)
    {
      /* FIXME: Diagnose an unterminated format string if it hasn't been
	 diagnosed by -Wformat.  Similarly to a null format pointer,
	 -Wformay diagnoses only nul pointer constants, this pass can
	 do better).  */
      return NULL;
    }

  return fmtstr;
}

/* The format_warning_at_substring function is not used here in a way
   that makes using attribute format viable.  Suppress the warning.  */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsuggest-attribute=format"

/* For convenience and brevity.  */

static bool
  (* const fmtwarn) (const substring_loc &, const source_range *,
		     const char *, int, const char *, ...)
  = format_warning_at_substring;

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


/* A minimum and maximum number of bytes.  */

struct result_range
{
  unsigned HOST_WIDE_INT min, max;
};

/* Description of the result of conversion either of a single directive
   or the whole format string.  */

struct fmtresult
{
  fmtresult ()
  : argmin (), argmax (), knownrange (), bounded (), constant ()
  {
    range.min = range.max = HOST_WIDE_INT_MAX;
  }

  /* The range a directive's argument is in.  */
  tree argmin, argmax;

  /* The minimum and maximum number of bytes that a directive
     results in on output for an argument in the range above.  */
  result_range range;

  /* True when the range above is obtained from a known value of
     a directive's argument or its bounds and not the result of
     heuristics that depend on warning levels.  */
  bool knownrange;

  /* True when the range is the result of an argument determined
     to be bounded to a subrange of its type or value (such as by
     value range propagation or the width of the formt directive),
     false otherwise.  */
  bool bounded;

  /* True when the output of a directive is constant.  This is rare
     and typically requires that the argument(s) of the directive
     are also constant (such as determined by constant propagation,
     though not value range propagation).  */
  bool constant;
};

/* Description of a conversion specification.  */

struct conversion_spec
{
  /* A bitmap of flags, one for each character.  */
  unsigned flags[256 / sizeof (int)];
  /* Numeric width as in "%8x".  */
  int width;
  /* Numeric precision as in "%.32s".  */
  int precision;

  /* Width specified via the '*' character.  */
  tree star_width;
  /* Precision specified via the asterisk.  */
  tree star_precision;

  /* Length modifier.  */
  format_lengths modifier;

  /* Format specifier character.  */
  char specifier;

  /* Numeric width was given.  */
  unsigned have_width: 1;
  /* Numeric precision was given.  */
  unsigned have_precision: 1;
  /* Non-zero when certain flags should be interpreted even for a directive
     that normally doesn't accept them (used when "%p" with flags such as
     space or plus is interepreted as a "%x".  */
  unsigned force_flags: 1;

  /* Format conversion function that given a conversion specification
     and an argument returns the formatting result.  */
  fmtresult (*fmtfunc) (const conversion_spec &, tree);

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
   the INTEGER_CST tree node X in BASE.  PLUS indicates whether 1 for
   a plus sign should be added for positive numbers, and PREFIX whether
   the length of an octal ('O') or hexadecimal ('0x') prefix should be
   added for nonzero numbers.  Return -1 if X cannot be represented.  */

static int
tree_digits (tree x, int base, bool plus, bool prefix)
{
  unsigned HOST_WIDE_INT absval;

  int res;

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
	  if (i < 0)
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

  res += ilog (absval, base);

  if (prefix && absval)
    {
      if (base == 8)
	res += 1;
      else if (base == 16)
	res += 2;
    }

  return res;
}

/* Given the formatting result described by RES and NAVAIL, the number
   of available in the destination, return the number of bytes remaining
   in the destination.  */

static inline result_range
bytes_remaining (unsigned HOST_WIDE_INT navail, const format_result &res)
{
  result_range range;

  if (HOST_WIDE_INT_MAX <= navail)
    {
      range.min = range.max = navail;
      return range;
    }

  if (res.number_chars < navail)
    {
      range.min = range.max = navail - res.number_chars;
    }
  else if (res.number_chars_min < navail)
    {
      range.max = navail - res.number_chars_min;
    }
  else
    range.max = 0;

  if (res.number_chars_max < navail)
    range.min = navail - res.number_chars_max;
  else
    range.min = 0;

  return range;
}

/* Given the formatting result described by RES and NAVAIL, the number
   of available in the destination, return the minimum number of bytes
   remaining in the destination.  */

static inline unsigned HOST_WIDE_INT
min_bytes_remaining (unsigned HOST_WIDE_INT navail, const format_result &res)
{
  if (HOST_WIDE_INT_MAX <= navail)
    return navail;

  if (1 < warn_format_length || res.bounded)
    {
      /* At level 2, or when all directives output an exact number
	 of bytes or when their arguments were bounded by known
	 ranges, use the greater of the two byte counters if it's
	 valid to compute the result.  */
      if (res.number_chars_max < HOST_WIDE_INT_MAX)
	navail -= res.number_chars_max;
      else if (res.number_chars < HOST_WIDE_INT_MAX)
	navail -= res.number_chars;
      else if (res.number_chars_min < HOST_WIDE_INT_MAX)
	navail -= res.number_chars_min;
    }
  else
    {
      /* At level 1 use the smaller of the byte counters to compute
	 the result.  */
      if (res.number_chars < HOST_WIDE_INT_MAX)
	navail -= res.number_chars;
      else if (res.number_chars_min < HOST_WIDE_INT_MAX)
	navail -= res.number_chars_min;
      else if (res.number_chars_max < HOST_WIDE_INT_MAX)
	navail -= res.number_chars_max;
    }

  if (navail > HOST_WIDE_INT_MAX)
    navail = 0;

  return navail;
}

/* Description of a call to a formatted function.  */

struct pass_sprintf_length::call_info
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
     writing any.  */
  bool nowrite;
};

/* Return the result of formatting the '%%' directive.  */

static fmtresult
format_percent (const conversion_spec &, tree)
{
  fmtresult res;
  res.argmin = res.argmax = NULL_TREE;
  res.range.min = res.range.max = 1;
  res.bounded = res.constant = true;
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

/* Set *PWIDTH and *PPREC according to the width and precision specified
   in SPEC.  Each is set to HOST_WIDE_INT_MIN when the corresponding
   field is specified but unknown, to zero for width and -1 for precision,
   respectively when it's not specified, or to a non-negative value
   corresponding to the known value.  */

static void
get_width_and_precision (const conversion_spec &spec,
			 HOST_WIDE_INT *pwidth, HOST_WIDE_INT *pprec)
{
  HOST_WIDE_INT width = spec.have_width ? spec.width : 0;
  HOST_WIDE_INT prec = spec.have_precision ? spec.precision : -1;

  if (spec.star_width)
    {
      if (TREE_CODE (spec.star_width) == INTEGER_CST)
	{
	  width = tree_to_shwi (spec.star_width);
	  if (width < 0)
	    {
	      if (width == HOST_WIDE_INT_MIN)
		{
		  /* Avoid undefined behavior due to negating a minimum.
		     This case will be diagnosed since it will result in
		     more than INT_MAX bytes on output, either by the
		     directive itself (when INT_MAX < HOST_WIDE_INT_MAX)
		     or by the format function itself.  */
		  width = HOST_WIDE_INT_MAX;
		}
	      else
		width = -width;
	    }
	}
      else
	width = HOST_WIDE_INT_MIN;
    }

  if (spec.star_precision)
    {
      if (TREE_CODE (spec.star_precision) == INTEGER_CST)
	{
	  prec = tree_to_shwi (spec.star_precision);
	  if (prec < 0)
	    prec = 0;
	}
      else
	prec = HOST_WIDE_INT_MIN;
    }

  *pwidth = width;
  *pprec = prec;
}

/* Return a range representing the minimum and maximum number of bytes
   that the conversion specification SPEC will write on output for the
   integer argument ARG when non-null.  ARG may be null (for vararg
   functions).  */

static fmtresult
format_integer (const conversion_spec &spec, tree arg)
{
  tree intmax_type_node;
  tree uintmax_type_node;

  /* Set WIDTH and PRECISION based on the specification.  */
  HOST_WIDE_INT width;
  HOST_WIDE_INT prec;
  get_width_and_precision (spec, &width, &prec);

  bool sign = spec.specifier == 'd' || spec.specifier == 'i';

  /* The type of the "formal" argument expected by the directive.  */
  tree dirtype = NULL_TREE;

  /* Determine the expected type of the argument from the length
     modifier.  */
  switch (spec.modifier)
    {
    case FMT_LEN_none:
      if (spec.specifier == 'p')
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

      /* Base to format the number in.  */
      int base;

      /* True when a signed conversion is preceded by a sign or space.  */
      bool maybesign;

      switch (spec.specifier)
	{
	case 'd':
	case 'i':
	  /* Space is only effective for signed conversions.  */
	  maybesign = spec.get_flag (' ');
	  base = 10;
	  break;
	case 'u':
	  maybesign = spec.force_flags ? spec.get_flag (' ') : false;
	  base = 10;
	  break;
	case 'o':
	  maybesign = spec.force_flags ? spec.get_flag (' ') : false;
	  base = 8;
	  break;
	case 'X':
	case 'x':
	  maybesign = spec.force_flags ? spec.get_flag (' ') : false;
	  base = 16;
	  break;
	default:
	  gcc_unreachable ();
	}

      int len;

      if ((prec == HOST_WIDE_INT_MIN || prec == 0) && integer_zerop (arg))
	{
	  /* As a special case, a precision of zero with an argument
	     of zero results in zero bytes regardless of flags (with
	     width having the normal effect).  This must extend to
	     the case of a specified precision with an unknown value
	     because it can be zero.  */
	  len = 0;
	}
      else
	{
	  /* Convert the argument to the type of the directive.  */
	  arg = fold_convert (dirtype, arg);

	  maybesign |= spec.get_flag ('+');

	  /* True when a conversion is preceded by a prefix indicating the base
	     of the argument (octal or hexadecimal).  */
	  bool maybebase = spec.get_flag ('#');
	  len = tree_digits (arg, base, maybesign, maybebase);

	  if (len < prec)
	    len = prec;
	}

      if (len < width)
	len = width;

      /* The minimum and maximum number of bytes produced by the directive.  */
      fmtresult res;

      res.range.min = len;

      /* The upper bound of the number of bytes is unlimited when either
	 width or precision is specified but its value is unknown, and
	 the same as the lower bound otherwise.  */
      if (width == HOST_WIDE_INT_MIN || prec == HOST_WIDE_INT_MIN)
	{
	  res.range.max = HOST_WIDE_INT_MAX;
	}
      else
	{
	  res.range.max = len;
	  res.bounded = true;
	  res.constant = true;
	  res.knownrange = true;
	  res.bounded = true;
	}

      return res;
    }
  else if (TREE_CODE (TREE_TYPE (arg)) == INTEGER_TYPE
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
      && TREE_CODE (argtype) == INTEGER_TYPE)
    {
      /* Try to determine the range of values of the integer argument
	 (range information is not available for pointers).  */
      wide_int min, max;
      enum value_range_type range_type = get_range_info (arg, &min, &max);
      if (range_type == VR_RANGE)
	{
	  res.argmin = build_int_cst (argtype, wi::fits_uhwi_p (min)
				      ? min.to_uhwi () : min.to_shwi ());
	  res.argmax = build_int_cst (argtype, wi::fits_uhwi_p (max)
				      ? max.to_uhwi () : max.to_shwi ());

	  /* For a range with a negative lower bound and a non-negative
	     upper bound, use one to determine the minimum number of bytes
	     on output and whichever of the two bounds that results in
	     the greater number of bytes on output for the upper bound.
	     For example, for ARG in the range of [-3, 123], use 123 as
	     the upper bound for %i but -3 for %u.  */
	  if (wi::neg_p (min) && !wi::neg_p (max))
	    {
	      argmin = res.argmin;
	      argmax = res.argmax;
	      int minbytes = format_integer (spec, res.argmin).range.min;
	      int maxbytes = format_integer (spec, res.argmax).range.max;
	      if (maxbytes < minbytes)
		argmax = res.argmin;

	      argmin = integer_zero_node;
	    }
	  else
	    {
	      argmin = res.argmin;
	      argmax = res.argmax;
	    }

	  /* The argument is bounded by the known range of values
	     determined by Value Range Propagation.  */
	  res.bounded = true;
	  res.knownrange = true;
	}
      else if (range_type == VR_ANTI_RANGE)
	{
	  /* Handle anti-ranges if/when bug 71690 is resolved.  */
	}
      else if (range_type == VR_VARYING)
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
		  return format_integer (spec, arg);
		}

	      if (code == NOP_EXPR)
		{
		  tree type = TREE_TYPE (gimple_assign_rhs1 (def));
		  if (TREE_CODE (type) == INTEGER_TYPE
		      || TREE_CODE (type) == POINTER_TYPE)
		    argtype = type;
		}
	    }
	}
    }

  if (!argmin)
    {
      /* For an unknown argument (e.g., one passed to a vararg function)
	 or one whose value range cannot be determined, create a T_MIN
	 constant if the argument's type is signed and T_MAX otherwise,
	 and use those to compute the range of bytes that the directive
	 can output.  When precision is specified but unknown, use zero
	 as the minimum since it results in no bytes on output (unless
	 width is specified to be greater than 0).  */
      argmin = build_int_cst (argtype, prec != HOST_WIDE_INT_MIN);

      int typeprec = TYPE_PRECISION (dirtype);
      int argprec = TYPE_PRECISION (argtype);

      if (argprec < typeprec)
	{
	  if (POINTER_TYPE_P (argtype))
	    argmax = build_all_ones_cst (argtype);
	  else if (TYPE_UNSIGNED (argtype))
	    argmax = TYPE_MAX_VALUE (argtype);
	  else
	    argmax = TYPE_MIN_VALUE (argtype);
	}
      else
	{
	  if (POINTER_TYPE_P (dirtype))
	    argmax = build_all_ones_cst (dirtype);
	  else if (TYPE_UNSIGNED (dirtype))
	    argmax = TYPE_MAX_VALUE (dirtype);
	  else
	    argmax = TYPE_MIN_VALUE (dirtype);
	}

      res.argmin = argmin;
      res.argmax = argmax;
    }

  /* Recursively compute the minimum and maximum from the known range,
     taking care to swap them if the lower bound results in longer
     output than the upper bound (e.g., in the range [-1, 0].  */
  res.range.min = format_integer (spec, argmin).range.min;
  res.range.max = format_integer (spec, argmax).range.max;

  /* The result is bounded either when the argument is determined to be
     (e.g., when it's within some range) or when the minimum and maximum
     are the same.  That can happen here for example when the specified
     width is as wide as the greater of MIN and MAX, as would be the case
     with sprintf (d, "%08x", x) with a 32-bit integer x.  */
  res.bounded |= res.range.min == res.range.max;

  if (res.range.max < res.range.min)
    {
      unsigned HOST_WIDE_INT tmp = res.range.max;
      res.range.max = res.range.min;
      res.range.min = tmp;
    }

  return res;
}

/* Return the number of bytes to format using the format specifier
   SPEC the largest value in the real floating TYPE.  */

static int
format_floating_max (tree type, char spec, int prec = -1)
{
  machine_mode mode = TYPE_MODE (type);

  /* IBM Extended mode.  */
  if (MODE_COMPOSITE_P (mode))
    mode = DFmode;

  /* Get the real type format desription for the target.  */
  const real_format *rfmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE rv;

  {
    char buf[256];
    get_max_float (rfmt, buf, sizeof buf);
    real_from_string (&rv, buf);
  }

  /* Convert the GCC real value representation with the precision
     of the real type to the mpfr_t format with the GCC default
     round-to-nearest mode.  */
  mpfr_t x;
  mpfr_init2 (x, rfmt->p);
  mpfr_from_real (x, &rv, GMP_RNDN);

  int n;

  if (-1 < prec)
    {
      const char fmt[] = { '%', '.', '*', 'R', spec, '\0' };
      n = mpfr_snprintf (NULL, 0, fmt, prec, x);
    }
  else
    {
      const char fmt[] = { '%', 'R', spec, '\0' };
      n = mpfr_snprintf (NULL, 0, fmt, x);
    }

  /* Return a value one greater to account for the leading minus sign.  */
  return n + 1;
}

/* Return a range representing the minimum and maximum number of bytes
   that the conversion specification SPEC will output for any argument
   given the WIDTH and PRECISION (extracted from SPEC).  This function
   is used when the directive argument or its value isn't known.  */

static fmtresult
format_floating (const conversion_spec &spec, int width, int prec)
{
  tree type;
  bool ldbl = false;

  switch (spec.modifier)
    {
    case FMT_LEN_l:
    case FMT_LEN_none:
      type = double_type_node;
      break;

    case FMT_LEN_L:
      type = long_double_type_node;
      ldbl = true;
      break;

    case FMT_LEN_ll:
      type = long_double_type_node;
      ldbl = true;
      break;

    default:
      return fmtresult ();
    }

  /* The minimum and maximum number of bytes produced by the directive.  */
  fmtresult res;

  /* Log10 of of the maximum number of exponent digits for the type.  */
  int logexpdigs = 2;

  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2)
    {
      /* The base in which the exponent is represented should always
	 be 2 in GCC.  */

      const double log10_2 = .30102999566398119521;

      /* Compute T_MAX_EXP for base 2.  */
      int expdigs = REAL_MODE_FORMAT (TYPE_MODE (type))->emax * log10_2;
      logexpdigs = ilog (expdigs, 10);
    }

  switch (spec.specifier)
    {
    case 'A':
    case 'a':
      {
	/* The minimum output is "0x.p+0".  */
	res.range.min = 6 + (prec > 0 ? prec : 0);
	res.range.max = (width == INT_MIN
			 ? HOST_WIDE_INT_MAX
			 : format_floating_max (type, 'a', prec));

	/* The output of "%a" is fully specified only when precision
	   is explicitly specified and width isn't unknown.  */
	res.bounded = INT_MIN != width && -1 < prec;
	break;
      }

    case 'E':
    case 'e':
      {
	bool sign = spec.get_flag ('+') || spec.get_flag (' ');
	/* The minimum output is "[-+]1.234567e+00" regardless
	   of the value of the actual argument.  */
	res.range.min = (sign
			 + 1 /* unit */ + (prec < 0 ? 7 : prec ? prec + 1 : 0)
			 + 2 /* e+ */ + 2);
	/* Unless width is uknown the maximum output is the minimum plus
	   sign (unless already included), plus the difference between
	   the minimum exponent of 2 and the maximum exponent for the type.  */
	res.range.max = (width == INT_MIN
			 ? HOST_WIDE_INT_M1U
			 : res.range.min + !sign + logexpdigs - 2);

	/* "%e" is fully specified and the range of bytes is bounded
	   unless width is unknown.  */
	res.bounded = INT_MIN != width;
	break;
      }

    case 'F':
    case 'f':
      {
	/* The minimum output is "1.234567" regardless of the value
	   of the actual argument.  */
	res.range.min = 2 + (prec < 0 ? 6 : prec);

	/* Compute the maximum just once.  */
	const int f_max[] = {
	  format_floating_max (double_type_node, 'f', prec),
	  format_floating_max (long_double_type_node, 'f', prec)
	};
	res.range.max = width == INT_MIN ? HOST_WIDE_INT_MAX : f_max [ldbl];

	/* "%f" is fully specified and the range of bytes is bounded
	   unless width is unknown.  */
	res.bounded = INT_MIN != width;
	break;
      }
    case 'G':
    case 'g':
      {
	/* The minimum is the same as for '%F'.  */
	res.range.min = 2 + (prec < 0 ? 6 : prec);

	/* Compute the maximum just once.  */
	const int g_max[] = {
	  format_floating_max (double_type_node, 'g', prec),
	  format_floating_max (long_double_type_node, 'g', prec)
	};
	res.range.max = width == INT_MIN ? HOST_WIDE_INT_MAX : g_max [ldbl];

	/* "%g" is fully specified and the range of bytes is bounded
	   unless width is unknown.  */
	res.bounded = INT_MIN != width;
	break;
      }

    default:
      return fmtresult ();
    }

  if (width > 0)
    {
      if (res.range.min < (unsigned)width)
	res.range.min = width;
      if (res.range.max < (unsigned)width)
	res.range.max = width;
    }

  return res;
}

/* Return a range representing the minimum and maximum number of bytes
   that the conversion specification SPEC will write on output for the
   floating argument ARG.  */

static fmtresult
format_floating (const conversion_spec &spec, tree arg)
{
  /* Set WIDTH to -1 when it's not specified, to INT_MIN when it is
     specified by the asterisk to an unknown value, and otherwise to
     a non-negative value corresponding to the specified width.  */
  int width = -1;
  int prec = -1;

  /* The minimum and maximum number of bytes produced by the directive.  */
  fmtresult res;
  res.constant = arg && TREE_CODE (arg) == REAL_CST;

  if (spec.have_width)
    width = spec.width;
  else if (spec.star_width)
    {
      if (TREE_CODE (spec.star_width) == INTEGER_CST)
	{
	  width = tree_to_shwi (spec.star_width);
	  if (width < 0)
	    width = -width;
	}
      else
	width = INT_MIN;
    }

  if (spec.have_precision)
    prec = spec.precision;
  else if (spec.star_precision)
    {
      if (TREE_CODE (spec.star_precision) == INTEGER_CST)
	prec = tree_to_shwi (spec.star_precision);
      else
	{
	  /* FIXME: Handle non-constant precision.  */
	  res.range.min = res.range.max = HOST_WIDE_INT_M1U;
	  return res;
	}
    }
  else if (res.constant && TOUPPER (spec.specifier) != 'A')
    {
      /* Specify the precision explicitly since mpfr_sprintf defaults
	 to zero.  */
      prec = 6;
    }

  if (res.constant)
    {
      /* Set up an array to easily iterate over.  */
      unsigned HOST_WIDE_INT* const minmax[] = {
	&res.range.min, &res.range.max
      };

      /* Get the real type format desription for the target.  */
      const REAL_VALUE_TYPE *rvp = TREE_REAL_CST_PTR (arg);
      const real_format *rfmt = REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (arg)));

      /* Convert the GCC real value representation with the precision
	 of the real type to the mpfr_t format with the GCC default
	 round-to-nearest mode.  */
      mpfr_t mpfrval;
      mpfr_init2 (mpfrval, rfmt->p);
      mpfr_from_real (mpfrval, rvp, GMP_RNDN);

      char fmtstr [40];
      char *pfmt = fmtstr;
      *pfmt++ = '%';

      /* Append flags.  */
      for (const char *pf = "-+ #0"; *pf; ++pf)
	if (spec.get_flag (*pf))
	  *pfmt++ = *pf;

      /* Append width when specified and precision.  */
      if (-1 < width)
	pfmt += sprintf (pfmt, "%i", width);
      if (-1 < prec)
	pfmt += sprintf (pfmt, ".%i", prec);

      /* Append the MPFR 'R' floating type specifier (no length modifier
	 is necessary or allowed by MPFR for mpfr_t values).  */
      *pfmt++ = 'R';

      /* Save the position of the MPFR rounding specifier and skip over
	 it.  It will be set in each iteration in the loop below.  */
      char* const rndspec = pfmt++;

      /* Append the C type specifier and nul-terminate.  */
      *pfmt++ = spec.specifier;
      *pfmt = '\0';

      for (int i = 0; i != sizeof minmax / sizeof *minmax; ++i)
	{
	  /* Use the MPFR rounding specifier to round down in the first
	     iteration and then up.  In most but not all cases this will
	     result in the same number of bytes.  */
	  *rndspec = "DU"[i];

	  /* Format it and store the result in the corresponding
	     member of the result struct.  */
	  *minmax[i] = mpfr_snprintf (NULL, 0, fmtstr, mpfrval);
	}

      /* The range of output is known even if the result isn't bounded.  */
      if (width == INT_MIN)
	{
	  res.knownrange = false;
	  res.range.max = HOST_WIDE_INT_MAX;
	}
      else
	res.knownrange = true;

      /* The output of all directives except "%a" is fully specified
	 and so the result is bounded unless it exceeds INT_MAX.
	 For "%a" the output is fully specified only when precision
	 is explicitly specified.  */
      res.bounded = (res.knownrange
		     && (TOUPPER (spec.specifier) != 'A'
			 || (0 <= prec && (unsigned) prec < target_int_max ()))
		     && res.range.min < target_int_max ());

      return res;
    }

  return format_floating (spec, width, prec);
}

/* Return a FMTRESULT struct set to the lengths of the shortest and longest
   strings referenced by the expression STR, or (-1, -1) when not known.
   Used by the format_string function below.  */

static fmtresult
get_string_length (tree str)
{
  if (!str)
    return fmtresult ();

  if (tree slen = c_strlen (str, 1))
    {
      /* Simply return the length of the string.  */
      fmtresult res;
      res.range.min = res.range.max = tree_to_shwi (slen);
      res.bounded = true;
      res.constant = true;
      res.knownrange = true;
      return res;
    }

  /* Determine the length of the shortest and longest string referenced
     by STR.  Strings of unknown lengths are bounded by the sizes of
     arrays that subexpressions of STR may refer to.  Pointers that
     aren't known to point any such arrays result in LENRANGE[1] set
     to SIZE_MAX.  */
  tree lenrange[2];
  get_range_strlen (str, lenrange);

  if (lenrange [0] || lenrange [1])
    {
      fmtresult res;

      res.range.min = (tree_fits_uhwi_p (lenrange[0])
		       ? tree_to_uhwi (lenrange[0]) : 1 < warn_format_length);
      res.range.max = (tree_fits_uhwi_p (lenrange[1])
		       ? tree_to_uhwi (lenrange[1]) : HOST_WIDE_INT_M1U);

      /* Set RES.BOUNDED to true if and only if all strings referenced
	 by STR are known to be bounded (though not necessarily by their
	 actual length but perhaps by their maximum possible length).  */
      res.bounded = res.range.max < target_int_max ();
      res.knownrange = res.bounded;

      /* Set RES.CONSTANT to false even though that may be overly
	 conservative in rare cases like: 'x ? a : b' where a and
	 b have the same lengths and consist of the same characters.  */
      res.constant = false;

      return res;
    }

  return get_string_length (NULL_TREE);
}

/* Return the minimum and maximum number of characters formatted
   by the '%c' and '%s' format directives and ther wide character
   forms for the argument ARG.  ARG can be null (for functions
   such as vsprinf).  */

static fmtresult
format_string (const conversion_spec &spec, tree arg)
{
  /* Set WIDTH and PRECISION based on the specification.  */
  HOST_WIDE_INT width;
  HOST_WIDE_INT prec;
  get_width_and_precision (spec, &width, &prec);

  fmtresult res;

  /* The maximum number of bytes for an unknown wide character argument
     to a "%lc" directive adjusted for precision but not field width.
     6 is the longest UTF-8 sequence for a single wide character.  */
  const unsigned HOST_WIDE_INT max_bytes_for_unknown_wc
    = (0 <= prec ? prec : 1 < warn_format_length ? 6 : 1);

  /* The maximum number of bytes for an unknown string argument to either
     a "%s" or "%ls" directive adjusted for precision but not field width.  */
  const unsigned HOST_WIDE_INT max_bytes_for_unknown_str
    = (0 <= prec ? prec : 1 < warn_format_length);

  /* The result is bounded unless overriddden for a non-constant string
     of an unknown length.  */
  bool bounded = true;

  if (spec.specifier == 'c')
    {
      if (spec.modifier == FMT_LEN_l)
	{
	  /* Positive if the argument is a wide NUL character?  */
	  int nul = (arg && TREE_CODE (arg) == INTEGER_CST
		     ? integer_zerop (arg) : -1);

	  /* A '%lc' directive is the same as '%ls' for a two element
	     wide string character with the second element of NUL, so
	     when the character is unknown the minimum number of bytes
	     is the smaller of either 0 (at level 1) or 1 (at level 2)
	     and WIDTH, and the maximum is MB_CUR_MAX in the selected
	     locale, which is unfortunately, unknown.  */
	  res.range.min = 1 == warn_format_length ? !nul : nul < 1;
	  res.range.max = max_bytes_for_unknown_wc;
	  /* The range above is good enough to issue warnings but not
	     for value range propagation, so clear BOUNDED.  */
	  res.bounded = false;
	}
      else
	{
	  /* A plain '%c' directive.  Its ouput is exactly 1.  */
	  res.range.min = res.range.max = 1;
	  res.bounded = true;
	  res.knownrange = true;
	  res.constant = arg && TREE_CODE (arg) == INTEGER_CST;
	}
    }
  else   /* spec.specifier == 's' */
    {
      /* Compute the range the argument's length can be in.  */
      fmtresult slen = get_string_length (arg);
      if (slen.constant)
	{
	  gcc_checking_assert (slen.range.min == slen.range.max);

	  /* A '%s' directive with a string argument with constant length.  */
	  res.range = slen.range;

	  /* The output of "%s" and "%ls" directives with a constant
	     string is in a known range unless width of an unknown value
	     is specified.  For "%s" it is the length of the string.  For
	     "%ls" it is in the range [length, length * MB_LEN_MAX].
	     (The final range can be further constrained by width and
	     precision but it's always known.)  */
	  res.knownrange = -1 < width;

	  if (spec.modifier == FMT_LEN_l)
	    {
	      bounded = false;

	      if (warn_format_length > 1)
		{
		  /* Leave the minimum number of bytes the wide string
		     converts to equal to its length and set the maximum
		     to the worst case length which is the string length
		     multiplied by MB_LEN_MAX.  */

		  /* It's possible to be smarter about computing the maximum
		     by scanning the wide string for any 8-bit characters and
		     if it contains none, using its length for the maximum.
		     Even though this would be simple to do it's unlikely to
		     be worth it when dealing with wide characters.  */
		  res.range.max *= target_mb_len_max;
		}

	      /* For a wide character string, use precision as the maximum
		 even if precision is greater than the string length since
		 the number of bytes the string converts to may be greater
		 (due to MB_CUR_MAX).  */
	      if (0 <= prec)
		res.range.max = prec;
	    }
	  else if (0 <= width)
	    {
	      /* The output of a "%s" directive with a constant argument
		 and constant or no width is bounded.  It is constant if
		 precision is either not specified or it is specified and
		 its value is known.  */
	      res.bounded = true;
	      res.constant = prec != HOST_WIDE_INT_MIN;
	    }
	  else if (width == HOST_WIDE_INT_MIN)
	    {
	      /* Specified but unknown width makes the output unbounded.  */
	      res.range.max = HOST_WIDE_INT_MAX;
	    }

	  if (0 <= prec && (unsigned HOST_WIDE_INT)prec < res.range.min)
	    {
	      res.range.min = prec;
	      res.range.max = prec;
	    }
	  else if (prec == HOST_WIDE_INT_MIN)
	    {
	      /* When precision is specified but not known the lower
		 bound is assumed to be as low as zero.  */
	      res.range.min = 0;
	    }
	}
      else
	{
	  /* For a '%s' and '%ls' directive with a non-constant string,
	     the minimum number of characters is the greater of WIDTH
	     and either 0 in mode 1 or the smaller of PRECISION and 1
	     in mode 2, and the maximum is PRECISION or -1 to disable
	     tracking.  */

	  if (0 <= prec)
	    {
	      if (slen.range.min >= target_int_max ())
		slen.range.min = 0;
	      else if ((unsigned HOST_WIDE_INT)prec < slen.range.min)
		slen.range.min = prec;

	      if ((unsigned HOST_WIDE_INT)prec < slen.range.max
		  || slen.range.max >= target_int_max ())
		slen.range.max = prec;
	    }
	  else if (slen.range.min >= target_int_max ())
	    {
	      slen.range.min = max_bytes_for_unknown_str;
	      slen.range.max = max_bytes_for_unknown_str;
	      bounded = false;
	    }

	  res.range = slen.range;

	  /* The output is considered bounded when a precision has been
	     specified to limit the number of bytes or when the number
	     of bytes is known or contrained to some range.  */
	  res.bounded = 0 <= prec || slen.bounded;
	  res.knownrange = slen.knownrange;
	  res.constant = false;
	}
    }

  /* Adjust the lengths for field width.  */
  if (0 < width)
    {
      if (res.range.min < (unsigned HOST_WIDE_INT)width)
	res.range.min = width;

      if (res.range.max < (unsigned HOST_WIDE_INT)width)
	res.range.max = width;

      /* Adjust BOUNDED if width happens to make them equal.  */
      if (res.range.min == res.range.max && res.range.min < target_int_max ()
	  && bounded)
	res.bounded = true;
    }

  /* When precision is specified the range of characters on output
     is known to be bounded by it.  */
  if (-1 < width && -1 < prec)
    res.knownrange = true;

  return res;
}

/* Compute the length of the output resulting from the conversion
   specification SPEC with the argument ARG in a call described by INFO
   and update the overall result of the call in *RES.  The format directive
   corresponding to SPEC starts at CVTBEG and is CVTLEN characters long.  */

static void
format_directive (const pass_sprintf_length::call_info &info,
		  format_result *res, const char *cvtbeg, size_t cvtlen,
		  const conversion_spec &spec, tree arg)
{
  /* Offset of the beginning of the directive from the beginning
     of the format string.  */
  size_t offset = cvtbeg - info.fmtstr;

  /* Create a location for the whole directive from the % to the format
     specifier.  */
  substring_loc dirloc (info.fmtloc, TREE_TYPE (info.format),
			offset, offset, offset + cvtlen - 1);

  /* Also create a location range for the argument if possible.
     This doesn't work for integer literals or function calls.  */
  source_range argrange;
  source_range *pargrange;
  if (arg && CAN_HAVE_LOCATION_P (arg))
    {
      argrange = EXPR_LOCATION_RANGE (arg);
      pargrange = &argrange;
    }
  else
    pargrange = NULL;

  /* Bail when there is no function to compute the output length,
     or when minimum length checking has been disabled.   */
  if (!spec.fmtfunc || res->number_chars_min >= HOST_WIDE_INT_MAX)
    return;

  /* Compute the (approximate) length of the formatted output.  */
  fmtresult fmtres = spec.fmtfunc (spec, arg);

  /* The overall result is bounded and constant only if the output
     of every directive is bounded and constant, respectively.  */
  res->bounded &= fmtres.bounded;
  res->constant &= fmtres.constant;

  /* Record whether the output of all directives is known to be
     bounded by some maximum, implying that their arguments are
     either known exactly or determined to be in a known range
     or, for strings, limited by the upper bounds of the arrays
     they refer to.  */
  res->knownrange &= fmtres.knownrange;

  if (!fmtres.knownrange)
    {
      /* Only when the range is known, check it against the host value
	 of INT_MAX.  Otherwise the range doesn't correspond to known
	 values of the argument.  */
      if (fmtres.range.max >= target_int_max ())
	{
	  /* Normalize the MAX counter to avoid having to deal with it
	     later.  The counter can be less than HOST_WIDE_INT_M1U
	     when compiling for an ILP32 target on an LP64 host.  */
	  fmtres.range.max = HOST_WIDE_INT_M1U;
	  /* Disable exact and maximum length checking after a failure
	     to determine the maximum number of characters (for example
	     for wide characters or wide character strings) but continue
	     tracking the minimum number of characters.  */
	  res->number_chars_max = HOST_WIDE_INT_M1U;
	  res->number_chars = HOST_WIDE_INT_M1U;
	}

      if (fmtres.range.min >= target_int_max ())
	{
	  /* Disable exact length checking after a failure to determine
	     even the minimum number of characters (it shouldn't happen
	     except in an error) but keep tracking the minimum and maximum
	     number of characters.  */
	  res->number_chars = HOST_WIDE_INT_M1U;
	  return;
	}
    }

  /* Compute the number of available bytes in the destination.  There
     must always be at least one byte of space for the terminating
     NUL that's appended after the format string has been processed.  */
  unsigned HOST_WIDE_INT navail = min_bytes_remaining (info.objsize, *res);

  if (fmtres.range.min < fmtres.range.max)
    {
      /* The result is a range (i.e., it's inexact).  */
      if (!res->warned)
	{
	  bool warned = false;

	  if (navail < fmtres.range.min)
	    {
	      /* The minimum directive output is longer than there is
		 room in the destination.  */
	      if (fmtres.range.min == fmtres.range.max)
		{
		  const char* fmtstr
		    = (info.bounded
		       ? G_("%<%.*s%> directive output truncated writing "
			    "%wu bytes into a region of size %wu")
		       : G_("%<%.*s%> directive writing %wu bytes "
			    "into a region of size %wu"));
		  warned = fmtwarn (dirloc, pargrange, NULL,
				    OPT_Wformat_length_, fmtstr,
				    (int)cvtlen, cvtbeg, fmtres.range.min,
				    navail);
		}
	      else if (fmtres.range.max < HOST_WIDE_INT_MAX)
		{
		  const char* fmtstr
		    = (info.bounded
		       ? G_("%<%.*s%> directive output truncated writing "
			    "between %wu and %wu bytes into a region of "
			    "size %wu")
		       : G_("%<%.*s%> directive writing between %wu and "
			    "%wu bytes into a region of size %wu"));
		  warned = fmtwarn (dirloc, pargrange, NULL,
				    OPT_Wformat_length_, fmtstr,
				    (int)cvtlen, cvtbeg,
				    fmtres.range.min, fmtres.range.max, navail);
		}
	      else
		{
		  const char* fmtstr
		    = (info.bounded
		       ? G_("%<%.*s%> directive output truncated writing "
			    "%wu or more bytes into a region of size %wu")
		       : G_("%<%.*s%> directive writing %wu or more bytes "
			    "into a region of size %wu"));
		  warned = fmtwarn (dirloc, pargrange, NULL,
				    OPT_Wformat_length_, fmtstr,
				    (int)cvtlen, cvtbeg,
				    fmtres.range.min, navail);
		}
	    }
	  else if (navail < fmtres.range.max
		   && (((spec.specifier == 's'
			 && fmtres.range.max < HOST_WIDE_INT_MAX)
			/* && (spec.precision || spec.star_precision) */)
		       || 1 < warn_format_length))
	    {
	      /* The maximum directive output is longer than there is
		 room in the destination and the output length is either
		 explicitly constrained by the precision (for strings)
 		 or the warning level is greater than 1.  */
	      if (fmtres.range.max >= HOST_WIDE_INT_MAX)
		{
		  const char* fmtstr
		    = (info.bounded
		       ? G_("%<%.*s%> directive output may be truncated "
			    "writing %wu or more bytes a region of size %wu")
		       : G_("%<%.*s%> directive writing %wu or more bytes "
			    "into a region of size %wu"));
		  warned = fmtwarn (dirloc, pargrange, NULL,
				    OPT_Wformat_length_, fmtstr,
				    (int)cvtlen, cvtbeg,
				    fmtres.range.min, navail);
		}
	      else
		{
		  const char* fmtstr
		    = (info.bounded
		       ? G_("%<%.*s%> directive output may be truncated "
			    "writing between %wu and %wu bytes into a region "
			    "of size %wu")
		       : G_("%<%.*s%> directive writing between %wu and %wu "
			    "bytes into a region of size %wu"));
		  warned = fmtwarn (dirloc, pargrange, NULL,
				    OPT_Wformat_length_, fmtstr,
				    (int)cvtlen, cvtbeg,
				    fmtres.range.min, fmtres.range.max,
				    navail);
		}
	    }

	  res->warned |= warned;

	  if (warned && fmtres.argmin)
	    {
	      if (fmtres.argmin == fmtres.argmax)
		inform (info.fmtloc, "directive argument %qE", fmtres.argmin);
	      else if (fmtres.bounded)
		inform (info.fmtloc, "directive argument in the range [%E, %E]",
			fmtres.argmin, fmtres.argmax);
	      else
		inform (info.fmtloc,
			"using the range [%qE, %qE] for directive argument",
			fmtres.argmin, fmtres.argmax);
	    }
	}

      /* Disable exact length checking but adjust the minimum and maximum.  */
      res->number_chars = HOST_WIDE_INT_M1U;
      if (res->number_chars_max < HOST_WIDE_INT_MAX
	  && fmtres.range.max < HOST_WIDE_INT_MAX)
	res->number_chars_max += fmtres.range.max;

      res->number_chars_min += fmtres.range.min;
    }
  else
    {
      if (!res->warned && fmtres.range.min > 0 && navail < fmtres.range.min)
	{
	  const char* fmtstr
	    = (info.bounded
	       ? (1 < fmtres.range.min
		  ? G_("%<%.*s%> directive output truncated while writing "
		       "%wu bytes into a region of size %wu")
		  : G_("%<%.*s%> directive output truncated while writing "
		       "%wu byte into a region of size %wu"))
	       : (1 < fmtres.range.min
		  ? G_("%<%.*s%> directive writing %wu bytes "
		       "into a region of size %wu")
		  : G_("%<%.*s%> directive writing %wu byte "
		       "into a region of size %wu")));

	  res->warned = fmtwarn (dirloc, pargrange, NULL,
				 OPT_Wformat_length_, fmtstr,
				 (int)cvtlen, cvtbeg, fmtres.range.min,
				 navail);
	}
      *res += fmtres.range.min;
    }

  /* Has the minimum directive output length exceeded the maximum
     of 4095 bytes required to be supported?  */
  bool minunder4k = fmtres.range.min < 4096;
  if (!minunder4k || fmtres.range.max > 4095)
    res->under4k = false;

  if (!res->warned && 1 < warn_format_length
      && (!minunder4k || fmtres.range.max > 4095))
    {
      /* The directive output may be longer than the maximum required
	 to be handled by an implementation according to 7.21.6.1, p15
	 of C11.  Warn on this only at level 2 but remember this and
	 prevent folding the return value when done.  This allows for
	 the possibility of the actual libc call failing due to ENOMEM
	 (like Glibc does under some conditions).  */

      if (fmtres.range.min == fmtres.range.max)
	res->warned = fmtwarn (dirloc, pargrange, NULL,
			       OPT_Wformat_length_,
			       "%<%.*s%> directive output of %wu bytes exceeds "
			       "minimum required size of 4095",
			       (int)cvtlen, cvtbeg, fmtres.range.min);
      else
	{
	  const char *fmtstr
	    = (minunder4k
	       ? G_("%<%.*s%> directive output between %qu and %wu "
		    "bytes may exceed minimum required size of 4095")
	       : G_("%<%.*s%> directive output between %qu and %wu "
		    "bytes exceeds minimum required size of 4095"));

	  res->warned = fmtwarn (dirloc, pargrange, NULL,
				 OPT_Wformat_length_, fmtstr,
				 (int)cvtlen, cvtbeg,
				 fmtres.range.min, fmtres.range.max);
	}
    }

  /* Has the minimum directive output length exceeded INT_MAX?  */
  bool exceedmin = res->number_chars_min > target_int_max ();

  if (!res->warned
      && (exceedmin
	  || (1 < warn_format_length
	      && res->number_chars_max > target_int_max ())))
    {
      /* The directive output causes the total length of output
	 to exceed INT_MAX bytes.  */

      if (fmtres.range.min == fmtres.range.max)
	res->warned = fmtwarn (dirloc, pargrange, NULL,
			       OPT_Wformat_length_,
			       "%<%.*s%> directive output of %wu bytes causes "
			       "result to exceed %<INT_MAX%>",
			       (int)cvtlen, cvtbeg, fmtres.range.min);
      else
	{
	  const char *fmtstr
	    = (exceedmin
	       ? G_ ("%<%.*s%> directive output between %wu and %wu "
		     "bytes causes result to exceed %<INT_MAX%>")
	       : G_ ("%<%.*s%> directive output between %wu and %wu "
		     "bytes may cause result to exceed %<INT_MAX%>"));
	  res->warned = fmtwarn (dirloc, pargrange, NULL,
				 OPT_Wformat_length_, fmtstr,
				 (int)cvtlen, cvtbeg,
				 fmtres.range.min, fmtres.range.max);
	}
    }
}

/* Account for the number of bytes between BEG and END (or between
   BEG + strlen (BEG) when END is null) in the format string in a call
   to a formatted output function described by INFO.  Reflect the count
   in RES and issue warnings as appropriate.  */

static void
add_bytes (const pass_sprintf_length::call_info &info,
	   const char *beg, const char *end, format_result *res)
{
  if (res->number_chars_min >= HOST_WIDE_INT_MAX)
    return;

  /* The number of bytes to output is the number of bytes between
     the end of the last directive and the beginning of the next
     one if it exists, otherwise the number of characters remaining
     in the format string plus 1 for the terminating NUL.  */
  size_t nbytes = end ? end - beg : strlen (beg) + 1;

  /* Return if there are no bytes to add at this time but there are
     directives remaining in the format string.  */
  if (!nbytes)
    return;

  /* Compute the range of available bytes in the destination.  There
     must always be at least one byte left for the terminating NUL
     that's appended after the format string has been processed.  */
  result_range avail_range = bytes_remaining (info.objsize, *res);

  /* If issuing a diagnostic (only when one hasn't already been issued),
     distinguish between a possible overflow ("may write") and a certain
     overflow somewhere "past the end."  (Ditto for truncation.)
     KNOWNRANGE is used to warn even at level 1 about possibly writing
     past the end or truncation due to strings of unknown lengths that
     are bounded by the arrays they are known to refer to.  */
  if (!res->warned
      && (avail_range.max < nbytes
	  || ((res->knownrange || 1 < warn_format_length)
	      && avail_range.min < nbytes)))
    {
      /* Set NAVAIL to the number of available bytes used to decide
	 whether or not to issue a warning below.  The exact kind of
	 warning will depend on AVAIL_RANGE.  */
      unsigned HOST_WIDE_INT navail = avail_range.max;
      if (nbytes <= navail && avail_range.min < HOST_WIDE_INT_MAX
	  && (res->knownrange || 1 < warn_format_length))
	navail = avail_range.min;

      /* Compute the offset of the first format character that is beyond
	 the end of the destination region and the length of the rest of
	 the format string from that point on.  */
      unsigned HOST_WIDE_INT off
	= (unsigned HOST_WIDE_INT)(beg - info.fmtstr) + navail;

      size_t len = strlen (info.fmtstr + off);

      /* Create a location that underscores the substring of the format
	 string that is or may be written past the end (or is or may be
	 truncated), pointing the caret at the first character of the
	 substring.  */
      substring_loc loc
	(info.fmtloc, TREE_TYPE (info.format), off, len ? off : 0,
	 off + len - !!len);

      /* Is the output of the last directive the result of the argument
	 being within a range whose lower bound would fit in the buffer
	 but the upper bound would not?  If so, use the word "may" to
	 indicate that the overflow/truncation may (but need not) happen.  */
      bool boundrange
	= (res->number_chars_min < res->number_chars_max
	   && res->number_chars_min < info.objsize);

      if (!end && ((nbytes - navail) == 1 || boundrange))
	{
	  /* There is room for the rest of the format string but none
	     for the terminating nul.  */
	  const char *text
	    = (info.bounded   // Snprintf and the like.
	       ? (boundrange
		  ? G_("output may be truncated before the last format character"
		       : "output truncated before the last format character"))
	       : (boundrange
		  ? G_("may write a terminating nul past the end "
		       "of the destination")
		  : G_("writing a terminating nul past the end "
		       "of the destination")));

	  res->warned = fmtwarn (loc, NULL, NULL, OPT_Wformat_length_, text);
	}
      else
	{
	  /* There isn't enough room for 1 or more characters that remain
	     to copy from the format string.  */
	  const char *text
	    = (info.bounded   // Snprintf and the like.
	       ? (boundrange
		  ? G_("output may be truncated at or before format character "
		       "%qc at offset %wu")
		  : G_("output truncated at format character %qc at offset %wu"))
	       : (res->number_chars >= HOST_WIDE_INT_MAX
		  ? G_("may write format character %#qc at offset %wu past "
		       "the end of the destination")
		  : G_("writing format character %#qc at offset %wu past "
		       "the end of the destination")));

	  res->warned = fmtwarn (loc, NULL, NULL, OPT_Wformat_length_,
				 text, info.fmtstr[off], off);
	}
    }

  if (res->warned && !end && info.objsize < HOST_WIDE_INT_MAX)
    {
      /* If a warning has been issued for buffer overflow or truncation
	 (but not otherwise) help the user figure out how big a buffer
	 they need.  */

      location_t callloc = gimple_location (info.callstmt);

      unsigned HOST_WIDE_INT min = res->number_chars_min;
      unsigned HOST_WIDE_INT max = res->number_chars_max;
      unsigned HOST_WIDE_INT exact
	= (res->number_chars < HOST_WIDE_INT_MAX
	   ? res->number_chars : res->number_chars_min);

      if (min < max && max < HOST_WIDE_INT_MAX)
	inform (callloc,
		"format output between %wu and %wu bytes into "
		"a destination of size %wu",
		min + nbytes, max + nbytes, info.objsize);
      else
	inform (callloc,
		(nbytes + exact == 1
		 ? G_("format output %wu byte into a destination of size %wu")
		 : G_("format output %wu bytes into a destination of size %wu")),
		nbytes + exact, info.objsize);
    }

  /* Add the number of bytes and then check for INT_MAX overflow.  */
  *res += nbytes;

  /* Has the minimum output length minus the terminating nul exceeded
     INT_MAX?  */
  bool exceedmin = (res->number_chars_min - !end) > target_int_max ();

  if (!res->warned
      && (exceedmin
	  || (1 < warn_format_length
	      && (res->number_chars_max - !end) > target_int_max ())))
    {
      /* The function's output exceeds INT_MAX bytes.  */

      /* Set NAVAIL to the number of available bytes used to decide
	 whether or not to issue a warning below.  The exact kind of
	 warning will depend on AVAIL_RANGE.  */
      unsigned HOST_WIDE_INT navail = avail_range.max;
      if (nbytes <= navail && avail_range.min < HOST_WIDE_INT_MAX
	  && (res->bounded || 1 < warn_format_length))
	navail = avail_range.min;

      /* Compute the offset of the first format character that is beyond
	 the end of the destination region and the length of the rest of
	 the format string from that point on.  */
      unsigned HOST_WIDE_INT off = (unsigned HOST_WIDE_INT)(beg - info.fmtstr);
      if (navail < HOST_WIDE_INT_MAX)
	off += navail;

      size_t len = strlen (info.fmtstr + off);

      substring_loc loc
	(info.fmtloc, TREE_TYPE (info.format), off - !len, len ? off : 0,
	 off + len - !!len);

      if (res->number_chars_min == res->number_chars_max)
	res->warned = fmtwarn (loc, NULL, NULL,
			       OPT_Wformat_length_,
			       "output of %wu bytes causes "
			       "result to exceed %<INT_MAX%>",
			       res->number_chars_min - !end);
      else
	{
	  const char *text
	    = (exceedmin
	       ? G_ ("output between %wu and %wu bytes causes "
		     "result to exceed %<INT_MAX%>")
	       : G_ ("output between %wu and %wu bytes may cause "
		     "result to exceed %<INT_MAX%>"));
	  res->warned = fmtwarn (loc, NULL, NULL, OPT_Wformat_length_,
				 text,
				 res->number_chars_min - !end,
				 res->number_chars_max - !end);
	}
    }
}

#pragma GCC diagnostic pop

/* Compute the length of the output resulting from the call to a formatted
   output function described by INFO and store the result of the call in
   *RES.  Issue warnings for detected past the end writes.  Return true
   if the complete format string has been processed and *RES can be relied
   on, false otherwise (e.g., when a unknown or unhandled directive was seen
   that caused the processing to be terminated early).  */

bool
pass_sprintf_length::compute_format_length (const call_info &info,
					    format_result *res)
{
  /* The variadic argument counter.  */
  unsigned argno = info.argidx;

  /* Reset exact, minimum, and maximum character counters.  */
  res->number_chars = res->number_chars_min = res->number_chars_max = 0;

  /* No directive has been seen yet so the length of output is bounded
     by the known range [0, 0] and constant (with no conversion producing
     more than 4K bytes) until determined otherwise.  */
  res->bounded = true;
  res->knownrange = true;
  res->constant = true;
  res->under4k = true;
  res->floating = false;
  res->warned = false;

  const char *pf = info.fmtstr;

  for ( ; ; )
    {
      /* The beginning of the next format directive.  */
      const char *dir = strchr (pf, '%');

      /* Add the number of bytes between the end of the last directive
	 and either the next if one exists, or the end of the format
	 string.  */
      add_bytes (info, pf, dir, res);

      if (!dir)
	break;

      pf = dir + 1;

      if (0 && *pf == 0)
	{
	  /* Incomplete directive.  */
	  return false;
	}

      conversion_spec spec = conversion_spec ();

      /* POSIX numbered argument index or zero when none.  */
      unsigned dollar = 0;

      if (ISDIGIT (*pf))
	{
	  /* This could be either a POSIX positional argument, the '0'
	     flag, or a width, depending on what follows.  Store it as
	     width and sort it out later after the next character has
	     been seen.  */
	  char *end;
	  spec.width = strtol (pf, &end, 10);
	  spec.have_width = true;
	  pf = end;
	}
      else if ('*' == *pf)
	{
	  /* Similarly to the block above, this could be either a POSIX
	     positional argument or a width, depending on what follows.  */
	  if (gimple_call_num_args (info.callstmt) <= argno)
	    return false;

	  spec.star_width = gimple_call_arg (info.callstmt, argno++);
	  ++pf;
	}

      if (*pf == '$')
	{
	  /* Handle the POSIX dollar sign which references the 1-based
	     positional argument number.  */
	  if (spec.have_width)
	    dollar = spec.width + info.argidx;
	  else if (spec.star_width
		   && TREE_CODE (spec.star_width) == INTEGER_CST)
	    dollar = spec.width + tree_to_shwi (spec.star_width);

	  /* Bail when the numbered argument is out of range (it will
	     have already been diagnosed by -Wformat).  */
	  if (dollar == 0
	      || dollar == info.argidx
	      || dollar > gimple_call_num_args (info.callstmt))
	    return false;

	  --dollar;

	  spec.star_width = NULL_TREE;
	  spec.have_width = false;
	  ++pf;
	}

      if (dollar || !spec.star_width)
	{
	  if (spec.have_width)
	    {
	      if (spec.width == 0)
		{
		  /* The '0' that has been interpreted as a width above is
		     actually a flag.  Reset HAVE_WIDTH, set the '0' flag,
		     and continue processing other flags.  */
		  spec.have_width = false;
		  spec.set_flag ('0');
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
	    switch (*pf)
	      {
	      case ' ':
	      case '0':
	      case '+':
	      case '-':
	      case '#':
		spec.set_flag (*pf++);
		break;

	      default:
		goto start_width;
	      }
	  }

	start_width:
	  if (ISDIGIT (*pf))
	    {
	      char *end;
	      spec.width = strtol (pf, &end, 10);
	      spec.have_width = true;
	      pf = end;
	    }
	  else if ('*' == *pf)
	    {
	      spec.star_width = gimple_call_arg (info.callstmt, argno++);
	      ++pf;
	    }
	  else if ('\'' == *pf)
	    {
	      /* The POSIX apostrophe indicating a numeric grouping
		 in the current locale.  Even though it's possible to
		 estimate the upper bound on the size of the output
		 based on the number of digits it probably isn't worth
		 continuing.  */
	      return false;
	    }
	}

    start_precision:
      if ('.' == *pf)
	{
	  ++pf;

	  if (ISDIGIT (*pf))
	    {
	      char *end;
	      spec.precision = strtol (pf, &end, 10);
	      spec.have_precision = true;
	      pf = end;
	    }
	  else if ('*' == *pf)
	    {
	      spec.star_precision = gimple_call_arg (info.callstmt, argno++);
	      ++pf;
	    }
	  else
	    {
	      /* The decimal precision or the asterisk are optional.
		 When neither is specified it's taken to be zero.  */
	      spec.precision = 0;
	      spec.have_precision = true;
	    }
	}

      switch (*pf)
	{
	case 'h':
	  if (pf[1] == 'h')
	    {
	      ++pf;
	      spec.modifier = FMT_LEN_hh;
	    }
	  else
	    spec.modifier = FMT_LEN_h;
	  ++pf;
	  break;

	case 'j':
	  spec.modifier = FMT_LEN_j;
	  ++pf;
	  break;

	case 'L':
	  spec.modifier = FMT_LEN_L;
	  ++pf;
	  break;

	case 'l':
	  if (pf[1] == 'l')
	    {
	      ++pf;
	      spec.modifier = FMT_LEN_ll;
	    }
	  else
	    spec.modifier = FMT_LEN_l;
	  ++pf;
	  break;

	case 't':
	  spec.modifier = FMT_LEN_t;
	  ++pf;
	  break;

	case 'z':
	  spec.modifier = FMT_LEN_z;
	  ++pf;
	  break;
	}

      switch (*pf)
	{
	  /* Handle a sole '%' character the same as "%%" but since it's
	     undefined prevent the result from being folded.  */
	case '\0':
	  --pf;
	  res->bounded = false;
	  /* FALLTHRU */
	case '%':
	  spec.fmtfunc = format_percent;
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
	  spec.fmtfunc = format_floating;
	  break;

	case 'd':
	case 'i':
	case 'o':
	case 'u':
	case 'x':
	case 'X':
	  spec.fmtfunc = format_integer;
	  break;

	case 'p':
	  /* The %p output is implementation-defined.  It's possible
	     to determine this format but due to extensions (especially
	     those of the Linux kernel -- see bug 78512) the first %p
	     in the format string disables any further processing.  */
	  return false;

	case 'n':
	  break;

	case 'c':
	case 'S':
	case 's':
	  spec.fmtfunc = format_string;
	  break;

	default:
	  /* Unknown conversion specification.  */
	  return false;
	}

      spec.specifier = *pf++;

      /* Compute the length of the format directive.  */
      size_t dirlen = pf - dir;

      /* Extract the argument if the directive takes one and if it's
	 available (e.g., the function doesn't take a va_list).  Treat
	 missing arguments the same as va_list, even though they will
	 have likely already been diagnosed by -Wformat.  */
      tree arg = NULL_TREE;
      if (spec.specifier != '%'
	  && argno < gimple_call_num_args (info.callstmt))
	arg = gimple_call_arg (info.callstmt, dollar ? dollar : argno++);

      ::format_directive (info, res, dir, dirlen, spec, arg);
    }

  /* Complete format string was processed (with or without warnings).  */
  return true;
}

/* Return the size of the object referenced by the expression DEST if
   available, or -1 otherwise.  */

static unsigned HOST_WIDE_INT
get_destination_size (tree dest)
{
  /* Use __builtin_object_size to determine the size of the destination
     object.  When optimizing, determine the smallest object (such as
     a member array as opposed to the whole enclosing object), otherwise
     use type-zero object size to determine the size of the enclosing
     object (the function fails without optimization in this type).  */
  int ost = optimize > 0;
  unsigned HOST_WIDE_INT size;
  if (compute_builtin_object_size (dest, ost, &size))
    return size;

  return HOST_WIDE_INT_M1U;
}

/* Given a suitable result RES of a call to a formatted output function
   described by INFO, substitute the result for the return value of
   the call.  The result is suitable if the number of bytes it represents
   is known and exact.  A result that isn't suitable for substitution may
   have its range set to the range of return values, if that is known.  */

static void
try_substitute_return_value (gimple_stmt_iterator *gsi,
			     const pass_sprintf_length::call_info &info,
			     const format_result &res)
{
  tree lhs = gimple_get_lhs (info.callstmt);

  /* Avoid the return value optimization when the behavior of the call
     is undefined either because any directive may have produced 4K or
     more of output, or the return value exceeds INT_MAX, or because
     the output overflows the destination object (but leave it enabled
     when the function is bounded because then the behavior is well-
     defined).  */
  if (lhs && res.bounded && res.under4k
      && (info.bounded || res.number_chars <= info.objsize)
      && res.number_chars - 1 <= target_int_max ())
    {
      tree cst = build_int_cst (integer_type_node, res.number_chars - 1);

      if (info.nowrite)
	{
	  /* Replace the call to the bounded function with a zero size
	     (e.g., snprintf(0, 0, "%i", 123) with the constant result
	     of the function minus 1 for the terminating NUL which
	     the function's  return value does not include.  */
	  if (!update_call_from_tree (gsi, cst))
	    gimplify_and_update_call_from_tree (gsi, cst);
	  gimple *callstmt = gsi_stmt (*gsi);
	  update_stmt (callstmt);
	}
      else
	{
	  /* Replace the left-hand side of the call with the constant
	     result of the formatted function minus 1 for the terminating
	     NUL which the function's return value does not include.  */
	  gimple_call_set_lhs (info.callstmt, NULL_TREE);
	  gimple *g = gimple_build_assign (lhs, cst);
	  gsi_insert_after (gsi, g, GSI_NEW_STMT);
	  update_stmt (info.callstmt);
	}

      if (dump_file)
	{
	  location_t callloc = gimple_location (info.callstmt);
	  fprintf (dump_file, "On line %i substituting ",
		   LOCATION_LINE (callloc));
	  print_generic_expr (dump_file, cst, dump_flags);
	  fprintf (dump_file, " for ");
	  print_generic_expr (dump_file, info.func, dump_flags);
	  fprintf (dump_file, " %s (output %s).\n",
		   info.nowrite ? "call" : "return value",
		   res.constant ? "constant" : "variable");
	}
    }
  else
    {
      unsigned HOST_WIDE_INT maxbytes;

      if (lhs
	  && res.bounded
	  && ((maxbytes = res.number_chars - 1) <= target_int_max ()
	      || (res.number_chars_min - 1 <= target_int_max ()
		  && (maxbytes = res.number_chars_max - 1) <= target_int_max ()))
	  && (info.bounded || maxbytes < info.objsize))
	{
	  /* If the result is in a valid range bounded by the size of
	     the destination set it so that it can be used for subsequent
	     optimizations.  */
	  int prec = TYPE_PRECISION (integer_type_node);

	  if (res.number_chars < target_int_max () && res.under4k)
	    {
	      wide_int num = wi::shwi (res.number_chars - 1, prec);
	      set_range_info (lhs, VR_RANGE, num, num);
	    }
	  else if (res.number_chars_min < target_int_max ()
		   && res.number_chars_max < target_int_max ())
	    {
	      wide_int min = wi::shwi (res.under4k ? res.number_chars_min - 1
				       : target_int_min (), prec);
	      wide_int max = wi::shwi (res.number_chars_max - 1, prec);
	      set_range_info (lhs, VR_RANGE, min, max);
	    }
	}

      if (dump_file)
	{
	  const char *inbounds
	    = (res.number_chars_min <= info.objsize
	       ? (res.number_chars_max <= info.objsize
		  ? "in" : "potentially out-of")
	       : "out-of");

	  location_t callloc = gimple_location (info.callstmt);
	  fprintf (dump_file, "On line %i ", LOCATION_LINE (callloc));
	  print_generic_expr (dump_file, info.func, dump_flags);

	  const char *ign = lhs ? "" : " ignored";
	  if (res.number_chars >= HOST_WIDE_INT_MAX)
	    fprintf (dump_file,
		     " %s-bounds return value in range [%lu, %lu]%s.\n",
		     inbounds,
		     (unsigned long)res.number_chars_min,
		     (unsigned long)res.number_chars_max, ign);
	  else
	    fprintf (dump_file, " %s-bounds return value %lu%s.\n",
		     inbounds, (unsigned long)res.number_chars, ign);
	}
    }
}

/* Determine if a GIMPLE CALL is to one of the sprintf-like built-in
   functions and if so, handle it.  */

void
pass_sprintf_length::handle_gimple_call (gimple_stmt_iterator *gsi)
{
  call_info info = call_info ();

  info.callstmt = gsi_stmt (*gsi);
  if (!gimple_call_builtin_p (info.callstmt, BUILT_IN_NORMAL))
    return;

  info.func = gimple_call_fndecl (info.callstmt);
  info.fncode = DECL_FUNCTION_CODE (info.func);

  /* The size of the destination as in snprintf(dest, size, ...).  */
  unsigned HOST_WIDE_INT dstsize = HOST_WIDE_INT_M1U;

  /* The size of the destination determined by __builtin_object_size.  */
  unsigned HOST_WIDE_INT objsize = HOST_WIDE_INT_M1U;

  /* Buffer size argument number (snprintf and vsnprintf).  */
  unsigned HOST_WIDE_INT idx_dstsize = HOST_WIDE_INT_M1U;

  /* Object size argument number (snprintf_chk and vsnprintf_chk).  */
  unsigned HOST_WIDE_INT idx_objsize = HOST_WIDE_INT_M1U;

  /* Format string argument number (valid for all functions).  */
  unsigned idx_format;

  switch (info.fncode)
    {
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
      return;
    }

  info.format = gimple_call_arg (info.callstmt, idx_format);

  if (idx_dstsize == HOST_WIDE_INT_M1U)
    {
      /* For non-bounded functions like sprintf, determine the size
	 of the destination from the object or pointer passed to it
	 as the first argument.  */
      dstsize = get_destination_size (gimple_call_arg (info.callstmt, 0));
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
	      if ((idx_objsize == HOST_WIDE_INT_M1U
		   || !warn_stringop_overflow))
		warning_at (gimple_location (info.callstmt),
			    OPT_Wformat_length_,
			    "specified bound %wu exceeds maximum object size "
			    "%wu",
			    dstsize, target_size_max () / 2);
	    }
	  else if (dstsize > target_int_max ())
	    warning_at (gimple_location (info.callstmt), OPT_Wformat_length_,
			"specified bound %wu exceeds %<INT_MAX %>",
			dstsize);
	}
      else if (TREE_CODE (size) == SSA_NAME)
	{
	  /* Try to determine the range of values of the argument
	     and use the greater of the two at -Wformat-level 1 and
	     the smaller of them at level 2.  */
	  wide_int min, max;
	  enum value_range_type range_type
	    = get_range_info (size, &min, &max);
	  if (range_type == VR_RANGE)
	    {
	      dstsize
		= (warn_format_length < 2
		   ? wi::fits_uhwi_p (max) ? max.to_uhwi () : max.to_shwi ()
		   : wi::fits_uhwi_p (min) ? min.to_uhwi () : min.to_shwi ());
	    }
	}
    }

  if (idx_objsize != HOST_WIDE_INT_M1U)
    {
      if (tree size = gimple_call_arg (info.callstmt, idx_objsize))
	  if (tree_fits_uhwi_p (size))
	    objsize = tree_to_uhwi (size);
    }

  if (info.bounded && !dstsize)
    {
      /* As a special case, when the explicitly specified destination
	 size argument (to a bounded function like snprintf) is zero
	 it is a request to determine the number of bytes on output
	 without actually producing any.  Pretend the size is
	 unlimited in this case.  */
      info.objsize = HOST_WIDE_INT_MAX;
      info.nowrite = true;
    }
  else
    {
      /* Set the object size to the smaller of the two arguments
	 of both have been specified and they're not equal.  */
      info.objsize = dstsize < objsize ? dstsize : objsize;

      if (info.bounded
	  && dstsize < target_size_max () / 2 && objsize < dstsize
	  /* Avoid warning if -Wstringop-overflow is specified since
	     it also warns for the same thing though only for the
	     checking built-ins.  */
	  && (idx_objsize == HOST_WIDE_INT_M1U
	      || !warn_stringop_overflow))
	{
	  warning_at (gimple_location (info.callstmt), OPT_Wformat_length_,
		      "specified bound %wu exceeds the size %wu "
		      "of the destination object", dstsize, objsize);
	}
    }

  if (integer_zerop (info.format))
    {
      /* This is diagnosed with -Wformat only when the null is a constant
	 pointer.  The warning here diagnoses instances where the pointer
	 is not constant.  */
      warning_at (EXPR_LOC_OR_LOC (info.format, input_location),
		  OPT_Wformat_length_, "null format string");
      return;
    }

  info.fmtstr = get_format_string (info.format, &info.fmtloc);
  if (!info.fmtstr)
    return;

  /* The result is the number of bytes output by the formatted function,
     including the terminating NUL.  */
  format_result res = format_result ();

  bool success = compute_format_length (info, &res);

  /* When optimizing and the printf return value optimization is enabled,
     attempt to substitute the computed result for the return value of
     the call.  Avoid this optimization when -frounding-math is in effect
     and the format string contains a floating point directive.  */
  if (success
      && optimize > 0
      && flag_printf_return_value
      && (!flag_rounding_math || !res.floating))
    try_substitute_return_value (gsi, info, res);
}

/* Execute the pass for function FUN.  */

unsigned int
pass_sprintf_length::execute (function *fun)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  /* Iterate over statements, looking for function calls.  */
	  gimple *stmt = gsi_stmt (si);

	  if (is_gimple_call (stmt))
	    handle_gimple_call (&si);
	}
    }

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
