/* Check calls to formatted I/O functions (-Wformat).
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "toplev.h"
#include "c-common.h"
#include "intl.h"
#include "diagnostic.h"


/* Command line options and their associated flags.  */

/* Warn about format/argument anomalies in calls to formatted I/O functions
   (*printf, *scanf, strftime, strfmon, etc.).  */

int warn_format;

/* Warn about Y2K problems with strftime formats.  */

int warn_format_y2k;

/* Warn about excess arguments to formats.  */

int warn_format_extra_args;

/* Warn about non-literal format arguments.  */

int warn_format_nonliteral;

/* Warn about possible security problems with calls to format functions.  */

int warn_format_security;

/* Set format warning options according to a -Wformat=n option.  */

void
set_Wformat (setting)
     int setting;
{
  warn_format = setting;
  warn_format_y2k = setting;
  warn_format_extra_args = setting;
  if (setting != 1)
    {
      warn_format_nonliteral = setting;
      warn_format_security = setting;
    }
}


/* Handle attributes associated with format checking.  */

/* This must be in the same order as format_types, with format_type_error
   last.  */
enum format_type { printf_format_type, scanf_format_type,
		   strftime_format_type, strfmon_format_type,
		   format_type_error };

typedef struct function_format_info
{
  enum format_type format_type;	/* type of format (printf, scanf, etc.) */
  unsigned HOST_WIDE_INT format_num;	/* number of format argument */
  unsigned HOST_WIDE_INT first_arg_num;	/* number of first arg (zero for varargs) */
} function_format_info;

static bool decode_format_attr		PARAMS ((tree,
						 function_format_info *, int));
static enum format_type decode_format_type	PARAMS ((const char *));

/* Handle a "format" attribute; arguments as in
   struct attribute_spec.handler.  */
tree
handle_format_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags;
     bool *no_add_attrs;
{
  tree type = *node;
  function_format_info info;
  tree argument;
  unsigned HOST_WIDE_INT arg_num;

  if (!decode_format_attr (args, &info, 0))
    {
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* If a parameter list is specified, verify that the format_num
     argument is actually a string, in case the format attribute
     is in error.  */
  argument = TYPE_ARG_TYPES (type);
  if (argument)
    {
      for (arg_num = 1; argument != 0 && arg_num != info.format_num;
	   ++arg_num, argument = TREE_CHAIN (argument))
	;

      if (! argument
	  || TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
	  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
	      != char_type_node))
	{
	  if (!(flags & (int) ATTR_FLAG_BUILT_IN))
	    error ("format string arg not a string type");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      else if (info.first_arg_num != 0)
	{
	  /* Verify that first_arg_num points to the last arg,
	     the ...  */
	  while (argument)
	    arg_num++, argument = TREE_CHAIN (argument);

	  if (arg_num != info.first_arg_num)
	    {
	      if (!(flags & (int) ATTR_FLAG_BUILT_IN))
		error ("args to be formatted is not '...'");
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }
	}
    }

  if (info.format_type == strftime_format_type && info.first_arg_num != 0)
    {
      error ("strftime formats cannot format arguments");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  return NULL_TREE;
}


/* Handle a "format_arg" attribute; arguments as in
   struct attribute_spec.handler.  */
tree
handle_format_arg_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name ATTRIBUTE_UNUSED;
     tree args;
     int flags;
     bool *no_add_attrs;
{
  tree type = *node;
  tree format_num_expr = TREE_VALUE (args);
  unsigned HOST_WIDE_INT format_num;
  unsigned HOST_WIDE_INT arg_num;
  tree argument;

  /* Strip any conversions from the first arg number and verify it
     is a constant.  */
  while (TREE_CODE (format_num_expr) == NOP_EXPR
	 || TREE_CODE (format_num_expr) == CONVERT_EXPR
	 || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
    format_num_expr = TREE_OPERAND (format_num_expr, 0);

  if (TREE_CODE (format_num_expr) != INTEGER_CST
      || TREE_INT_CST_HIGH (format_num_expr) != 0)
    {
      error ("format string has invalid operand number");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  format_num = TREE_INT_CST_LOW (format_num_expr);

  /* If a parameter list is specified, verify that the format_num
     argument is actually a string, in case the format attribute
     is in error.  */
  argument = TYPE_ARG_TYPES (type);
  if (argument)
    {
      for (arg_num = 1; argument != 0 && arg_num != format_num;
	   ++arg_num, argument = TREE_CHAIN (argument))
	;

      if (! argument
	  || TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
	  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
	      != char_type_node))
	{
	  if (!(flags & (int) ATTR_FLAG_BUILT_IN))
	    error ("format string arg not a string type");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }

  if (TREE_CODE (TREE_TYPE (type)) != POINTER_TYPE
      || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (type)))
	  != char_type_node))
    {
      if (!(flags & (int) ATTR_FLAG_BUILT_IN))
	error ("function does not return string type");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  return NULL_TREE;
}


/* Decode the arguments to a "format" attribute into a function_format_info
   structure.  It is already known that the list is of the right length.
   If VALIDATED_P is true, then these attributes have already been validated
   and this function will abort if they are erroneous; if false, it
   will give an error message.  Returns true if the attributes are
   successfully decoded, false otherwise.  */

static bool
decode_format_attr (args, info, validated_p)
     tree args;
     function_format_info *info;
     int validated_p;
{
  tree format_type_id = TREE_VALUE (args);
  tree format_num_expr = TREE_VALUE (TREE_CHAIN (args));
  tree first_arg_num_expr
    = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));

  if (TREE_CODE (format_type_id) != IDENTIFIER_NODE)
    {
      if (validated_p)
	abort ();
      error ("unrecognized format specifier");
      return false;
    }
  else
    {
      const char *p = IDENTIFIER_POINTER (format_type_id);

      info->format_type = decode_format_type (p);

      if (info->format_type == format_type_error)
	{
	  if (validated_p)
	    abort ();
	  warning ("`%s' is an unrecognized format function type", p);
	  return false;
	}
    }

  /* Strip any conversions from the string index and first arg number
     and verify they are constants.  */
  while (TREE_CODE (format_num_expr) == NOP_EXPR
	 || TREE_CODE (format_num_expr) == CONVERT_EXPR
	 || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
    format_num_expr = TREE_OPERAND (format_num_expr, 0);

  while (TREE_CODE (first_arg_num_expr) == NOP_EXPR
	 || TREE_CODE (first_arg_num_expr) == CONVERT_EXPR
	 || TREE_CODE (first_arg_num_expr) == NON_LVALUE_EXPR)
    first_arg_num_expr = TREE_OPERAND (first_arg_num_expr, 0);

  if (TREE_CODE (format_num_expr) != INTEGER_CST
      || TREE_INT_CST_HIGH (format_num_expr) != 0
      || TREE_CODE (first_arg_num_expr) != INTEGER_CST
      || TREE_INT_CST_HIGH (first_arg_num_expr) != 0)
    {
      if (validated_p)
	abort ();
      error ("format string has invalid operand number");
      return false;
    }

  info->format_num = TREE_INT_CST_LOW (format_num_expr);
  info->first_arg_num = TREE_INT_CST_LOW (first_arg_num_expr);
  if (info->first_arg_num != 0 && info->first_arg_num <= info->format_num)
    {
      if (validated_p)
	abort ();
      error ("format string arg follows the args to be formatted");
      return false;
    }

  return true;
}

/* Check a call to a format function against a parameter list.  */

/* The meaningfully distinct length modifiers for format checking recognised
   by GCC.  */
enum format_lengths
{
  FMT_LEN_none,
  FMT_LEN_hh,
  FMT_LEN_h,
  FMT_LEN_l,
  FMT_LEN_ll,
  FMT_LEN_L,
  FMT_LEN_z,
  FMT_LEN_t,
  FMT_LEN_j,
  FMT_LEN_MAX
};


/* The standard versions in which various format features appeared.  */
enum format_std_version
{
  STD_C89,
  STD_C94,
  STD_C9L, /* C99, but treat as C89 if -Wno-long-long.  */
  STD_C99,
  STD_EXT
};

/* The C standard version C++ is treated as equivalent to
   or inheriting from, for the purpose of format features supported.  */
#define CPLUSPLUS_STD_VER	STD_C94
/* The C standard version we are checking formats against when pedantic.  */
#define C_STD_VER		((int)(c_language == clk_cplusplus	  \
				 ? CPLUSPLUS_STD_VER			  \
				 : (flag_isoc99				  \
				    ? STD_C99				  \
				    : (flag_isoc94 ? STD_C94 : STD_C89))))
/* The name to give to the standard version we are warning about when
   pedantic.  FEATURE_VER is the version in which the feature warned out
   appeared, which is higher than C_STD_VER.  */
#define C_STD_NAME(FEATURE_VER) (c_language == clk_cplusplus	\
				 ? "ISO C++"			\
				 : ((FEATURE_VER) == STD_EXT	\
				    ? "ISO C"			\
				    : "ISO C89"))
/* Adjust a C standard version, which may be STD_C9L, to account for
   -Wno-long-long.  Returns other standard versions unchanged.  */
#define ADJ_STD(VER)		((int)((VER) == STD_C9L			      \
				       ? (warn_long_long ? STD_C99 : STD_C89) \
				       : (VER)))

/* Flags that may apply to a particular kind of format checked by GCC.  */
enum
{
  /* This format converts arguments of types determined by the
     format string.  */
  FMT_FLAG_ARG_CONVERT = 1,
  /* The scanf allocation 'a' kludge applies to this format kind.  */
  FMT_FLAG_SCANF_A_KLUDGE = 2,
  /* A % during parsing a specifier is allowed to be a modified % rather
     that indicating the format is broken and we are out-of-sync.  */
  FMT_FLAG_FANCY_PERCENT_OK = 4,
  /* With $ operand numbers, it is OK to reference the same argument more
     than once.  */
  FMT_FLAG_DOLLAR_MULTIPLE = 8,
  /* This format type uses $ operand numbers (strfmon doesn't).  */
  FMT_FLAG_USE_DOLLAR = 16,
  /* Zero width is bad in this type of format (scanf).  */
  FMT_FLAG_ZERO_WIDTH_BAD = 32,
  /* Empty precision specification is OK in this type of format (printf).  */
  FMT_FLAG_EMPTY_PREC_OK = 64,
  /* Gaps are allowed in the arguments with $ operand numbers if all
     arguments are pointers (scanf).  */
  FMT_FLAG_DOLLAR_GAP_POINTER_OK = 128
  /* Not included here: details of whether width or precision may occur
     (controlled by width_char and precision_char); details of whether
     '*' can be used for these (width_type and precision_type); details
     of whether length modifiers can occur (length_char_specs).  */
};


/* Structure describing a length modifier supported in format checking, and
   possibly a doubled version such as "hh".  */
typedef struct
{
  /* Name of the single-character length modifier.  */
  const char *const name;
  /* Index into a format_char_info.types array.  */
  const enum format_lengths index;
  /* Standard version this length appears in.  */
  const enum format_std_version std;
  /* Same, if the modifier can be repeated, or NULL if it can't.  */
  const char *const double_name;
  const enum format_lengths double_index;
  const enum format_std_version double_std;
} format_length_info;


/* Structure describing the combination of a conversion specifier
   (or a set of specifiers which act identically) and a length modifier.  */
typedef struct
{
  /* The standard version this combination of length and type appeared in.
     This is only relevant if greater than those for length and type
     individually; otherwise it is ignored.  */
  enum format_std_version std;
  /* The name to use for the type, if different from that generated internally
     (e.g., "signed size_t").  */
  const char *name;
  /* The type itself.  */
  tree *type;
} format_type_detail;


/* Macros to fill out tables of these.  */
#define BADLEN	{ 0, NULL, NULL }
#define NOLENGTHS	{ BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN }


/* Structure describing a format conversion specifier (or a set of specifiers
   which act identically), and the length modifiers used with it.  */
typedef struct
{
  const char *const format_chars;
  const int pointer_count;
  const enum format_std_version std;
  /* Types accepted for each length modifier.  */
  const format_type_detail types[FMT_LEN_MAX];
  /* List of other modifier characters allowed with these specifiers.
     This lists flags, and additionally "w" for width, "p" for precision
     (right precision, for strfmon), "#" for left precision (strfmon),
     "a" for scanf "a" allocation extension (not applicable in C99 mode),
     "*" for scanf suppression, and "E" and "O" for those strftime
     modifiers.  */
  const char *const flag_chars;
  /* List of additional flags describing these conversion specifiers.
     "c" for generic character pointers being allowed, "2" for strftime
     two digit year formats, "3" for strftime formats giving two digit
     years in some locales, "4" for "2" which becomes "3" with an "E" modifier,
     "o" if use of strftime "O" is a GNU extension beyond C99,
     "W" if the argument is a pointer which is dereferenced and written into,
     "R" if the argument is a pointer which is dereferenced and read from,
     "i" for printf integer formats where the '0' flag is ignored with
     precision, and "[" for the starting character of a scanf scanset.  */
  const char *const flags2;
} format_char_info;


/* Structure describing a flag accepted by some kind of format.  */
typedef struct
{
  /* The flag character in question (0 for end of array).  */
  const int flag_char;
  /* Zero if this entry describes the flag character in general, or a
     non-zero character that may be found in flags2 if it describes the
     flag when used with certain formats only.  If the latter, only
     the first such entry found that applies to the current conversion
     specifier is used; the values of `name' and `long_name' it supplies
     will be used, if non-NULL and the standard version is higher than
     the unpredicated one, for any pedantic warning.  For example, 'o'
     for strftime formats (meaning 'O' is an extension over C99).  */
  const int predicate;
  /* Nonzero if the next character after this flag in the format should
     be skipped ('=' in strfmon), zero otherwise.  */
  const int skip_next_char;
  /* The name to use for this flag in diagnostic messages.  For example,
     N_("`0' flag"), N_("field width").  */
  const char *const name;
  /* Long name for this flag in diagnostic messages; currently only used for
     "ISO C does not support ...".  For example, N_("the `I' printf flag").  */
  const char *const long_name;
  /* The standard version in which it appeared.  */
  const enum format_std_version std;
} format_flag_spec;


/* Structure describing a combination of flags that is bad for some kind
   of format.  */
typedef struct
{
  /* The first flag character in question (0 for end of array).  */
  const int flag_char1;
  /* The second flag character.  */
  const int flag_char2;
  /* Non-zero if the message should say that the first flag is ignored with
     the second, zero if the combination should simply be objected to.  */
  const int ignored;
  /* Zero if this entry applies whenever this flag combination occurs,
     a non-zero character from flags2 if it only applies in some
     circumstances (e.g. 'i' for printf formats ignoring 0 with precision).  */
  const int predicate;
} format_flag_pair;


/* Structure describing a particular kind of format processed by GCC.  */
typedef struct
{
  /* The name of this kind of format, for use in diagnostics.  Also
     the name of the attribute (without preceding and following __).  */
  const char *const name;
  /* Specifications of the length modifiers accepted; possibly NULL.  */
  const format_length_info *const length_char_specs;
  /* Details of the conversion specification characters accepted.  */
  const format_char_info *const conversion_specs;
  /* String listing the flag characters that are accepted.  */
  const char *const flag_chars;
  /* String listing modifier characters (strftime) accepted.  May be NULL.  */
  const char *const modifier_chars;
  /* Details of the flag characters, including pseudo-flags.  */
  const format_flag_spec *const flag_specs;
  /* Details of bad combinations of flags.  */
  const format_flag_pair *const bad_flag_pairs;
  /* Flags applicable to this kind of format.  */
  const int flags;
  /* Flag character to treat a width as, or 0 if width not used.  */
  const int width_char;
  /* Flag character to treat a left precision (strfmon) as,
     or 0 if left precision not used.  */
  const int left_precision_char;
  /* Flag character to treat a precision (for strfmon, right precision) as,
     or 0 if precision not used.  */
  const int precision_char;
  /* If a flag character has the effect of suppressing the conversion of
     an argument ('*' in scanf), that flag character, otherwise 0.  */
  const int suppression_char;
  /* Flag character to treat a length modifier as (ignored if length
     modifiers not used).  Need not be placed in flag_chars for conversion
     specifiers, but is used to check for bad combinations such as length
     modifier with assignment suppression in scanf.  */
  const int length_code_char;
  /* Pointer to type of argument expected if '*' is used for a width,
     or NULL if '*' not used for widths.  */
  tree *const width_type;
  /* Pointer to type of argument expected if '*' is used for a precision,
     or NULL if '*' not used for precisions.  */
  tree *const precision_type;
} format_kind_info;


/* Structure describing details of a type expected in format checking,
   and the type to check against it.  */
typedef struct format_wanted_type
{
  /* The type wanted.  */
  tree wanted_type;
  /* The name of this type to use in diagnostics.  */
  const char *wanted_type_name;
  /* The level of indirection through pointers at which this type occurs.  */
  int pointer_count;
  /* Whether, when pointer_count is 1, to allow any character type when
     pedantic, rather than just the character or void type specified.  */
  int char_lenient_flag;
  /* Whether the argument, dereferenced once, is written into and so the
     argument must not be a pointer to a const-qualified type.  */
  int writing_in_flag;
  /* Whether the argument, dereferenced once, is read from and so
     must not be a NULL pointer.  */
  int reading_from_flag;
  /* If warnings should be of the form "field precision is not type int",
     the name to use (in this case "field precision"), otherwise NULL,
     for "%s format, %s arg" type messages.  If (in an extension), this
     is a pointer type, wanted_type_name should be set to include the
     terminating '*' characters of the type name to give a correct
     message.  */
  const char *name;
  /* The actual parameter to check against the wanted type.  */
  tree param;
  /* The argument number of that parameter.  */
  int arg_num;
  /* The next type to check for this format conversion, or NULL if none.  */
  struct format_wanted_type *next;
} format_wanted_type;


static const format_length_info printf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, "hh", FMT_LEN_hh, STD_C99 },
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C9L },
  { "q", FMT_LEN_ll, STD_EXT, NULL, 0, 0 },
  { "L", FMT_LEN_L, STD_C89, NULL, 0, 0 },
  { "z", FMT_LEN_z, STD_C99, NULL, 0, 0 },
  { "Z", FMT_LEN_z, STD_EXT, NULL, 0, 0 },
  { "t", FMT_LEN_t, STD_C99, NULL, 0, 0 },
  { "j", FMT_LEN_j, STD_C99, NULL, 0, 0 },
  { NULL, 0, 0, NULL, 0, 0 }
};


/* This differs from printf_length_specs only in that "Z" is not accepted.  */
static const format_length_info scanf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, "hh", FMT_LEN_hh, STD_C99 },
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C9L },
  { "q", FMT_LEN_ll, STD_EXT, NULL, 0, 0 },
  { "L", FMT_LEN_L, STD_C89, NULL, 0, 0 },
  { "z", FMT_LEN_z, STD_C99, NULL, 0, 0 },
  { "t", FMT_LEN_t, STD_C99, NULL, 0, 0 },
  { "j", FMT_LEN_j, STD_C99, NULL, 0, 0 },
  { NULL, 0, 0, NULL, 0, 0 }
};


/* All tables for strfmon use STD_C89 everywhere, since -pedantic warnings
   make no sense for a format type not part of any C standard version.  */
static const format_length_info strfmon_length_specs[] =
{
  /* A GNU extension.  */
  { "L", FMT_LEN_L, STD_C89, NULL, 0, 0 },
  { NULL, 0, 0, NULL, 0, 0 }
};

static const format_flag_spec printf_flag_specs[] =
{
  { ' ',  0, 0, N_("` ' flag"),        N_("the ` ' printf flag"),              STD_C89 },
  { '+',  0, 0, N_("`+' flag"),        N_("the `+' printf flag"),              STD_C89 },
  { '#',  0, 0, N_("`#' flag"),        N_("the `#' printf flag"),              STD_C89 },
  { '0',  0, 0, N_("`0' flag"),        N_("the `0' printf flag"),              STD_C89 },
  { '-',  0, 0, N_("`-' flag"),        N_("the `-' printf flag"),              STD_C89 },
  { '\'', 0, 0, N_("`'' flag"),        N_("the `'' printf flag"),              STD_EXT },
  { 'I',  0, 0, N_("`I' flag"),        N_("the `I' printf flag"),              STD_EXT },
  { 'w',  0, 0, N_("field width"),     N_("field width in printf format"),     STD_C89 },
  { 'p',  0, 0, N_("precision"),       N_("precision in printf format"),       STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair printf_flag_pairs[] =
{
  { ' ', '+', 1, 0   },
  { '0', '-', 1, 0   },
  { '0', 'p', 1, 'i' },
  { 0, 0, 0, 0 }
};


static const format_flag_spec scanf_flag_specs[] =
{
  { '*',  0, 0, N_("assignment suppression"), N_("the assignment suppression scanf feature"), STD_C89 },
  { 'a',  0, 0, N_("`a' flag"),               N_("the `a' scanf flag"),                       STD_EXT },
  { 'w',  0, 0, N_("field width"),            N_("field width in scanf format"),              STD_C89 },
  { 'L',  0, 0, N_("length modifier"),        N_("length modifier in scanf format"),          STD_C89 },
  { '\'', 0, 0, N_("`'' flag"),               N_("the `'' scanf flag"),                       STD_EXT },
  { 'I',  0, 0, N_("`I' flag"),               N_("the `I' scanf flag"),                       STD_EXT },
  { 0, 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair scanf_flag_pairs[] =
{
  { '*', 'L', 0, 0 },
  { 0, 0, 0, 0 }
};


static const format_flag_spec strftime_flag_specs[] =
{
  { '_', 0,   0, N_("`_' flag"),     N_("the `_' strftime flag"),          STD_EXT },
  { '-', 0,   0, N_("`-' flag"),     N_("the `-' strftime flag"),          STD_EXT },
  { '0', 0,   0, N_("`0' flag"),     N_("the `0' strftime flag"),          STD_EXT },
  { '^', 0,   0, N_("`^' flag"),     N_("the `^' strftime flag"),          STD_EXT },
  { '#', 0,   0, N_("`#' flag"),     N_("the `#' strftime flag"),          STD_EXT },
  { 'w', 0,   0, N_("field width"),  N_("field width in strftime format"), STD_EXT },
  { 'E', 0,   0, N_("`E' modifier"), N_("the `E' strftime modifier"),      STD_C99 },
  { 'O', 0,   0, N_("`O' modifier"), N_("the `O' strftime modifier"),      STD_C99 },
  { 'O', 'o', 0, NULL,               N_("the `O' modifier"),               STD_EXT },
  { 0, 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair strftime_flag_pairs[] =
{
  { 'E', 'O', 0, 0 },
  { '_', '-', 0, 0 },
  { '_', '0', 0, 0 },
  { '-', '0', 0, 0 },
  { '^', '#', 0, 0 },
  { 0, 0, 0, 0 }
};


static const format_flag_spec strfmon_flag_specs[] =
{
  { '=',  0, 1, N_("fill character"),  N_("fill character in strfmon format"),  STD_C89 },
  { '^',  0, 0, N_("`^' flag"),        N_("the `^' strfmon flag"),              STD_C89 },
  { '+',  0, 0, N_("`+' flag"),        N_("the `+' strfmon flag"),              STD_C89 },
  { '(',  0, 0, N_("`(' flag"),        N_("the `(' strfmon flag"),              STD_C89 },
  { '!',  0, 0, N_("`!' flag"),        N_("the `!' strfmon flag"),              STD_C89 },
  { '-',  0, 0, N_("`-' flag"),        N_("the `-' strfmon flag"),              STD_C89 },
  { 'w',  0, 0, N_("field width"),     N_("field width in strfmon format"),     STD_C89 },
  { '#',  0, 0, N_("left precision"),  N_("left precision in strfmon format"),  STD_C89 },
  { 'p',  0, 0, N_("right precision"), N_("right precision in strfmon format"), STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in strfmon format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, 0 }
};

static const format_flag_pair strfmon_flag_pairs[] =
{
  { '+', '(', 0, 0 },
  { 0, 0, 0, 0 }
};


#define T_I	&integer_type_node
#define T89_I	{ STD_C89, NULL, T_I }
#define T99_I	{ STD_C99, NULL, T_I }
#define T_L	&long_integer_type_node
#define T89_L	{ STD_C89, NULL, T_L }
#define T_LL	&long_long_integer_type_node
#define T9L_LL	{ STD_C9L, NULL, T_LL }
#define TEX_LL	{ STD_EXT, NULL, T_LL }
#define T_S	&short_integer_type_node
#define T89_S	{ STD_C89, NULL, T_S }
#define T_UI	&unsigned_type_node
#define T89_UI	{ STD_C89, NULL, T_UI }
#define T99_UI	{ STD_C99, NULL, T_UI }
#define T_UL	&long_unsigned_type_node
#define T89_UL	{ STD_C89, NULL, T_UL }
#define T_ULL	&long_long_unsigned_type_node
#define T9L_ULL	{ STD_C9L, NULL, T_ULL }
#define TEX_ULL	{ STD_EXT, NULL, T_ULL }
#define T_US	&short_unsigned_type_node
#define T89_US	{ STD_C89, NULL, T_US }
#define T_F	&float_type_node
#define T89_F	{ STD_C89, NULL, T_F }
#define T99_F	{ STD_C99, NULL, T_F }
#define T_D	&double_type_node
#define T89_D	{ STD_C89, NULL, T_D }
#define T99_D	{ STD_C99, NULL, T_D }
#define T_LD	&long_double_type_node
#define T89_LD	{ STD_C89, NULL, T_LD }
#define T99_LD	{ STD_C99, NULL, T_LD }
#define T_C	&char_type_node
#define T89_C	{ STD_C89, NULL, T_C }
#define T_SC	&signed_char_type_node
#define T99_SC	{ STD_C99, NULL, T_SC }
#define T_UC	&unsigned_char_type_node
#define T99_UC	{ STD_C99, NULL, T_UC }
#define T_V	&void_type_node
#define T89_V	{ STD_C89, NULL, T_V }
#define T_W	&wchar_type_node
#define T94_W	{ STD_C94, "wchar_t", T_W }
#define TEX_W	{ STD_EXT, "wchar_t", T_W }
#define T_WI	&wint_type_node
#define T94_WI	{ STD_C94, "wint_t", T_WI }
#define TEX_WI	{ STD_EXT, "wint_t", T_WI }
#define T_ST    &size_type_node
#define T99_ST	{ STD_C99, "size_t", T_ST }
#define T_SST   &signed_size_type_node
#define T99_SST	{ STD_C99, "signed size_t", T_SST }
#define T_PD    &ptrdiff_type_node
#define T99_PD	{ STD_C99, "ptrdiff_t", T_PD }
#define T_UPD   &unsigned_ptrdiff_type_node
#define T99_UPD	{ STD_C99, "unsigned ptrdiff_t", T_UPD }
#define T_IM    &intmax_type_node
#define T99_IM	{ STD_C99, "intmax_t", T_IM }
#define T_UIM   &uintmax_type_node
#define T99_UIM	{ STD_C99, "uintmax_t", T_UIM }

static const format_char_info print_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  TEX_LL,  T99_SST, T99_PD,  T99_IM  }, "-wp0 +'I", "i"  },
  { "oxX", 0, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "-wp0#",    "i"  },
  { "u",   0, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "-wp0'I",   "i"  },
  { "fgG", 0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#'", ""   },
  { "eE",  0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#",  ""   },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T94_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       ""   },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",      "cR" },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       "c"  },
  { "n",   1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  BADLEN,  T99_SST, T99_PD,  T99_IM  }, "",         "W"  },
  /* C99 conversion specifiers.  */
  { "F",   0, STD_C99, { T99_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#'", ""   },
  { "aA",  0, STD_C99, { T99_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#",  ""   },
  /* X/Open conversion specifiers.  */
  { "C",   0, STD_EXT, { TEX_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       ""   },
  { "S",   1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",      "R"  },
  /* GNU conversion specifiers.  */
  { "m",   0, STD_EXT, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",      ""   },
  { NULL,  0, 0, NOLENGTHS, NULL, NULL }
};

static const format_char_info scan_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",    1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  TEX_LL,  T99_SST, T99_PD,  T99_IM  }, "*w'I", "W"   },
  { "u",     1, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "*w'I", "W"   },
  { "oxX",   1, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "*w",   "W"   },
  { "efgEG", 1, STD_C89, { T89_F,   BADLEN,  BADLEN,  T89_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN  }, "*w'",  "W"   },
  { "c",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*w",   "cW"  },
  { "s",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*aw",  "cW"  },
  { "[",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*aw",  "cW[" },
  { "p",     2, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*w",   "W"   },
  { "n",     1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  BADLEN,  T99_SST, T99_PD,  T99_IM  }, "",     "W"   },
  /* C99 conversion specifiers.  */
  { "FaA",   1, STD_C99, { T99_F,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN  }, "*w'",  "W"   },
  /* X/Open conversion specifiers.  */
  { "C",     1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*w",   "W"   },
  { "S",     1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*aw",  "W"   },
  { NULL, 0, 0, NOLENGTHS, NULL, NULL }
};

static const format_char_info time_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "ABZab",		0, STD_C89, NOLENGTHS, "^#",     ""   },
  { "cx", 		0, STD_C89, NOLENGTHS, "E",      "3"  },
  { "HIMSUWdmw",	0, STD_C89, NOLENGTHS, "-_0Ow",  ""   },
  { "j",		0, STD_C89, NOLENGTHS, "-_0Ow",  "o"  },
  { "p",		0, STD_C89, NOLENGTHS, "#",      ""   },
  { "X",		0, STD_C89, NOLENGTHS, "E",      ""   },
  { "y", 		0, STD_C89, NOLENGTHS, "EO-_0w", "4"  },
  { "Y",		0, STD_C89, NOLENGTHS, "-_0EOw", "o"  },
  { "%",		0, STD_C89, NOLENGTHS, "",       ""   },
  /* C99 conversion specifiers.  */
  { "C",		0, STD_C99, NOLENGTHS, "-_0EOw", "o"  },
  { "D", 		0, STD_C99, NOLENGTHS, "",       "2"  },
  { "eVu",		0, STD_C99, NOLENGTHS, "-_0Ow",  ""   },
  { "FRTnrt",		0, STD_C99, NOLENGTHS, "",       ""   },
  { "g", 		0, STD_C99, NOLENGTHS, "O-_0w",  "2o" },
  { "G",		0, STD_C99, NOLENGTHS, "-_0Ow",  "o"  },
  { "h",		0, STD_C99, NOLENGTHS, "^#",     ""   },
  { "z",		0, STD_C99, NOLENGTHS, "O",      "o"  },
  /* GNU conversion specifiers.  */
  { "kls",		0, STD_EXT, NOLENGTHS, "-_0Ow",  ""   },
  { "P",		0, STD_EXT, NOLENGTHS, "",       ""   },
  { NULL,		0, 0, NOLENGTHS, NULL, NULL }
};

static const format_char_info monetary_char_table[] =
{
  { "in", 0, STD_C89, { T89_D, BADLEN, BADLEN, BADLEN, BADLEN, T89_LD, BADLEN, BADLEN, BADLEN }, "=^+(!-w#p", "" },
  { NULL, 0, 0, NOLENGTHS, NULL, NULL }
};


/* This must be in the same order as enum format_type.  */
static const format_kind_info format_types[] =
{
  { "printf",   printf_length_specs,  print_char_table, " +#0-'I", NULL, 
    printf_flag_specs, printf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_DOLLAR_MULTIPLE|FMT_FLAG_USE_DOLLAR|FMT_FLAG_EMPTY_PREC_OK,
    'w', 0, 'p', 0, 'L',
    &integer_type_node, &integer_type_node
  },
  { "scanf",    scanf_length_specs,   scan_char_table,  "*'I", NULL, 
    scanf_flag_specs, scanf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_SCANF_A_KLUDGE|FMT_FLAG_USE_DOLLAR|FMT_FLAG_ZERO_WIDTH_BAD|FMT_FLAG_DOLLAR_GAP_POINTER_OK,
    'w', 0, 0, '*', 'L',
    NULL, NULL
  },
  { "strftime", NULL,                 time_char_table,  "_-0^#", "EO",
    strftime_flag_specs, strftime_flag_pairs,
    FMT_FLAG_FANCY_PERCENT_OK, 'w', 0, 0, 0, 0,
    NULL, NULL
  },
  { "strfmon",  strfmon_length_specs, monetary_char_table, "=^+(!-", NULL, 
    strfmon_flag_specs, strfmon_flag_pairs,
    FMT_FLAG_ARG_CONVERT, 'w', '#', 'p', 0, 'L',
    NULL, NULL
  }
};


/* Structure detailing the results of checking a format function call
   where the format expression may be a conditional expression with
   many leaves resulting from nested conditional expressions.  */
typedef struct
{
  /* Number of leaves of the format argument that could not be checked
     as they were not string literals.  */
  int number_non_literal;
  /* Number of leaves of the format argument that were null pointers or
     string literals, but had extra format arguments.  */
  int number_extra_args;
  /* Number of leaves of the format argument that were null pointers or
     string literals, but had extra format arguments and used $ operand
     numbers.  */
  int number_dollar_extra_args;
  /* Number of leaves of the format argument that were wide string
     literals.  */
  int number_wide;
  /* Number of leaves of the format argument that were empty strings.  */
  int number_empty;
  /* Number of leaves of the format argument that were unterminated
     strings.  */
  int number_unterminated;
  /* Number of leaves of the format argument that were not counted above.  */
  int number_other;
} format_check_results;

static void check_format_info	PARAMS ((int *, function_format_info *, tree));
static void check_format_info_recurse PARAMS ((int *, format_check_results *,
					       function_format_info *, tree,
					       tree, unsigned HOST_WIDE_INT));
static void check_format_info_main PARAMS ((int *, format_check_results *,
					    function_format_info *,
					    const char *, int, tree,
					    unsigned HOST_WIDE_INT));
static void status_warning PARAMS ((int *, const char *, ...))
     ATTRIBUTE_PRINTF_2;

static void init_dollar_format_checking		PARAMS ((int, tree));
static int maybe_read_dollar_number		PARAMS ((int *, const char **, int,
							 tree, tree *,
							 const format_kind_info *));
static void finish_dollar_format_checking	PARAMS ((int *, format_check_results *, int));

static const format_flag_spec *get_flag_spec	PARAMS ((const format_flag_spec *,
							 int, const char *));

static void check_format_types	PARAMS ((int *, format_wanted_type *));

/* Decode a format type from a string, returning the type, or
   format_type_error if not valid, in which case the caller should print an
   error message.  */
static enum format_type
decode_format_type (s)
     const char *s;
{
  int i;
  int slen;
  slen = strlen (s);
  for (i = 0; i < (int) format_type_error; i++)
    {
      int alen;
      if (!strcmp (s, format_types[i].name))
	break;
      alen = strlen (format_types[i].name);
      if (slen == alen + 4 && s[0] == '_' && s[1] == '_'
	  && s[slen - 1] == '_' && s[slen - 2] == '_'
	  && !strncmp (s + 2, format_types[i].name, alen))
	break;
    }
  return ((enum format_type) i);
}


/* Check the argument list of a call to printf, scanf, etc.
   ATTRS are the attributes on the function type.
   PARAMS is the list of argument values.  Also, if -Wmissing-format-attribute,
   warn for calls to vprintf or vscanf in functions with no such format
   attribute themselves.  */

void
check_function_format (status, attrs, params)
     int *status;
     tree attrs;
     tree params;
{
  tree a;

  /* See if this function has any format attributes.  */
  for (a = attrs; a; a = TREE_CHAIN (a))
    {
      if (is_attribute_p ("format", TREE_PURPOSE (a)))
	{
	  /* Yup; check it.  */
	  function_format_info info;
	  decode_format_attr (TREE_VALUE (a), &info, 1);
	  check_format_info (status, &info, params);
	  if (warn_missing_format_attribute && info.first_arg_num == 0
	      && (format_types[info.format_type].flags
		  & (int) FMT_FLAG_ARG_CONVERT))
	    {
	      tree c;
	      for (c = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
		   c;
		   c = TREE_CHAIN (c))
		if (is_attribute_p ("format", TREE_PURPOSE (c))
		    && (decode_format_type (IDENTIFIER_POINTER
					    (TREE_VALUE (TREE_VALUE (c))))
			== info.format_type))
		  break;
	      if (c == NULL_TREE)
		{
		  /* Check if the current function has a parameter to which
		     the format attribute could be attached; if not, it
		     can't be a candidate for a format attribute, despite
		     the vprintf-like or vscanf-like call.  */
		  tree args;
		  for (args = DECL_ARGUMENTS (current_function_decl);
		       args != 0;
		       args = TREE_CHAIN (args))
		    {
		      if (TREE_CODE (TREE_TYPE (args)) == POINTER_TYPE
			  && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (args)))
			      == char_type_node))
			break;
		    }
		  if (args != 0)
		    warning ("function might be possible candidate for `%s' format attribute",
			     format_types[info.format_type].name);
		}
	    }
	}
    }
}

/* This function replaces `warning' inside the printf format checking
   functions.  If the `status' parameter is non-NULL, then it is
   dereferenced and set to 1 whenever a warning is caught.  Otherwise
   it warns as usual by replicating the innards of the warning
   function from diagnostic.c.  */
static void
status_warning VPARAMS ((int *status, const char *msgid, ...))
{
  diagnostic_context dc;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, int *, status);
  VA_FIXEDARG (ap, const char *, msgid);

  if (status)
    *status = 1;
  else
    {
      /* This duplicates the warning function behavior.  */
      set_diagnostic_context
	(&dc, msgid, &ap, input_filename, lineno, /* warn = */ 1);
      report_diagnostic (&dc);
    }

  VA_CLOSE (ap);
}

/* Variables used by the checking of $ operand number formats.  */
static char *dollar_arguments_used = NULL;
static char *dollar_arguments_pointer_p = NULL;
static int dollar_arguments_alloc = 0;
static int dollar_arguments_count;
static int dollar_first_arg_num;
static int dollar_max_arg_used;
static int dollar_format_warned;

/* Initialize the checking for a format string that may contain $
   parameter number specifications; we will need to keep track of whether
   each parameter has been used.  FIRST_ARG_NUM is the number of the first
   argument that is a parameter to the format, or 0 for a vprintf-style
   function; PARAMS is the list of arguments starting at this argument.  */

static void
init_dollar_format_checking (first_arg_num, params)
     int first_arg_num;
     tree params;
{
  tree oparams = params;

  dollar_first_arg_num = first_arg_num;
  dollar_arguments_count = 0;
  dollar_max_arg_used = 0;
  dollar_format_warned = 0;
  if (first_arg_num > 0)
    {
      while (params)
	{
	  dollar_arguments_count++;
	  params = TREE_CHAIN (params);
	}
    }
  if (dollar_arguments_alloc < dollar_arguments_count)
    {
      if (dollar_arguments_used)
	free (dollar_arguments_used);
      if (dollar_arguments_pointer_p)
	free (dollar_arguments_pointer_p);
      dollar_arguments_alloc = dollar_arguments_count;
      dollar_arguments_used = xmalloc (dollar_arguments_alloc);
      dollar_arguments_pointer_p = xmalloc (dollar_arguments_alloc);
    }
  if (dollar_arguments_alloc)
    {
      memset (dollar_arguments_used, 0, dollar_arguments_alloc);
      if (first_arg_num > 0)
	{
	  int i = 0;
	  params = oparams;
	  while (params)
	    {
	      dollar_arguments_pointer_p[i] = (TREE_CODE (TREE_TYPE (TREE_VALUE (params)))
					       == POINTER_TYPE);
	      params = TREE_CHAIN (params);
	      i++;
	    }
	}
    }
}


/* Look for a decimal number followed by a $ in *FORMAT.  If DOLLAR_NEEDED
   is set, it is an error if one is not found; otherwise, it is OK.  If
   such a number is found, check whether it is within range and mark that
   numbered operand as being used for later checking.  Returns the operand
   number if found and within range, zero if no such number was found and
   this is OK, or -1 on error.  PARAMS points to the first operand of the
   format; PARAM_PTR is made to point to the parameter referred to.  If
   a $ format is found, *FORMAT is updated to point just after it.  */

static int
maybe_read_dollar_number (status, format, dollar_needed, params, param_ptr,
			  fki)
     int *status;
     const char **format;
     int dollar_needed;
     tree params;
     tree *param_ptr;
     const format_kind_info *fki;
{
  int argnum;
  int overflow_flag;
  const char *fcp = *format;
  if (! ISDIGIT (*fcp))
    {
      if (dollar_needed)
	{
	  status_warning (status, "missing $ operand number in format");
	  return -1;
	}
      else
	return 0;
    }
  argnum = 0;
  overflow_flag = 0;
  while (ISDIGIT (*fcp))
    {
      int nargnum;
      nargnum = 10 * argnum + (*fcp - '0');
      if (nargnum < 0 || nargnum / 10 != argnum)
	overflow_flag = 1;
      argnum = nargnum;
      fcp++;
    }
  if (*fcp != '$')
    {
      if (dollar_needed)
	{
	  status_warning (status, "missing $ operand number in format");
	  return -1;
	}
      else
	return 0;
    }
  *format = fcp + 1;
  if (pedantic && !dollar_format_warned)
    {
      status_warning (status,
		      "%s does not support %%n$ operand number formats",
		      C_STD_NAME (STD_EXT));
      dollar_format_warned = 1;
    }
  if (overflow_flag || argnum == 0
      || (dollar_first_arg_num && argnum > dollar_arguments_count))
    {
      status_warning (status, "operand number out of range in format");
      return -1;
    }
  if (argnum > dollar_max_arg_used)
    dollar_max_arg_used = argnum;
  /* For vprintf-style functions we may need to allocate more memory to
     track which arguments are used.  */
  while (dollar_arguments_alloc < dollar_max_arg_used)
    {
      int nalloc;
      nalloc = 2 * dollar_arguments_alloc + 16;
      dollar_arguments_used = xrealloc (dollar_arguments_used, nalloc);
      dollar_arguments_pointer_p = xrealloc (dollar_arguments_pointer_p,
					     nalloc);
      memset (dollar_arguments_used + dollar_arguments_alloc, 0,
	      nalloc - dollar_arguments_alloc);
      dollar_arguments_alloc = nalloc;
    }
  if (!(fki->flags & (int) FMT_FLAG_DOLLAR_MULTIPLE)
      && dollar_arguments_used[argnum - 1] == 1)
    {
      dollar_arguments_used[argnum - 1] = 2;
      status_warning (status,
		      "format argument %d used more than once in %s format",
		      argnum, fki->name);
    }
  else
    dollar_arguments_used[argnum - 1] = 1;
  if (dollar_first_arg_num)
    {
      int i;
      *param_ptr = params;
      for (i = 1; i < argnum && *param_ptr != 0; i++)
	*param_ptr = TREE_CHAIN (*param_ptr);

      if (*param_ptr == 0)
	{
	  /* This case shouldn't be caught here.  */
	  abort ();
	}
    }
  else
    *param_ptr = 0;
  return argnum;
}


/* Finish the checking for a format string that used $ operand number formats
   instead of non-$ formats.  We check for unused operands before used ones
   (a serious error, since the implementation of the format function
   can't know what types to pass to va_arg to find the later arguments).
   and for unused operands at the end of the format (if we know how many
   arguments the format had, so not for vprintf).  If there were operand
   numbers out of range on a non-vprintf-style format, we won't have reached
   here.  If POINTER_GAP_OK, unused arguments are OK if all arguments are
   pointers.  */

static void
finish_dollar_format_checking (status, res, pointer_gap_ok)
     int *status;
     format_check_results *res;
     int pointer_gap_ok;
{
  int i;
  bool found_pointer_gap = false;
  for (i = 0; i < dollar_max_arg_used; i++)
    {
      if (!dollar_arguments_used[i])
	{
	  if (pointer_gap_ok && (dollar_first_arg_num == 0
				 || dollar_arguments_pointer_p[i]))
	    found_pointer_gap = true;
	  else
	    status_warning (status, "format argument %d unused before used argument %d in $-style format",
			    i + 1, dollar_max_arg_used);
	}
    }
  if (found_pointer_gap
      || (dollar_first_arg_num
	  && dollar_max_arg_used < dollar_arguments_count))
    {
      res->number_other--;
      res->number_dollar_extra_args++;
    }
}


/* Retrieve the specification for a format flag.  SPEC contains the
   specifications for format flags for the applicable kind of format.
   FLAG is the flag in question.  If PREDICATES is NULL, the basic
   spec for that flag must be retrieved and this function aborts if
   it cannot be found.  If PREDICATES is not NULL, it is a string listing
   possible predicates for the spec entry; if an entry predicated on any
   of these is found, it is returned, otherwise NULL is returned.  */

static const format_flag_spec *
get_flag_spec (spec, flag, predicates)
     const format_flag_spec *spec;
     int flag;
     const char *predicates;
{
  int i;
  for (i = 0; spec[i].flag_char != 0; i++)
    {
      if (spec[i].flag_char != flag)
	continue;
      if (predicates != NULL)
	{
	  if (spec[i].predicate != 0
	      && strchr (predicates, spec[i].predicate) != 0)
	    return &spec[i];
	}
      else if (spec[i].predicate == 0)
	return &spec[i];
    }
  if (predicates == NULL)
    abort ();
  else
    return NULL;
}


/* Check the argument list of a call to printf, scanf, etc.
   INFO points to the function_format_info structure.
   PARAMS is the list of argument values.  */

static void
check_format_info (status, info, params)
     int *status;
     function_format_info *info;
     tree params;
{
  unsigned HOST_WIDE_INT arg_num;
  tree format_tree;
  format_check_results res;
  /* Skip to format argument.  If the argument isn't available, there's
     no work for us to do; prototype checking will catch the problem.  */
  for (arg_num = 1; ; ++arg_num)
    {
      if (params == 0)
	return;
      if (arg_num == info->format_num)
	break;
      params = TREE_CHAIN (params);
    }
  format_tree = TREE_VALUE (params);
  params = TREE_CHAIN (params);
  if (format_tree == 0)
    return;

  res.number_non_literal = 0;
  res.number_extra_args = 0;
  res.number_dollar_extra_args = 0;
  res.number_wide = 0;
  res.number_empty = 0;
  res.number_unterminated = 0;
  res.number_other = 0;

  check_format_info_recurse (status, &res, info, format_tree, params, arg_num);

  if (res.number_non_literal > 0)
    {
      /* Functions taking a va_list normally pass a non-literal format
	 string.  These functions typically are declared with
	 first_arg_num == 0, so avoid warning in those cases.  */
      if (!(format_types[info->format_type].flags & (int) FMT_FLAG_ARG_CONVERT))
	{
	  /* For strftime-like formats, warn for not checking the format
	     string; but there are no arguments to check.  */
	  if (warn_format_nonliteral)
	    status_warning (status, "format not a string literal, format string not checked");
	}
      else if (info->first_arg_num != 0)
	{
	  /* If there are no arguments for the format at all, we may have
	     printf (foo) which is likely to be a security hole.  */
	  while (arg_num + 1 < info->first_arg_num)
	    {
	      if (params == 0)
		break;
	      params = TREE_CHAIN (params);
	      ++arg_num;
	    }
	  if (params == 0 && (warn_format_nonliteral || warn_format_security))
	    status_warning (status, "format not a string literal and no format arguments");
	  else if (warn_format_nonliteral)
	    status_warning (status, "format not a string literal, argument types not checked");
	}
    }

  /* If there were extra arguments to the format, normally warn.  However,
     the standard does say extra arguments are ignored, so in the specific
     case where we have multiple leaves (conditional expressions or
     ngettext) allow extra arguments if at least one leaf didn't have extra
     arguments, but was otherwise OK (either non-literal or checked OK).
     If the format is an empty string, this should be counted similarly to the
     case of extra format arguments.  */
  if (res.number_extra_args > 0 && res.number_non_literal == 0
      && res.number_other == 0 && warn_format_extra_args)
    status_warning (status, "too many arguments for format");
  if (res.number_dollar_extra_args > 0 && res.number_non_literal == 0
      && res.number_other == 0 && warn_format_extra_args)
    status_warning (status, "unused arguments in $-style format");
  if (res.number_empty > 0 && res.number_non_literal == 0
      && res.number_other == 0)
    status_warning (status, "zero-length format string");

  if (res.number_wide > 0)
    status_warning (status, "format is a wide character string");

  if (res.number_unterminated > 0)
    status_warning (status, "unterminated format string");
}


/* Recursively check a call to a format function.  FORMAT_TREE is the
   format parameter, which may be a conditional expression in which
   both halves should be checked.  ARG_NUM is the number of the
   format argument; PARAMS points just after it in the argument list.  */

static void
check_format_info_recurse (status, res, info, format_tree, params, arg_num)
     int *status;
     format_check_results *res;
     function_format_info *info;
     tree format_tree;
     tree params;
     unsigned HOST_WIDE_INT arg_num;
{
  int format_length;
  HOST_WIDE_INT offset;
  const char *format_chars;
  tree array_size = 0;
  tree array_init;

  if (TREE_CODE (format_tree) == NOP_EXPR)
    {
      /* Strip coercion.  */
      check_format_info_recurse (status, res, info,
				 TREE_OPERAND (format_tree, 0), params,
				 arg_num);
      return;
    }

  if (TREE_CODE (format_tree) == CALL_EXPR)
    {
      tree type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (format_tree, 0)));
      tree attrs;
      bool found_format_arg = false;

      /* See if this is a call to a known internationalization function
	 that modifies the format arg.  Such a function may have multiple
	 format_arg attributes (for example, ngettext).  */

      for (attrs = TYPE_ATTRIBUTES (type);
	   attrs;
	   attrs = TREE_CHAIN (attrs))
	if (is_attribute_p ("format_arg", TREE_PURPOSE (attrs)))
	  {
	    tree inner_args;
	    tree format_num_expr;
	    int format_num;
	    int i;

	    /* Extract the argument number, which was previously checked
	       to be valid.  */
	    format_num_expr = TREE_VALUE (TREE_VALUE (attrs));
	    while (TREE_CODE (format_num_expr) == NOP_EXPR
		   || TREE_CODE (format_num_expr) == CONVERT_EXPR
		   || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
	      format_num_expr = TREE_OPERAND (format_num_expr, 0);

	    if (TREE_CODE (format_num_expr) != INTEGER_CST
		|| TREE_INT_CST_HIGH (format_num_expr) != 0)
	      abort ();

	    format_num = TREE_INT_CST_LOW (format_num_expr);

	    for (inner_args = TREE_OPERAND (format_tree, 1), i = 1;
		 inner_args != 0;
		 inner_args = TREE_CHAIN (inner_args), i++)
	      if (i == format_num)
		{
		  check_format_info_recurse (status, res, info,
					     TREE_VALUE (inner_args), params,
					     arg_num);
		  found_format_arg = true;
		  break;
		}
	  }

      /* If we found a format_arg attribute and did a recursive check,
	 we are done with checking this format string.  Otherwise, we
	 continue and this will count as a non-literal format string.  */
      if (found_format_arg)
	return;
    }

  if (TREE_CODE (format_tree) == COND_EXPR)
    {
      /* Check both halves of the conditional expression.  */
      check_format_info_recurse (status, res, info,
				 TREE_OPERAND (format_tree, 1), params,
				 arg_num);
      check_format_info_recurse (status, res, info,
				 TREE_OPERAND (format_tree, 2), params,
				 arg_num);
      return;
    }

  if (integer_zerop (format_tree))
    {
      /* FIXME: this warning should go away once Marc Espie's
	 __attribute__((nonnull)) patch is in.  Instead, checking for
	 nonnull attributes should probably change this function to act
	 specially if info == NULL and add a res->number_null entry for
	 that case, or maybe add a function pointer to be called at
	 the end instead of hardcoding check_format_info_main.  */
      status_warning (status, "null format string");

      /* Skip to first argument to check, so we can see if this format
	 has any arguments (it shouldn't).  */
      while (arg_num + 1 < info->first_arg_num)
	{
	  if (params == 0)
	    return;
	  params = TREE_CHAIN (params);
	  ++arg_num;
	}

      if (params == 0)
	res->number_other++;
      else
	res->number_extra_args++;

      return;
    }

  offset = 0;
  if (TREE_CODE (format_tree) == PLUS_EXPR)
    {
      tree arg0, arg1;

      arg0 = TREE_OPERAND (format_tree, 0);
      arg1 = TREE_OPERAND (format_tree, 1);
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);
      if (TREE_CODE (arg1) == INTEGER_CST)
	format_tree = arg0;
      else if (TREE_CODE (arg0) == INTEGER_CST)
	{
	  format_tree = arg1;
	  arg1 = arg0;
	}
      else
	{
	  res->number_non_literal++;
	  return;
	}
      if (!host_integerp (arg1, 0)
	  || (offset = tree_low_cst (arg1, 0)) < 0)
	{
	  res->number_non_literal++;
	  return;
	}
    }
  if (TREE_CODE (format_tree) != ADDR_EXPR)
    {
      res->number_non_literal++;
      return;
    }
  format_tree = TREE_OPERAND (format_tree, 0);
  if (TREE_CODE (format_tree) == VAR_DECL
      && TREE_CODE (TREE_TYPE (format_tree)) == ARRAY_TYPE
      && (array_init = decl_constant_value (format_tree)) != format_tree
      && TREE_CODE (array_init) == STRING_CST)
    {
      /* Extract the string constant initializer.  Note that this may include
	 a trailing NUL character that is not in the array (e.g.
	 const char a[3] = "foo";).  */
      array_size = DECL_SIZE_UNIT (format_tree);
      format_tree = array_init;
    }
  if (TREE_CODE (format_tree) != STRING_CST)
    {
      res->number_non_literal++;
      return;
    }
  if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (format_tree))) != char_type_node)
    {
      res->number_wide++;
      return;
    }
  format_chars = TREE_STRING_POINTER (format_tree);
  format_length = TREE_STRING_LENGTH (format_tree);
  if (array_size != 0)
    {
      /* Variable length arrays can't be initialized.  */
      if (TREE_CODE (array_size) != INTEGER_CST)
	abort ();
      if (host_integerp (array_size, 0))
	{
	  HOST_WIDE_INT array_size_value = TREE_INT_CST_LOW (array_size);
	  if (array_size_value > 0
	      && array_size_value == (int) array_size_value
	      && format_length > array_size_value)
	    format_length = array_size_value;
	}
    }
  if (offset)
    {
      if (offset >= format_length)
	{
	  res->number_non_literal++;
	  return;
	}
      format_chars += offset;
      format_length -= offset;
    }
  if (format_length < 1)
    {
      res->number_unterminated++;
      return;
    }
  if (format_length == 1)
    {
      res->number_empty++;
      return;
    }
  if (format_chars[--format_length] != 0)
    {
      res->number_unterminated++;
      return;
    }

  /* Skip to first argument to check.  */
  while (arg_num + 1 < info->first_arg_num)
    {
      if (params == 0)
	return;
      params = TREE_CHAIN (params);
      ++arg_num;
    }
  /* Provisionally increment res->number_other; check_format_info_main
     will decrement it if it finds there are extra arguments, but this way
     need not adjust it for every return.  */
  res->number_other++;
  check_format_info_main (status, res, info, format_chars, format_length,
			  params, arg_num);
}


/* Do the main part of checking a call to a format function.  FORMAT_CHARS
   is the NUL-terminated format string (which at this point may contain
   internal NUL characters); FORMAT_LENGTH is its length (excluding the
   terminating NUL character).  ARG_NUM is one less than the number of
   the first format argument to check; PARAMS points to that format
   argument in the list of arguments.  */

static void
check_format_info_main (status, res, info, format_chars, format_length,
			params, arg_num)
     int *status;
     format_check_results *res;
     function_format_info *info;
     const char *format_chars;
     int format_length;
     tree params;
     unsigned HOST_WIDE_INT arg_num;
{
  const char *orig_format_chars = format_chars;
  tree first_fillin_param = params;

  const format_kind_info *fki = &format_types[info->format_type];
  const format_flag_spec *flag_specs = fki->flag_specs;
  const format_flag_pair *bad_flag_pairs = fki->bad_flag_pairs;

  /* -1 if no conversions taking an operand have been found; 0 if one has
     and it didn't use $; 1 if $ formats are in use.  */
  int has_operand_number = -1;

  init_dollar_format_checking (info->first_arg_num, first_fillin_param);

  while (1)
    {
      int i;
      int suppressed = FALSE;
      const char *length_chars = NULL;
      enum format_lengths length_chars_val = FMT_LEN_none;
      enum format_std_version length_chars_std = STD_C89;
      int format_char;
      tree cur_param;
      tree wanted_type;
      int main_arg_num = 0;
      tree main_arg_params = 0;
      enum format_std_version wanted_type_std;
      const char *wanted_type_name;
      format_wanted_type width_wanted_type;
      format_wanted_type precision_wanted_type;
      format_wanted_type main_wanted_type;
      format_wanted_type *first_wanted_type = NULL;
      format_wanted_type *last_wanted_type = NULL;
      const format_length_info *fli = NULL;
      const format_char_info *fci = NULL;
      char flag_chars[256];
      int aflag = 0;
      if (*format_chars == 0)
	{
	  if (format_chars - orig_format_chars != format_length)
	    status_warning (status, "embedded `\\0' in format");
	  if (info->first_arg_num != 0 && params != 0
	      && has_operand_number <= 0)
	    {
	      res->number_other--;
	      res->number_extra_args++;
	    }
	  if (has_operand_number > 0)
	    finish_dollar_format_checking (status, res, fki->flags & (int) FMT_FLAG_DOLLAR_GAP_POINTER_OK);
	  return;
	}
      if (*format_chars++ != '%')
	continue;
      if (*format_chars == 0)
	{
	  status_warning (status, "spurious trailing `%%' in format");
	  continue;
	}
      if (*format_chars == '%')
	{
	  ++format_chars;
	  continue;
	}
      flag_chars[0] = 0;

      if ((fki->flags & (int) FMT_FLAG_USE_DOLLAR) && has_operand_number != 0)
	{
	  /* Possibly read a $ operand number at the start of the format.
	     If one was previously used, one is required here.  If one
	     is not used here, we can't immediately conclude this is a
	     format without them, since it could be printf %m or scanf %*.  */
	  int opnum;
	  opnum = maybe_read_dollar_number (status, &format_chars, 0,
					    first_fillin_param,
					    &main_arg_params, fki);
	  if (opnum == -1)
	    return;
	  else if (opnum > 0)
	    {
	      has_operand_number = 1;
	      main_arg_num = opnum + info->first_arg_num - 1;
	    }
	}

      /* Read any format flags, but do not yet validate them beyond removing
	 duplicates, since in general validation depends on the rest of
	 the format.  */
      while (*format_chars != 0
	     && strchr (fki->flag_chars, *format_chars) != 0)
	{
	  const format_flag_spec *s = get_flag_spec (flag_specs,
						     *format_chars, NULL);
	  if (strchr (flag_chars, *format_chars) != 0)
	    {
	      status_warning (status, "repeated %s in format", _(s->name));
	    }
	  else
	    {
	      i = strlen (flag_chars);
	      flag_chars[i++] = *format_chars;
	      flag_chars[i] = 0;
	    }
	  if (s->skip_next_char)
	    {
	      ++format_chars;
	      if (*format_chars == 0)
		{
		  status_warning (status, "missing fill character at end of strfmon format");
		  return;
		}
	    }
	  ++format_chars;
	}

      /* Read any format width, possibly * or *m$.  */
      if (fki->width_char != 0)
	{
	  if (fki->width_type != NULL && *format_chars == '*')
	    {
	      i = strlen (flag_chars);
	      flag_chars[i++] = fki->width_char;
	      flag_chars[i] = 0;
	      /* "...a field width...may be indicated by an asterisk.
		 In this case, an int argument supplies the field width..."  */
	      ++format_chars;
	      if (params == 0)
		{
		  status_warning (status, "too few arguments for format");
		  return;
		}
	      if (has_operand_number != 0)
		{
		  int opnum;
		  opnum = maybe_read_dollar_number (status, &format_chars,
						    has_operand_number == 1,
						    first_fillin_param,
						    &params, fki);
		  if (opnum == -1)
		    return;
		  else if (opnum > 0)
		    {
		      has_operand_number = 1;
		      arg_num = opnum + info->first_arg_num - 1;
		    }
		  else
		    has_operand_number = 0;
		}
	      if (info->first_arg_num != 0)
		{
		  cur_param = TREE_VALUE (params);
		  if (has_operand_number <= 0)
		    {
		      params = TREE_CHAIN (params);
		      ++arg_num;
		    }
		  width_wanted_type.wanted_type = *fki->width_type;
		  width_wanted_type.wanted_type_name = NULL;
		  width_wanted_type.pointer_count = 0;
		  width_wanted_type.char_lenient_flag = 0;
		  width_wanted_type.writing_in_flag = 0;
		  width_wanted_type.reading_from_flag = 0;
		  width_wanted_type.name = _("field width");
		  width_wanted_type.param = cur_param;
		  width_wanted_type.arg_num = arg_num;
		  width_wanted_type.next = NULL;
		  if (last_wanted_type != 0)
		    last_wanted_type->next = &width_wanted_type;
		  if (first_wanted_type == 0)
		    first_wanted_type = &width_wanted_type;
		  last_wanted_type = &width_wanted_type;
		}
	    }
	  else
	    {
	      /* Possibly read a numeric width.  If the width is zero,
		 we complain if appropriate.  */
	      int non_zero_width_char = FALSE;
	      int found_width = FALSE;
	      while (ISDIGIT (*format_chars))
		{
		  found_width = TRUE;
		  if (*format_chars != '0')
		    non_zero_width_char = TRUE;
		  ++format_chars;
		}
	      if (found_width && !non_zero_width_char &&
		  (fki->flags & (int) FMT_FLAG_ZERO_WIDTH_BAD))
		status_warning (status, "zero width in %s format",
				fki->name);
	      if (found_width)
		{
		  i = strlen (flag_chars);
		  flag_chars[i++] = fki->width_char;
		  flag_chars[i] = 0;
		}
	    }
	}

      /* Read any format left precision (must be a number, not *).  */
      if (fki->left_precision_char != 0 && *format_chars == '#')
	{
	  ++format_chars;
	  i = strlen (flag_chars);
	  flag_chars[i++] = fki->left_precision_char;
	  flag_chars[i] = 0;
	  if (!ISDIGIT (*format_chars))
	    status_warning (status, "empty left precision in %s format",
			    fki->name);
	  while (ISDIGIT (*format_chars))
	    ++format_chars;
	}

      /* Read any format precision, possibly * or *m$.  */
      if (fki->precision_char != 0 && *format_chars == '.')
	{
	  ++format_chars;
	  i = strlen (flag_chars);
	  flag_chars[i++] = fki->precision_char;
	  flag_chars[i] = 0;
	  if (fki->precision_type != NULL && *format_chars == '*')
	    {
	      /* "...a...precision...may be indicated by an asterisk.
		 In this case, an int argument supplies the...precision."  */
	      ++format_chars;
	      if (has_operand_number != 0)
		{
		  int opnum;
		  opnum = maybe_read_dollar_number (status, &format_chars,
						    has_operand_number == 1,
						    first_fillin_param,
						    &params, fki);
		  if (opnum == -1)
		    return;
		  else if (opnum > 0)
		    {
		      has_operand_number = 1;
		      arg_num = opnum + info->first_arg_num - 1;
		    }
		  else
		    has_operand_number = 0;
		}
	      if (info->first_arg_num != 0)
		{
		  if (params == 0)
		    {
		      status_warning (status, "too few arguments for format");
		      return;
		    }
		  cur_param = TREE_VALUE (params);
		  if (has_operand_number <= 0)
		    {
		      params = TREE_CHAIN (params);
		      ++arg_num;
		    }
		  precision_wanted_type.wanted_type = *fki->precision_type;
		  precision_wanted_type.wanted_type_name = NULL;
		  precision_wanted_type.pointer_count = 0;
		  precision_wanted_type.char_lenient_flag = 0;
		  precision_wanted_type.writing_in_flag = 0;
		  precision_wanted_type.reading_from_flag = 0;
		  precision_wanted_type.name = _("field precision");
		  precision_wanted_type.param = cur_param;
		  precision_wanted_type.arg_num = arg_num;
		  precision_wanted_type.next = NULL;
		  if (last_wanted_type != 0)
		    last_wanted_type->next = &precision_wanted_type;
		  if (first_wanted_type == 0)
		    first_wanted_type = &precision_wanted_type;
		  last_wanted_type = &precision_wanted_type;
		}
	    }
	  else
	    {
	      if (!(fki->flags & (int) FMT_FLAG_EMPTY_PREC_OK)
		  && !ISDIGIT (*format_chars))
		status_warning (status, "empty precision in %s format",
				fki->name);
	      while (ISDIGIT (*format_chars))
		++format_chars;
	    }
	}

      /* Read any length modifier, if this kind of format has them.  */
      fli = fki->length_char_specs;
      length_chars = NULL;
      length_chars_val = FMT_LEN_none;
      length_chars_std = STD_C89;
      if (fli)
	{
	  while (fli->name != 0 && fli->name[0] != *format_chars)
	    fli++;
	  if (fli->name != 0)
	    {
	      format_chars++;
	      if (fli->double_name != 0 && fli->name[0] == *format_chars)
		{
		  format_chars++;
		  length_chars = fli->double_name;
		  length_chars_val = fli->double_index;
		  length_chars_std = fli->double_std;
		}
	      else
		{
		  length_chars = fli->name;
		  length_chars_val = fli->index;
		  length_chars_std = fli->std;
		}
	      i = strlen (flag_chars);
	      flag_chars[i++] = fki->length_code_char;
	      flag_chars[i] = 0;
	    }
	  if (pedantic)
	    {
	      /* Warn if the length modifier is non-standard.  */
	      if (ADJ_STD (length_chars_std) > C_STD_VER)
		status_warning (status, "%s does not support the `%s' %s length modifier",
				C_STD_NAME (length_chars_std), length_chars,
				fki->name);
	    }
	}

      /* Read any modifier (strftime E/O).  */
      if (fki->modifier_chars != NULL)
	{
	  while (*format_chars != 0
		 && strchr (fki->modifier_chars, *format_chars) != 0)
	    {
	      if (strchr (flag_chars, *format_chars) != 0)
		{
		  const format_flag_spec *s = get_flag_spec (flag_specs,
							     *format_chars, NULL);
		  status_warning (status, "repeated %s in format", _(s->name));
		}
	      else
		{
		  i = strlen (flag_chars);
		  flag_chars[i++] = *format_chars;
		  flag_chars[i] = 0;
		}
	      ++format_chars;
	    }
	}

      /* Handle the scanf allocation kludge.  */
      if (fki->flags & (int) FMT_FLAG_SCANF_A_KLUDGE)
	{
	  if (*format_chars == 'a' && !flag_isoc99)
	    {
	      if (format_chars[1] == 's' || format_chars[1] == 'S'
		  || format_chars[1] == '[')
		{
		  /* `a' is used as a flag.  */
		  i = strlen (flag_chars);
		  flag_chars[i++] = 'a';
		  flag_chars[i] = 0;
		  format_chars++;
		}
	    }
	}

      format_char = *format_chars;
      if (format_char == 0
	  || (!(fki->flags & (int) FMT_FLAG_FANCY_PERCENT_OK)
	      && format_char == '%'))
	{
	  status_warning (status, "conversion lacks type at end of format");
	  continue;
	}
      format_chars++;
      fci = fki->conversion_specs;
      while (fci->format_chars != 0
	     && strchr (fci->format_chars, format_char) == 0)
	  ++fci;
      if (fci->format_chars == 0)
	{
          if (ISGRAPH(format_char))
	    status_warning (status, "unknown conversion type character `%c' in format",
		     format_char);
	  else
	    status_warning (status, "unknown conversion type character 0x%x in format",
		     format_char);
	  continue;
	}
      if (pedantic)
	{
	  if (ADJ_STD (fci->std) > C_STD_VER)
	    status_warning (status, "%s does not support the `%%%c' %s format",
			    C_STD_NAME (fci->std), format_char, fki->name);
	}

      /* Validate the individual flags used, removing any that are invalid.  */
      {
	int d = 0;
	for (i = 0; flag_chars[i] != 0; i++)
	  {
	    const format_flag_spec *s = get_flag_spec (flag_specs,
						       flag_chars[i], NULL);
	    flag_chars[i - d] = flag_chars[i];
	    if (flag_chars[i] == fki->length_code_char)
	      continue;
	    if (strchr (fci->flag_chars, flag_chars[i]) == 0)
	      {
		status_warning (status, "%s used with `%%%c' %s format",
				_(s->name), format_char, fki->name);
		d++;
		continue;
	      }
	    if (pedantic)
	      {
		const format_flag_spec *t;
		if (ADJ_STD (s->std) > C_STD_VER)
		  status_warning (status, "%s does not support %s",
				  C_STD_NAME (s->std), _(s->long_name));
		t = get_flag_spec (flag_specs, flag_chars[i], fci->flags2);
		if (t != NULL && ADJ_STD (t->std) > ADJ_STD (s->std))
		  {
		    const char *long_name = (t->long_name != NULL
					     ? t->long_name
					     : s->long_name);
		    if (ADJ_STD (t->std) > C_STD_VER)
		      status_warning (status, "%s does not support %s with the `%%%c' %s format",
				      C_STD_NAME (t->std), _(long_name),
				      format_char, fki->name);
		  }
	      }
	  }
	flag_chars[i - d] = 0;
      }

      if ((fki->flags & (int) FMT_FLAG_SCANF_A_KLUDGE)
	  && strchr (flag_chars, 'a') != 0)
	aflag = 1;

      if (fki->suppression_char
	  && strchr (flag_chars, fki->suppression_char) != 0)
	suppressed = 1;

      /* Validate the pairs of flags used.  */
      for (i = 0; bad_flag_pairs[i].flag_char1 != 0; i++)
	{
	  const format_flag_spec *s, *t;
	  if (strchr (flag_chars, bad_flag_pairs[i].flag_char1) == 0)
	    continue;
	  if (strchr (flag_chars, bad_flag_pairs[i].flag_char2) == 0)
	    continue;
	  if (bad_flag_pairs[i].predicate != 0
	      && strchr (fci->flags2, bad_flag_pairs[i].predicate) == 0)
	    continue;
	  s = get_flag_spec (flag_specs, bad_flag_pairs[i].flag_char1, NULL);
	  t = get_flag_spec (flag_specs, bad_flag_pairs[i].flag_char2, NULL);
	  if (bad_flag_pairs[i].ignored)
	    {
	      if (bad_flag_pairs[i].predicate != 0)
		status_warning (status, "%s ignored with %s and `%%%c' %s format",
				_(s->name), _(t->name), format_char,
				fki->name);
	      else
		status_warning (status, "%s ignored with %s in %s format",
				_(s->name), _(t->name), fki->name);
	    }
	  else
	    {
	      if (bad_flag_pairs[i].predicate != 0)
		status_warning (status, "use of %s and %s together with `%%%c' %s format",
				_(s->name), _(t->name), format_char,
				fki->name);
	      else
		status_warning (status, "use of %s and %s together in %s format",
				_(s->name), _(t->name), fki->name);
	    }
	}

      /* Give Y2K warnings.  */
      if (warn_format_y2k)
	{
	  int y2k_level = 0;
	  if (strchr (fci->flags2, '4') != 0)
	    if (strchr (flag_chars, 'E') != 0)
	      y2k_level = 3;
	    else
	      y2k_level = 2;
	  else if (strchr (fci->flags2, '3') != 0)
	    y2k_level = 3;
	  else if (strchr (fci->flags2, '2') != 0)
	    y2k_level = 2;
	  if (y2k_level == 3)
	    status_warning (status, "`%%%c' yields only last 2 digits of year in some locales",
			    format_char);
	  else if (y2k_level == 2)
	    status_warning (status, "`%%%c' yields only last 2 digits of year", format_char);
	}

      if (strchr (fci->flags2, '[') != 0)
	{
	  /* Skip over scan set, in case it happens to have '%' in it.  */
	  if (*format_chars == '^')
	    ++format_chars;
	  /* Find closing bracket; if one is hit immediately, then
	     it's part of the scan set rather than a terminator.  */
	  if (*format_chars == ']')
	    ++format_chars;
	  while (*format_chars && *format_chars != ']')
	    ++format_chars;
	  if (*format_chars != ']')
	    /* The end of the format string was reached.  */
	    status_warning (status, "no closing `]' for `%%[' format");
	}

      wanted_type = 0;
      wanted_type_name = 0;
      if (fki->flags & (int) FMT_FLAG_ARG_CONVERT)
	{
	  wanted_type = (fci->types[length_chars_val].type
			 ? *fci->types[length_chars_val].type : 0);
	  wanted_type_name = fci->types[length_chars_val].name;
	  wanted_type_std = fci->types[length_chars_val].std;
	  if (wanted_type == 0)
	    {
	      status_warning (status, "use of `%s' length modifier with `%c' type character",
			      length_chars, format_char);
	      /* Heuristic: skip one argument when an invalid length/type
		 combination is encountered.  */
	      arg_num++;
	      if (params == 0)
		{
		  status_warning (status, "too few arguments for format");
		  return;
		}
	      params = TREE_CHAIN (params);
	      continue;
	    }
	  else if (pedantic
		   /* Warn if non-standard, provided it is more non-standard
		      than the length and type characters that may already
		      have been warned for.  */
		   && ADJ_STD (wanted_type_std) > ADJ_STD (length_chars_std)
		   && ADJ_STD (wanted_type_std) > ADJ_STD (fci->std))
	    {
	      if (ADJ_STD (wanted_type_std) > C_STD_VER)
		status_warning (status, "%s does not support the `%%%s%c' %s format",
				C_STD_NAME (wanted_type_std), length_chars,
				format_char, fki->name);
	    }
	}

      /* Finally. . .check type of argument against desired type!  */
      if (info->first_arg_num == 0)
	continue;
      if ((fci->pointer_count == 0 && wanted_type == void_type_node)
	  || suppressed)
	{
	  if (main_arg_num != 0)
	    {
	      if (suppressed)
		status_warning (status, "operand number specified with suppressed assignment");
	      else
		status_warning (status, "operand number specified for format taking no argument");
	    }
	}
      else
	{
	  if (main_arg_num != 0)
	    {
	      arg_num = main_arg_num;
	      params = main_arg_params;
	    }
	  else
	    {
	      ++arg_num;
	      if (has_operand_number > 0)
		{
		  status_warning (status, "missing $ operand number in format");
		  return;
		}
	      else
		has_operand_number = 0;
	      if (params == 0)
		{
		  status_warning (status, "too few arguments for format");
		  return;
		}
	    }
	  cur_param = TREE_VALUE (params);
	  params = TREE_CHAIN (params);
	  main_wanted_type.wanted_type = wanted_type;
	  main_wanted_type.wanted_type_name = wanted_type_name;
	  main_wanted_type.pointer_count = fci->pointer_count + aflag;
	  main_wanted_type.char_lenient_flag = 0;
	  if (strchr (fci->flags2, 'c') != 0)
	    main_wanted_type.char_lenient_flag = 1;
	  main_wanted_type.writing_in_flag = 0;
	  main_wanted_type.reading_from_flag = 0;
	  if (aflag)
	    main_wanted_type.writing_in_flag = 1;
	  else
	    {
	      if (strchr (fci->flags2, 'W') != 0)
		main_wanted_type.writing_in_flag = 1;
	      if (strchr (fci->flags2, 'R') != 0)
		main_wanted_type.reading_from_flag = 1;
	    }
	  main_wanted_type.name = NULL;
	  main_wanted_type.param = cur_param;
	  main_wanted_type.arg_num = arg_num;
	  main_wanted_type.next = NULL;
	  if (last_wanted_type != 0)
	    last_wanted_type->next = &main_wanted_type;
	  if (first_wanted_type == 0)
	    first_wanted_type = &main_wanted_type;
	  last_wanted_type = &main_wanted_type;
	}

      if (first_wanted_type != 0)
	check_format_types (status, first_wanted_type);

    }
}


/* Check the argument types from a single format conversion (possibly
   including width and precision arguments).  */
static void
check_format_types (status, types)
     int *status;
     format_wanted_type *types;
{
  for (; types != 0; types = types->next)
    {
      tree cur_param;
      tree cur_type;
      tree orig_cur_type;
      tree wanted_type;
      tree promoted_type;
      int arg_num;
      int i;
      int char_type_flag;
      cur_param = types->param;
      cur_type = TREE_TYPE (cur_param);
      if (cur_type == error_mark_node)
	continue;
      char_type_flag = 0;
      wanted_type = types->wanted_type;
      arg_num = types->arg_num;

      /* The following should not occur here.  */
      if (wanted_type == 0)
	abort ();
      if (wanted_type == void_type_node && types->pointer_count == 0)
	abort ();

      if (types->pointer_count == 0)
	{
	  promoted_type = simple_type_promotes_to (wanted_type);
	  if (promoted_type != NULL_TREE)
	    wanted_type = promoted_type;
	}

      STRIP_NOPS (cur_param);

      /* Check the types of any additional pointer arguments
	 that precede the "real" argument.  */
      for (i = 0; i < types->pointer_count; ++i)
	{
	  if (TREE_CODE (cur_type) == POINTER_TYPE)
	    {
	      cur_type = TREE_TYPE (cur_type);
	      if (cur_type == error_mark_node)
		break;

	      /* Check for writing through a NULL pointer.  */
	      if (types->writing_in_flag
		  && i == 0
		  && cur_param != 0
		  && integer_zerop (cur_param))
		status_warning (status,
				"writing through null pointer (arg %d)",
				arg_num);

	      /* Check for reading through a NULL pointer.  */
	      if (types->reading_from_flag
		  && i == 0
		  && cur_param != 0
		  && integer_zerop (cur_param))
		status_warning (status,
				"reading through null pointer (arg %d)",
				arg_num);

	      if (cur_param != 0 && TREE_CODE (cur_param) == ADDR_EXPR)
		cur_param = TREE_OPERAND (cur_param, 0);
	      else
		cur_param = 0;

	      /* See if this is an attempt to write into a const type with
		 scanf or with printf "%n".  Note: the writing in happens
		 at the first indirection only, if for example
		 void * const * is passed to scanf %p; passing
		 const void ** is simply passing an incompatible type.  */
	      if (types->writing_in_flag
		  && i == 0
		  && (TYPE_READONLY (cur_type)
		      || (cur_param != 0
			  && (TREE_CODE_CLASS (TREE_CODE (cur_param)) == 'c'
			      || (DECL_P (cur_param)
				  && TREE_READONLY (cur_param))))))
		status_warning (status, "writing into constant object (arg %d)", arg_num);

	      /* If there are extra type qualifiers beyond the first
		 indirection, then this makes the types technically
		 incompatible.  */
	      if (i > 0
		  && pedantic
		  && (TYPE_READONLY (cur_type)
		      || TYPE_VOLATILE (cur_type)
		      || TYPE_RESTRICT (cur_type)))
		status_warning (status, "extra type qualifiers in format argument (arg %d)",
			 arg_num);

	    }
	  else
	    {
	      if (types->pointer_count == 1)
		status_warning (status, "format argument is not a pointer (arg %d)", arg_num);
	      else
		status_warning (status, "format argument is not a pointer to a pointer (arg %d)", arg_num);
	      break;
	    }
	}

      if (i < types->pointer_count)
	continue;

      orig_cur_type = cur_type;
      cur_type = TYPE_MAIN_VARIANT (cur_type);

      /* Check whether the argument type is a character type.  This leniency
	 only applies to certain formats, flagged with 'c'.
      */
      if (types->char_lenient_flag)
	char_type_flag = (cur_type == char_type_node
			  || cur_type == signed_char_type_node
			  || cur_type == unsigned_char_type_node);

      /* Check the type of the "real" argument, if there's a type we want.  */
      if (wanted_type == cur_type)
	continue;
      /* If we want `void *', allow any pointer type.
	 (Anything else would already have got a warning.)
	 With -pedantic, only allow pointers to void and to character
	 types.  */
      if (wanted_type == void_type_node
	  && (!pedantic || (i == 1 && char_type_flag)))
	continue;
      /* Don't warn about differences merely in signedness, unless
	 -pedantic.  With -pedantic, warn if the type is a pointer
	 target and not a character type, and for character types at
	 a second level of indirection.  */
      if (TREE_CODE (wanted_type) == INTEGER_TYPE
	  && TREE_CODE (cur_type) == INTEGER_TYPE
	  && (! pedantic || i == 0 || (i == 1 && char_type_flag))
	  && (TREE_UNSIGNED (wanted_type)
	      ? wanted_type == unsigned_type (cur_type)
	      : wanted_type == signed_type (cur_type)))
	continue;
      /* Likewise, "signed char", "unsigned char" and "char" are
	 equivalent but the above test won't consider them equivalent.  */
      if (wanted_type == char_type_node
	  && (! pedantic || i < 2)
	  && char_type_flag)
	continue;
      /* Now we have a type mismatch.  */
      {
	const char *this;
	const char *that;

	this = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (wanted_type)));
	that = 0;
	if (TYPE_NAME (orig_cur_type) != 0
	    && TREE_CODE (orig_cur_type) != INTEGER_TYPE
	    && !(TREE_CODE (orig_cur_type) == POINTER_TYPE
		 && TREE_CODE (TREE_TYPE (orig_cur_type)) == INTEGER_TYPE))
	  {
	    if (TREE_CODE (TYPE_NAME (orig_cur_type)) == TYPE_DECL
		&& DECL_NAME (TYPE_NAME (orig_cur_type)) != 0)
	      that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (orig_cur_type)));
	    else
	      that = IDENTIFIER_POINTER (TYPE_NAME (orig_cur_type));
	  }

	/* A nameless type can't possibly match what the format wants.
	   So there will be a warning for it.
	   Make up a string to describe vaguely what it is.  */
	if (that == 0)
	  {
	    if (TREE_CODE (orig_cur_type) == POINTER_TYPE)
	      that = _("pointer");
	    else
	      that = _("different type");
	  }

	/* Make the warning better in case of mismatch of int vs long.  */
	if (TREE_CODE (orig_cur_type) == INTEGER_TYPE
	    && TREE_CODE (wanted_type) == INTEGER_TYPE
	    && TYPE_PRECISION (orig_cur_type) == TYPE_PRECISION (wanted_type)
	    && TYPE_NAME (orig_cur_type) != 0
	    && TREE_CODE (TYPE_NAME (orig_cur_type)) == TYPE_DECL)
	  that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (orig_cur_type)));

	if (strcmp (this, that) != 0)
	  {
	    /* There may be a better name for the format, e.g. size_t,
	       but we should allow for programs with a perverse typedef
	       making size_t something other than what the compiler
	       thinks.  */
	    if (types->wanted_type_name != 0
		&& strcmp (types->wanted_type_name, that) != 0)
	      this = types->wanted_type_name;
	    if (types->name != 0)
	      status_warning (status, "%s is not type %s (arg %d)", types->name, this,
		       arg_num);
	    else
	      status_warning (status, "%s format, %s arg (arg %d)", this, that, arg_num);
	  }
      }
    }
}
