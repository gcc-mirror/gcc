/* Check calls to formatted I/O functions (-Wformat).
   Copyright (C) 1992-2015 Free Software Foundation, Inc.

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
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "stringpool.h"
#include "flags.h"
#include "c-common.h"
#include "c-objc.h"
#include "intl.h"
#include "diagnostic-core.h"
#include "langhooks.h"
#include "c-format.h"
#include "alloc-pool.h"
#include "c-target.h"

/* Handle attributes associated with format checking.  */

/* This must be in the same order as format_types, except for
   format_type_error.  Target-specific format types do not have
   matching enum values.  */
enum format_type { printf_format_type, asm_fprintf_format_type,
		   gcc_diag_format_type, gcc_tdiag_format_type,
		   gcc_cdiag_format_type,
		   gcc_cxxdiag_format_type, gcc_gfc_format_type,
		   gcc_objc_string_format_type,
		   format_type_error = -1};

typedef struct function_format_info
{
  int format_type;			/* type of format (printf, scanf, etc.) */
  unsigned HOST_WIDE_INT format_num;	/* number of format argument */
  unsigned HOST_WIDE_INT first_arg_num;	/* number of first arg (zero for varargs) */
} function_format_info;

static bool decode_format_attr (tree, function_format_info *, int);
static int decode_format_type (const char *);

static bool check_format_string (tree argument,
				 unsigned HOST_WIDE_INT format_num,
				 int flags, bool *no_add_attrs,
				 int expected_format_type);
static bool get_constant (tree expr, unsigned HOST_WIDE_INT *value,
			  int validated_p);
static const char *convert_format_name_to_system_name (const char *attr_name);
static bool cmp_attribs (const char *tattr_name, const char *attr_name);

static int first_target_format_type;
static const char *format_name (int format_num);
static int format_flags (int format_num);

/* Given a string S of length LINE_WIDTH, find the visual column
   corresponding to OFFSET bytes.   */

static unsigned int
location_column_from_byte_offset (const char *s, int line_width,
				  unsigned int offset)
{
  const char * c = s;
  if (*c != '"')
    return 0;

  c++, offset--;
  while (offset > 0)
    {
      if (c - s >= line_width)
	return 0;

      switch (*c)
	{
	case '\\':
	  c++;
	  if (c - s >= line_width)
	    return 0;
	  switch (*c)
	    {
	    case '\\': case '\'': case '"': case '?':
	    case '(': case '{': case '[': case '%':
	    case 'a': case 'b': case 'f': case 'n':
	    case 'r': case 't': case 'v': 
	    case 'e': case 'E':
	      c++, offset--;
	      break;

	    default:
	      return 0;
	    }
	  break;

	case '"':
	  /* We found the end of the string too early.  */
	  return 0;
	  
	default:
	  c++, offset--;
	  break;
	}
    }
  return c - s;
}

/* Return a location that encodes the same location as LOC but shifted
   by OFFSET bytes.  */

static location_t
location_from_offset (location_t loc, int offset)
{
  gcc_checking_assert (offset >= 0);
  if (linemap_location_from_macro_expansion_p (line_table, loc)
      || offset < 0)
    return loc;

  expanded_location s = expand_location_to_spelling_point (loc);
  int line_width;
  const char *line = location_get_source_line (s, &line_width);
  if (line == NULL)
    return loc;
  line += s.column - 1 ;
  line_width -= s.column - 1;
  unsigned int column =
    location_column_from_byte_offset (line, line_width, (unsigned) offset);

  return linemap_position_for_loc_and_offset (line_table, loc, column);
}

/* Check that we have a pointer to a string suitable for use as a format.
   The default is to check for a char type.
   For objective-c dialects, this is extended to include references to string
   objects validated by objc_string_ref_type_p ().  
   Targets may also provide a string object type that can be used within c and 
   c++ and shared with their respective objective-c dialects. In this case the
   reference to a format string is checked for validity via a hook.
   
   The function returns true if strref points to any string type valid for the 
   language dialect and target.  */

static bool
valid_stringptr_type_p (tree strref)
{
  return (strref != NULL
	  && TREE_CODE (strref) == POINTER_TYPE
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (strref)) == char_type_node
	      || objc_string_ref_type_p (strref)
	      || (*targetcm.string_object_ref_type_p) ((const_tree) strref)));
}

/* Handle a "format_arg" attribute; arguments as in
   struct attribute_spec.handler.  */
tree
handle_format_arg_attribute (tree *node, tree ARG_UNUSED (name),
			     tree args, int flags, bool *no_add_attrs)
{
  tree type = *node;
  tree format_num_expr = TREE_VALUE (args);
  unsigned HOST_WIDE_INT format_num = 0;

  if (!get_constant (format_num_expr, &format_num, 0))
    {
      error ("format string has invalid operand number");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (prototype_p (type))
    {
      /* The format arg can be any string reference valid for the language and
	target.  We cannot be more specific in this case.  */
      if (!check_format_string (type, format_num, flags, no_add_attrs, -1))
	return NULL_TREE;
    }

  if (!valid_stringptr_type_p (TREE_TYPE (type)))
    {
      if (!(flags & (int) ATTR_FLAG_BUILT_IN))
	error ("function does not return string type");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  return NULL_TREE;
}

/* Verify that the format_num argument is actually a string reference suitable,
   for the language dialect and target (in case the format attribute is in 
   error).  When we know the specific reference type expected, this is also 
   checked.  */
static bool
check_format_string (tree fntype, unsigned HOST_WIDE_INT format_num,
		     int flags, bool *no_add_attrs, int expected_format_type)
{
  unsigned HOST_WIDE_INT i;
  bool is_objc_sref, is_target_sref, is_char_ref;
  tree ref;
  int fmt_flags;
  function_args_iterator iter;

  i = 1;
  FOREACH_FUNCTION_ARGS (fntype, ref, iter)
    {
      if (i == format_num)
	break;
      i++;
    }

  if (!ref
      || !valid_stringptr_type_p (ref))
    {
      if (!(flags & (int) ATTR_FLAG_BUILT_IN))
	error ("format string argument is not a string type");
      *no_add_attrs = true;
      return false;
    }

  /* We only know that we want a suitable string reference.  */
  if (expected_format_type < 0)
    return true;

  /* Now check that the arg matches the expected type.  */
  is_char_ref = 
    (TYPE_MAIN_VARIANT (TREE_TYPE (ref)) == char_type_node);

  fmt_flags = format_flags (expected_format_type);
  is_objc_sref = is_target_sref = false;
  if (!is_char_ref)
    is_objc_sref = objc_string_ref_type_p (ref);

  if (!(fmt_flags & FMT_FLAG_PARSE_ARG_CONVERT_EXTERNAL))
    {
      if (is_char_ref)
	return true; /* OK, we expected a char and found one.  */
      else
	{
	  /* We expected a char but found an extended string type.  */
	  if (is_objc_sref)
	    error ("found a %<%s%> reference but the format argument should"
		   " be a string", format_name (gcc_objc_string_format_type));
	  else
	    error ("found a %qT but the format argument should be a string",
		   ref);
	  *no_add_attrs = true;
	  return false;
	}
    }

  /* We expect a string object type as the format arg.  */
  if (is_char_ref)
    {
      error ("format argument should be a %<%s%> reference but"
	     " a string was found", format_name (expected_format_type));
      *no_add_attrs = true;
      return false;
    }
  
  /* We will assert that objective-c will support either its own string type
     or the target-supplied variant.  */
  if (!is_objc_sref)
    is_target_sref = (*targetcm.string_object_ref_type_p) ((const_tree) ref);

  if (expected_format_type == (int) gcc_objc_string_format_type 
      && (is_objc_sref || is_target_sref))
    return true;

  /* We will allow a target string ref to match only itself.  */
  if (first_target_format_type 
      && expected_format_type >= first_target_format_type
      && is_target_sref)
    return true;
  else
    {
      error ("format argument should be a %<%s%> reference", 
	      format_name (expected_format_type));
      *no_add_attrs = true;
      return false;
    }

  gcc_unreachable ();
}

/* Verify EXPR is a constant, and store its value.
   If validated_p is true there should be no errors.
   Returns true on success, false otherwise.  */
static bool
get_constant (tree expr, unsigned HOST_WIDE_INT *value, int validated_p)
{
  if (!tree_fits_uhwi_p (expr))
    {
      gcc_assert (!validated_p);
      return false;
    }

  *value = TREE_INT_CST_LOW (expr);

  return true;
}

/* Decode the arguments to a "format" attribute into a
   function_format_info structure.  It is already known that the list
   is of the right length.  If VALIDATED_P is true, then these
   attributes have already been validated and must not be erroneous;
   if false, it will give an error message.  Returns true if the
   attributes are successfully decoded, false otherwise.  */

static bool
decode_format_attr (tree args, function_format_info *info, int validated_p)
{
  tree format_type_id = TREE_VALUE (args);
  tree format_num_expr = TREE_VALUE (TREE_CHAIN (args));
  tree first_arg_num_expr
    = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));

  if (TREE_CODE (format_type_id) != IDENTIFIER_NODE)
    {
      gcc_assert (!validated_p);
      error ("unrecognized format specifier");
      return false;
    }
  else
    {
      const char *p = IDENTIFIER_POINTER (format_type_id);

      p = convert_format_name_to_system_name (p);

      info->format_type = decode_format_type (p);
      
      if (!c_dialect_objc ()
	   && info->format_type == gcc_objc_string_format_type)
	{
	  gcc_assert (!validated_p);
	  warning (OPT_Wformat_, "%qE is only allowed in Objective-C dialects",
		   format_type_id);
	  info->format_type = format_type_error;
	  return false;
	}

      if (info->format_type == format_type_error)
	{
	  gcc_assert (!validated_p);
	  warning (OPT_Wformat_, "%qE is an unrecognized format function type",
		   format_type_id);
	  return false;
	}
    }

  if (!get_constant (format_num_expr, &info->format_num, validated_p))
    {
      error ("format string has invalid operand number");
      return false;
    }

  if (!get_constant (first_arg_num_expr, &info->first_arg_num, validated_p))
    {
      error ("%<...%> has invalid operand number");
      return false;
    }

  if (info->first_arg_num != 0 && info->first_arg_num <= info->format_num)
    {
      gcc_assert (!validated_p);
      error ("format string argument follows the args to be formatted");
      return false;
    }

  return true;
}

/* Check a call to a format function against a parameter list.  */

/* The C standard version C++ is treated as equivalent to
   or inheriting from, for the purpose of format features supported.  */
#define CPLUSPLUS_STD_VER	(cxx_dialect < cxx11 ? STD_C94 : STD_C99)
/* The C standard version we are checking formats against when pedantic.  */
#define C_STD_VER		((int) (c_dialect_cxx ()		   \
				 ? CPLUSPLUS_STD_VER			   \
				 : (flag_isoc99				   \
				    ? STD_C99				   \
				    : (flag_isoc94 ? STD_C94 : STD_C89))))
/* The name to give to the standard version we are warning about when
   pedantic.  FEATURE_VER is the version in which the feature warned out
   appeared, which is higher than C_STD_VER.  */
#define C_STD_NAME(FEATURE_VER) (c_dialect_cxx ()		\
				 ? (cxx_dialect < cxx11 ? "ISO C++98" \
				    : "ISO C++11")		\
				 : ((FEATURE_VER) == STD_EXT	\
				    ? "ISO C"			\
				    : "ISO C90"))
/* Adjust a C standard version, which may be STD_C9L, to account for
   -Wno-long-long.  Returns other standard versions unchanged.  */
#define ADJ_STD(VER)		((int) ((VER) == STD_C9L		      \
				       ? (warn_long_long ? STD_C99 : STD_C89) \
				       : (VER)))

/* Enum describing the kind of specifiers present in the format and
   requiring an argument.  */
enum format_specifier_kind {
  CF_KIND_FORMAT,
  CF_KIND_FIELD_WIDTH,
  CF_KIND_FIELD_PRECISION
};

static const char *kind_descriptions[] = {
  N_("format"),
  N_("field width specifier"),
  N_("field precision specifier")
};

/* Structure describing details of a type expected in format checking,
   and the type to check against it.  */
typedef struct format_wanted_type
{
  /* The type wanted.  */
  tree wanted_type;
  /* The name of this type to use in diagnostics.  */
  const char *wanted_type_name;
  /* Should be type checked just for scalar width identity.  */
  int scalar_identity_flag;
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
  /* The kind of specifier that this type is used for.  */
  enum format_specifier_kind kind;
  /* The starting character of the specifier.  This never includes the
     initial percent sign.  */
  const char *format_start;
  /* The length of the specifier.  */
  int format_length;
  /* The actual parameter to check against the wanted type.  */
  tree param;
  /* The argument number of that parameter.  */
  int arg_num;
  /* The offset location of this argument with respect to the format
     string location.  */
  unsigned int offset_loc;
  /* The next type to check for this format conversion, or NULL if none.  */
  struct format_wanted_type *next;
} format_wanted_type;

/* Convenience macro for format_length_info meaning unused.  */
#define NO_FMT NULL, FMT_LEN_none, STD_C89

static const format_length_info printf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, "hh", FMT_LEN_hh, STD_C99, 0 },
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C9L, 0 },
  { "q", FMT_LEN_ll, STD_EXT, NO_FMT, 0 },
  { "L", FMT_LEN_L, STD_C89, NO_FMT, 0 },
  { "z", FMT_LEN_z, STD_C99, NO_FMT, 0 },
  { "Z", FMT_LEN_z, STD_EXT, NO_FMT, 0 },
  { "t", FMT_LEN_t, STD_C99, NO_FMT, 0 },
  { "j", FMT_LEN_j, STD_C99, NO_FMT, 0 },
  { "H", FMT_LEN_H, STD_EXT, NO_FMT, 0 },
  { "D", FMT_LEN_D, STD_EXT, "DD", FMT_LEN_DD, STD_EXT, 0 },
  { NO_FMT, NO_FMT, 0 }
};

/* Length specifiers valid for asm_fprintf.  */
static const format_length_info asm_fprintf_length_specs[] =
{
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C89, 0 },
  { "w", FMT_LEN_none, STD_C89, NO_FMT, 0 },
  { NO_FMT, NO_FMT, 0 }
};

/* Length specifiers valid for GCC diagnostics.  */
static const format_length_info gcc_diag_length_specs[] =
{
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C89, 0 },
  { "w", FMT_LEN_none, STD_C89, NO_FMT, 0 },
  { NO_FMT, NO_FMT, 0 }
};

/* The custom diagnostics all accept the same length specifiers.  */
#define gcc_tdiag_length_specs gcc_diag_length_specs
#define gcc_cdiag_length_specs gcc_diag_length_specs
#define gcc_cxxdiag_length_specs gcc_diag_length_specs

/* This differs from printf_length_specs only in that "Z" is not accepted.  */
static const format_length_info scanf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, "hh", FMT_LEN_hh, STD_C99, 0 },
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C9L, 0 },
  { "q", FMT_LEN_ll, STD_EXT, NO_FMT, 0 },
  { "L", FMT_LEN_L, STD_C89, NO_FMT, 0 },
  { "z", FMT_LEN_z, STD_C99, NO_FMT, 0 },
  { "t", FMT_LEN_t, STD_C99, NO_FMT, 0 },
  { "j", FMT_LEN_j, STD_C99, NO_FMT, 0 },
  { "H", FMT_LEN_H, STD_EXT, NO_FMT, 0 },
  { "D", FMT_LEN_D, STD_EXT, "DD", FMT_LEN_DD, STD_EXT, 0 },
  { NO_FMT, NO_FMT, 0 }
};


/* All tables for strfmon use STD_C89 everywhere, since -pedantic warnings
   make no sense for a format type not part of any C standard version.  */
static const format_length_info strfmon_length_specs[] =
{
  /* A GNU extension.  */
  { "L", FMT_LEN_L, STD_C89, NO_FMT, 0 },
  { NO_FMT, NO_FMT, 0 }
};


/* For now, the Fortran front-end routines only use l as length modifier.  */
static const format_length_info gcc_gfc_length_specs[] =
{
  { "l", FMT_LEN_l, STD_C89, NO_FMT, 0 },
  { NO_FMT, NO_FMT, 0 }
};


static const format_flag_spec printf_flag_specs[] =
{
  { ' ',  0, 0, N_("' ' flag"),        N_("the ' ' printf flag"),              STD_C89 },
  { '+',  0, 0, N_("'+' flag"),        N_("the '+' printf flag"),              STD_C89 },
  { '#',  0, 0, N_("'#' flag"),        N_("the '#' printf flag"),              STD_C89 },
  { '0',  0, 0, N_("'0' flag"),        N_("the '0' printf flag"),              STD_C89 },
  { '-',  0, 0, N_("'-' flag"),        N_("the '-' printf flag"),              STD_C89 },
  { '\'', 0, 0, N_("''' flag"),        N_("the ''' printf flag"),              STD_EXT },
  { 'I',  0, 0, N_("'I' flag"),        N_("the 'I' printf flag"),              STD_EXT },
  { 'w',  0, 0, N_("field width"),     N_("field width in printf format"),     STD_C89 },
  { 'p',  0, 0, N_("precision"),       N_("precision in printf format"),       STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, STD_C89 }
};


static const format_flag_pair printf_flag_pairs[] =
{
  { ' ', '+', 1, 0   },
  { '0', '-', 1, 0   },
  { '0', 'p', 1, 'i' },
  { 0, 0, 0, 0 }
};

static const format_flag_spec asm_fprintf_flag_specs[] =
{
  { ' ',  0, 0, N_("' ' flag"),        N_("the ' ' printf flag"),              STD_C89 },
  { '+',  0, 0, N_("'+' flag"),        N_("the '+' printf flag"),              STD_C89 },
  { '#',  0, 0, N_("'#' flag"),        N_("the '#' printf flag"),              STD_C89 },
  { '0',  0, 0, N_("'0' flag"),        N_("the '0' printf flag"),              STD_C89 },
  { '-',  0, 0, N_("'-' flag"),        N_("the '-' printf flag"),              STD_C89 },
  { 'w',  0, 0, N_("field width"),     N_("field width in printf format"),     STD_C89 },
  { 'p',  0, 0, N_("precision"),       N_("precision in printf format"),       STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, STD_C89 }
};

static const format_flag_pair asm_fprintf_flag_pairs[] =
{
  { ' ', '+', 1, 0   },
  { '0', '-', 1, 0   },
  { '0', 'p', 1, 'i' },
  { 0, 0, 0, 0 }
};

static const format_flag_pair gcc_diag_flag_pairs[] =
{
  { 0, 0, 0, 0 }
};

#define gcc_tdiag_flag_pairs gcc_diag_flag_pairs
#define gcc_cdiag_flag_pairs gcc_diag_flag_pairs
#define gcc_cxxdiag_flag_pairs gcc_diag_flag_pairs
#define gcc_gfc_flag_pairs gcc_diag_flag_pairs

static const format_flag_spec gcc_diag_flag_specs[] =
{
  { '+',  0, 0, N_("'+' flag"),        N_("the '+' printf flag"),              STD_C89 },
  { '#',  0, 0, N_("'#' flag"),        N_("the '#' printf flag"),              STD_C89 },
  { 'q',  0, 0, N_("'q' flag"),        N_("the 'q' diagnostic flag"),          STD_C89 },
  { 'p',  0, 0, N_("precision"),       N_("precision in printf format"),       STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, STD_C89 }
};

#define gcc_tdiag_flag_specs gcc_diag_flag_specs
#define gcc_cdiag_flag_specs gcc_diag_flag_specs
#define gcc_cxxdiag_flag_specs gcc_diag_flag_specs
#define gcc_gfc_flag_specs gcc_diag_flag_specs

static const format_flag_spec scanf_flag_specs[] =
{
  { '*',  0, 0, N_("assignment suppression"), N_("the assignment suppression scanf feature"), STD_C89 },
  { 'a',  0, 0, N_("'a' flag"),               N_("the 'a' scanf flag"),                       STD_EXT },
  { 'm',  0, 0, N_("'m' flag"),               N_("the 'm' scanf flag"),                       STD_EXT },
  { 'w',  0, 0, N_("field width"),            N_("field width in scanf format"),              STD_C89 },
  { 'L',  0, 0, N_("length modifier"),        N_("length modifier in scanf format"),          STD_C89 },
  { '\'', 0, 0, N_("''' flag"),               N_("the ''' scanf flag"),                       STD_EXT },
  { 'I',  0, 0, N_("'I' flag"),               N_("the 'I' scanf flag"),                       STD_EXT },
  { 0, 0, 0, NULL, NULL, STD_C89 }
};


static const format_flag_pair scanf_flag_pairs[] =
{
  { '*', 'L', 0, 0 },
  { 'a', 'm', 0, 0 },
  { 0, 0, 0, 0 }
};


static const format_flag_spec strftime_flag_specs[] =
{
  { '_', 0,   0, N_("'_' flag"),     N_("the '_' strftime flag"),          STD_EXT },
  { '-', 0,   0, N_("'-' flag"),     N_("the '-' strftime flag"),          STD_EXT },
  { '0', 0,   0, N_("'0' flag"),     N_("the '0' strftime flag"),          STD_EXT },
  { '^', 0,   0, N_("'^' flag"),     N_("the '^' strftime flag"),          STD_EXT },
  { '#', 0,   0, N_("'#' flag"),     N_("the '#' strftime flag"),          STD_EXT },
  { 'w', 0,   0, N_("field width"),  N_("field width in strftime format"), STD_EXT },
  { 'E', 0,   0, N_("'E' modifier"), N_("the 'E' strftime modifier"),      STD_C99 },
  { 'O', 0,   0, N_("'O' modifier"), N_("the 'O' strftime modifier"),      STD_C99 },
  { 'O', 'o', 0, NULL,               N_("the 'O' modifier"),               STD_EXT },
  { 0, 0, 0, NULL, NULL, STD_C89 }
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
  { '^',  0, 0, N_("'^' flag"),        N_("the '^' strfmon flag"),              STD_C89 },
  { '+',  0, 0, N_("'+' flag"),        N_("the '+' strfmon flag"),              STD_C89 },
  { '(',  0, 0, N_("'(' flag"),        N_("the '(' strfmon flag"),              STD_C89 },
  { '!',  0, 0, N_("'!' flag"),        N_("the '!' strfmon flag"),              STD_C89 },
  { '-',  0, 0, N_("'-' flag"),        N_("the '-' strfmon flag"),              STD_C89 },
  { 'w',  0, 0, N_("field width"),     N_("field width in strfmon format"),     STD_C89 },
  { '#',  0, 0, N_("left precision"),  N_("left precision in strfmon format"),  STD_C89 },
  { 'p',  0, 0, N_("right precision"), N_("right precision in strfmon format"), STD_C89 },
  { 'L',  0, 0, N_("length modifier"), N_("length modifier in strfmon format"), STD_C89 },
  { 0, 0, 0, NULL, NULL, STD_C89 }
};

static const format_flag_pair strfmon_flag_pairs[] =
{
  { '+', '(', 0, 0 },
  { 0, 0, 0, 0 }
};


static const format_char_info print_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  TEX_LL,  T99_SST, T99_PD,  T99_IM,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +'I",  "i",  NULL },
  { "oxX", 0, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM, BADLEN,  BADLEN,  BADLEN }, "-wp0#",     "i",  NULL },
  { "u",   0, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM, BADLEN,  BADLEN,  BADLEN }, "-wp0'I",    "i",  NULL },
  { "fgG", 0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN,  TEX_D32, TEX_D64, TEX_D128 }, "-wp0 +#'I", "",   NULL },
  { "eE",  0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN,  TEX_D32, TEX_D64, TEX_D128 }, "-wp0 +#I",  "",   NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T94_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-w",        "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-wp",       "cR", NULL },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-w",        "c",  NULL },
  { "n",   1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  BADLEN,  T99_SST, T99_PD,  T99_IM,  BADLEN,  BADLEN,  BADLEN }, "",          "W",  NULL },
  /* C99 conversion specifiers.  */
  { "F",   0, STD_C99, { T99_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN,  TEX_D32, TEX_D64, TEX_D128 }, "-wp0 +#'I", "",   NULL },
  { "aA",  0, STD_C99, { T99_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-wp0 +#",   "",   NULL },
  /* X/Open conversion specifiers.  */
  { "C",   0, STD_EXT, { TEX_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-w",        "",   NULL },
  { "S",   1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-wp",       "R",  NULL },
  /* GNU conversion specifiers.  */
  { "m",   0, STD_EXT, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "-wp",       "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info asm_fprintf_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   T9L_LL,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +",  "i", NULL },
  { "oxX", 0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp0#",   "i", NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp0",    "i", NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       "", NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",    "cR", NULL },

  /* asm_fprintf conversion specifiers.  */
  { "O",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "R",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "I",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "L",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "U",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "r",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",  "", NULL },
  { "@",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info gcc_diag_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   T9L_LL,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "ox",  0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "pq", "cR", NULL },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "c",  NULL },

  /* Custom conversion specifiers.  */

  /* These will require a "tree" at runtime.  */
  { "K",   0, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",    "",   NULL },

  { "r",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",    "cR",   NULL },
  { "<>'R",0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "m",   0, STD_C89, NOARGUMENTS, "q",     "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info gcc_tdiag_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   T9L_LL,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "ox",  0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "pq", "cR", NULL },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "c",  NULL },

  /* Custom conversion specifiers.  */

  /* These will require a "tree" at runtime.  */
  { "DFKTEV", 0, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q+", "",   NULL },

  { "v",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q#",  "",   NULL },

  { "r",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",    "cR",   NULL },
  { "<>'R",0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "m",   0, STD_C89, NOARGUMENTS, "q",     "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info gcc_cdiag_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   T9L_LL,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "ox",  0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "pq", "cR", NULL },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "c",  NULL },

  /* Custom conversion specifiers.  */

  /* These will require a "tree" at runtime.  */
  { "DEFKTV", 0, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q+", "",   NULL },

  { "v",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q#",  "",   NULL },

  { "r",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",    "cR",   NULL },
  { "<>'R",0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "m",   0, STD_C89, NOARGUMENTS, "q",     "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info gcc_cxxdiag_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   T9L_LL,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "ox",  0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  T9L_ULL, BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "pq", "cR", NULL },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "c",  NULL },

  /* Custom conversion specifiers.  */

  /* These will require a "tree" at runtime.  */
  { "ADEFKSTVX",0,STD_C89,{ T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q+#",   "",   NULL },

  { "v", 0,STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q#",  "",   NULL },

  /* These accept either an 'int' or an 'enum tree_code' (which is handled as an 'int'.)  */
  { "CLOPQ",0,STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q",  "",   NULL },

  { "r",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "",    "cR",   NULL },
  { "<>'R",0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { "m",   0, STD_C89, NOARGUMENTS, "q",     "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info gcc_gfc_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T89_L,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "", "", NULL },
  { "u",   0, STD_C89, { T89_UI,  BADLEN,  BADLEN,  T89_UL,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "", "", NULL },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "", "", NULL },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "q", "cR", NULL },

  /* gfc conversion specifiers.  */

  { "C",   0, STD_C89, NOARGUMENTS, "",      "",   NULL },

  /* This will require a "locus" at runtime.  */
  { "L",   0, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "", "R", NULL },

  /* These will require nothing.  */
  { "<>",0, STD_C89, NOARGUMENTS, "",      "",   NULL },
  { NULL,  0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info scan_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",    1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  TEX_LL,  T99_SST, T99_PD,  T99_IM,  BADLEN,  BADLEN,  BADLEN }, "*w'I", "W",   NULL },
  { "u",     1, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM, BADLEN,  BADLEN,  BADLEN }, "*w'I", "W",   NULL },
  { "oxX",   1, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T9L_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM, BADLEN,  BADLEN,  BADLEN }, "*w",   "W",   NULL },
  { "efgEG", 1, STD_C89, { T89_F,   BADLEN,  BADLEN,  T89_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN,  TEX_D32, TEX_D64, TEX_D128 }, "*w'",  "W",   NULL },
  { "c",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*mw",   "cW",  NULL },
  { "s",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*amw",  "cW",  NULL },
  { "[",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*amw",  "cW[", NULL },
  { "p",     2, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*w",   "W",   NULL },
  { "n",     1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T9L_LL,  BADLEN,  T99_SST, T99_PD,  T99_IM,  BADLEN,  BADLEN,  BADLEN }, "",     "W",   NULL },
  /* C99 conversion specifiers.  */
  { "F",   1, STD_C99, { T99_F,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN,  TEX_D32, TEX_D64, TEX_D128 }, "*w'",  "W",   NULL },
  { "aA",   1, STD_C99, { T99_F,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*w'",  "W",   NULL },
  /* X/Open conversion specifiers.  */
  { "C",     1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*mw",   "W",   NULL },
  { "S",     1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN }, "*amw",  "W",   NULL },
  { NULL, 0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info time_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "ABZab",		0, STD_C89, NOLENGTHS, "^#",     "",   NULL },
  { "cx",		0, STD_C89, NOLENGTHS, "E",      "3",  NULL },
  { "HIMSUWdmw",	0, STD_C89, NOLENGTHS, "-_0Ow",  "",   NULL },
  { "j",		0, STD_C89, NOLENGTHS, "-_0Ow",  "o",  NULL },
  { "p",		0, STD_C89, NOLENGTHS, "#",      "",   NULL },
  { "X",		0, STD_C89, NOLENGTHS, "E",      "",   NULL },
  { "y",		0, STD_C89, NOLENGTHS, "EO-_0w", "4",  NULL },
  { "Y",		0, STD_C89, NOLENGTHS, "-_0EOw", "o",  NULL },
  { "%",		0, STD_C89, NOLENGTHS, "",       "",   NULL },
  /* C99 conversion specifiers.  */
  { "C",		0, STD_C99, NOLENGTHS, "-_0EOw", "o",  NULL },
  { "D",		0, STD_C99, NOLENGTHS, "",       "2",  NULL },
  { "eVu",		0, STD_C99, NOLENGTHS, "-_0Ow",  "",   NULL },
  { "FRTnrt",		0, STD_C99, NOLENGTHS, "",       "",   NULL },
  { "g",		0, STD_C99, NOLENGTHS, "O-_0w",  "2o", NULL },
  { "G",		0, STD_C99, NOLENGTHS, "-_0Ow",  "o",  NULL },
  { "h",		0, STD_C99, NOLENGTHS, "^#",     "",   NULL },
  { "z",		0, STD_C99, NOLENGTHS, "O",      "o",  NULL },
  /* GNU conversion specifiers.  */
  { "kls",		0, STD_EXT, NOLENGTHS, "-_0Ow",  "",   NULL },
  { "P",		0, STD_EXT, NOLENGTHS, "",       "",   NULL },
  { NULL,		0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

static const format_char_info monetary_char_table[] =
{
  { "in", 0, STD_C89, { T89_D, BADLEN, BADLEN, BADLEN, BADLEN, T89_LD, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN }, "=^+(!-w#p", "", NULL },
  { NULL, 0, STD_C89, NOLENGTHS, NULL, NULL, NULL }
};

/* This must be in the same order as enum format_type.  */
static const format_kind_info format_types_orig[] =
{
  { "gnu_printf",   printf_length_specs,  print_char_table, " +#0-'I", NULL,
    printf_flag_specs, printf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_DOLLAR_MULTIPLE|FMT_FLAG_USE_DOLLAR|FMT_FLAG_EMPTY_PREC_OK,
    'w', 0, 'p', 0, 'L', 0,
    &integer_type_node, &integer_type_node
  },
  { "asm_fprintf",   asm_fprintf_length_specs,  asm_fprintf_char_table, " +#0-", NULL,
    asm_fprintf_flag_specs, asm_fprintf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_EMPTY_PREC_OK,
    'w', 0, 'p', 0, 'L', 0,
    NULL, NULL
  },
  { "gcc_diag",   gcc_diag_length_specs,  gcc_diag_char_table, "q+#", NULL,
    gcc_diag_flag_specs, gcc_diag_flag_pairs,
    FMT_FLAG_ARG_CONVERT,
    0, 0, 'p', 0, 'L', 0,
    NULL, &integer_type_node
  },
  { "gcc_tdiag",   gcc_tdiag_length_specs,  gcc_tdiag_char_table, "q+#", NULL,
    gcc_tdiag_flag_specs, gcc_tdiag_flag_pairs,
    FMT_FLAG_ARG_CONVERT,
    0, 0, 'p', 0, 'L', 0,
    NULL, &integer_type_node
  },
  { "gcc_cdiag",   gcc_cdiag_length_specs,  gcc_cdiag_char_table, "q+#", NULL,
    gcc_cdiag_flag_specs, gcc_cdiag_flag_pairs,
    FMT_FLAG_ARG_CONVERT,
    0, 0, 'p', 0, 'L', 0,
    NULL, &integer_type_node
  },
  { "gcc_cxxdiag",   gcc_cxxdiag_length_specs,  gcc_cxxdiag_char_table, "q+#", NULL,
    gcc_cxxdiag_flag_specs, gcc_cxxdiag_flag_pairs,
    FMT_FLAG_ARG_CONVERT,
    0, 0, 'p', 0, 'L', 0,
    NULL, &integer_type_node
  },
  { "gcc_gfc", gcc_gfc_length_specs, gcc_gfc_char_table, "q+#", NULL,
    gcc_gfc_flag_specs, gcc_gfc_flag_pairs,
    FMT_FLAG_ARG_CONVERT,
    0, 0, 0, 0, 0, 0,
    NULL, NULL
  },
  { "NSString",   NULL,  NULL, NULL, NULL,
    NULL, NULL,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_PARSE_ARG_CONVERT_EXTERNAL, 0, 0, 0, 0, 0, 0,
    NULL, NULL
  },
  { "gnu_scanf",    scanf_length_specs,   scan_char_table,  "*'I", NULL,
    scanf_flag_specs, scanf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_SCANF_A_KLUDGE|FMT_FLAG_USE_DOLLAR|FMT_FLAG_ZERO_WIDTH_BAD|FMT_FLAG_DOLLAR_GAP_POINTER_OK,
    'w', 0, 0, '*', 'L', 'm',
    NULL, NULL
  },
  { "gnu_strftime", NULL,                 time_char_table,  "_-0^#", "EO",
    strftime_flag_specs, strftime_flag_pairs,
    FMT_FLAG_FANCY_PERCENT_OK, 'w', 0, 0, 0, 0, 0,
    NULL, NULL
  },
  { "gnu_strfmon",  strfmon_length_specs, monetary_char_table, "=^+(!-", NULL,
    strfmon_flag_specs, strfmon_flag_pairs,
    FMT_FLAG_ARG_CONVERT, 'w', '#', 'p', 0, 'L', 0,
    NULL, NULL
  }
};

/* This layer of indirection allows GCC to reassign format_types with
   new data if necessary, while still allowing the original data to be
   const.  */
static const format_kind_info *format_types = format_types_orig;
/* We can modify this one.  We also add target-specific format types
   to the end of the array.  */
static format_kind_info *dynamic_format_types;

static int n_format_types = ARRAY_SIZE (format_types_orig);

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
  location_t extra_arg_loc;
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
  /* Location of the format string.  */
  location_t format_string_loc;
} format_check_results;

typedef struct
{
  format_check_results *res;
  function_format_info *info;
  tree params;
} format_check_context;

/* Return the format name (as specified in the original table) for the format
   type indicated by format_num.  */
static const char *
format_name (int format_num)
{
  if (format_num >= 0 && format_num < n_format_types)
    return format_types[format_num].name;
  gcc_unreachable ();
}

/* Return the format flags (as specified in the original table) for the format
   type indicated by format_num.  */
static int
format_flags (int format_num)
{
  if (format_num >= 0 && format_num < n_format_types)
    return format_types[format_num].flags;
  gcc_unreachable ();
}

static void check_format_info (function_format_info *, tree);
static void check_format_arg (void *, tree, unsigned HOST_WIDE_INT);
static void check_format_info_main (format_check_results *,
				    function_format_info *,
				    const char *, int, tree,
				    unsigned HOST_WIDE_INT,
				    pool_allocator<format_wanted_type> &);

static void init_dollar_format_checking (int, tree);
static int maybe_read_dollar_number (const char **, int,
				     tree, tree *, const format_kind_info *);
static bool avoid_dollar_number (const char *);
static void finish_dollar_format_checking (format_check_results *, int);

static const format_flag_spec *get_flag_spec (const format_flag_spec *,
					      int, const char *);

static void check_format_types (location_t, format_wanted_type *);
static void format_type_warning (location_t, format_wanted_type *, tree, tree);

/* Decode a format type from a string, returning the type, or
   format_type_error if not valid, in which case the caller should print an
   error message.  */
static int
decode_format_type (const char *s)
{
  int i;
  int slen;

  s = convert_format_name_to_system_name (s);
  slen = strlen (s);
  for (i = 0; i < n_format_types; i++)
    {
      int alen;
      if (!strcmp (s, format_types[i].name))
	return i;
      alen = strlen (format_types[i].name);
      if (slen == alen + 4 && s[0] == '_' && s[1] == '_'
	  && s[slen - 1] == '_' && s[slen - 2] == '_'
	  && !strncmp (s + 2, format_types[i].name, alen))
	return i;
    }
  return format_type_error;
}


/* Check the argument list of a call to printf, scanf, etc.
   ATTRS are the attributes on the function type.  There are NARGS argument
   values in the array ARGARRAY.
   Also, if -Wsuggest-attribute=format,
   warn for calls to vprintf or vscanf in functions with no such format
   attribute themselves.  */

void
check_function_format (tree attrs, int nargs, tree *argarray)
{
  tree a;

  /* See if this function has any format attributes.  */
  for (a = attrs; a; a = TREE_CHAIN (a))
    {
      if (is_attribute_p ("format", TREE_PURPOSE (a)))
	{
	  /* Yup; check it.  */
	  function_format_info info;
	  decode_format_attr (TREE_VALUE (a), &info, /*validated=*/true);
	  if (warn_format)
	    {
	      /* FIXME: Rewrite all the internal functions in this file
		 to use the ARGARRAY directly instead of constructing this
		 temporary list.  */
	      tree params = NULL_TREE;
	      int i;
	      for (i = nargs - 1; i >= 0; i--)
		params = tree_cons (NULL_TREE, argarray[i], params);
	      check_format_info (&info, params);
	    }
	  if (warn_suggest_attribute_format && info.first_arg_num == 0
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
		       args = DECL_CHAIN (args))
		    {
		      if (TREE_CODE (TREE_TYPE (args)) == POINTER_TYPE
			  && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (args)))
			      == char_type_node))
			break;
		    }
		  if (args != 0)
		    warning (OPT_Wsuggest_attribute_format, "function might "
			     "be possible candidate for %qs format attribute",
			     format_types[info.format_type].name);
		}
	    }
	}
    }
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
init_dollar_format_checking (int first_arg_num, tree params)
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
      free (dollar_arguments_used);
      free (dollar_arguments_pointer_p);
      dollar_arguments_alloc = dollar_arguments_count;
      dollar_arguments_used = XNEWVEC (char, dollar_arguments_alloc);
      dollar_arguments_pointer_p = XNEWVEC (char, dollar_arguments_alloc);
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
maybe_read_dollar_number (const char **format,
			  int dollar_needed, tree params, tree *param_ptr,
			  const format_kind_info *fki)
{
  int argnum;
  int overflow_flag;
  const char *fcp = *format;
  if (!ISDIGIT (*fcp))
    {
      if (dollar_needed)
	{
	  warning (OPT_Wformat_, "missing $ operand number in format");
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
	  warning (OPT_Wformat_, "missing $ operand number in format");
	  return -1;
	}
      else
	return 0;
    }
  *format = fcp + 1;
  if (pedantic && !dollar_format_warned)
    {
      warning (OPT_Wformat_, "%s does not support %%n$ operand number formats",
	       C_STD_NAME (STD_EXT));
      dollar_format_warned = 1;
    }
  if (overflow_flag || argnum == 0
      || (dollar_first_arg_num && argnum > dollar_arguments_count))
    {
      warning (OPT_Wformat_, "operand number out of range in format");
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
      dollar_arguments_used = XRESIZEVEC (char, dollar_arguments_used,
					  nalloc);
      dollar_arguments_pointer_p = XRESIZEVEC (char, dollar_arguments_pointer_p,
					       nalloc);
      memset (dollar_arguments_used + dollar_arguments_alloc, 0,
	      nalloc - dollar_arguments_alloc);
      dollar_arguments_alloc = nalloc;
    }
  if (!(fki->flags & (int) FMT_FLAG_DOLLAR_MULTIPLE)
      && dollar_arguments_used[argnum - 1] == 1)
    {
      dollar_arguments_used[argnum - 1] = 2;
      warning (OPT_Wformat_, "format argument %d used more than once in %s format",
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

      /* This case shouldn't be caught here.  */
      gcc_assert (*param_ptr);
    }
  else
    *param_ptr = 0;
  return argnum;
}

/* Ensure that FORMAT does not start with a decimal number followed by
   a $; give a diagnostic and return true if it does, false otherwise.  */

static bool
avoid_dollar_number (const char *format)
{
  if (!ISDIGIT (*format))
    return false;
  while (ISDIGIT (*format))
    format++;
  if (*format == '$')
    {
      warning (OPT_Wformat_, "$ operand number used after format without operand number");
      return true;
    }
  return false;
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
finish_dollar_format_checking (format_check_results *res, int pointer_gap_ok)
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
	    warning_at (res->format_string_loc, OPT_Wformat_,
			"format argument %d unused before used argument %d in $-style format",
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
   spec for that flag must be retrieved and must exist.  If
   PREDICATES is not NULL, it is a string listing possible predicates
   for the spec entry; if an entry predicated on any of these is
   found, it is returned, otherwise NULL is returned.  */

static const format_flag_spec *
get_flag_spec (const format_flag_spec *spec, int flag, const char *predicates)
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
  gcc_assert (predicates);
  return NULL;
}


/* Check the argument list of a call to printf, scanf, etc.
   INFO points to the function_format_info structure.
   PARAMS is the list of argument values.  */

static void
check_format_info (function_format_info *info, tree params)
{
  format_check_context format_ctx;
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
  res.extra_arg_loc = UNKNOWN_LOCATION;
  res.number_dollar_extra_args = 0;
  res.number_wide = 0;
  res.number_empty = 0;
  res.number_unterminated = 0;
  res.number_other = 0;
  res.format_string_loc = input_location;

  format_ctx.res = &res;
  format_ctx.info = info;
  format_ctx.params = params;

  check_function_arguments_recurse (check_format_arg, &format_ctx,
				    format_tree, arg_num);

  location_t loc = format_ctx.res->format_string_loc;

  if (res.number_non_literal > 0)
    {
      /* Functions taking a va_list normally pass a non-literal format
	 string.  These functions typically are declared with
	 first_arg_num == 0, so avoid warning in those cases.  */
      if (!(format_types[info->format_type].flags & (int) FMT_FLAG_ARG_CONVERT))
	{
	  /* For strftime-like formats, warn for not checking the format
	     string; but there are no arguments to check.  */
	  warning_at (loc, OPT_Wformat_nonliteral,
		      "format not a string literal, format string not checked");
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
	  if (params == 0 && warn_format_security)
	    warning_at (loc, OPT_Wformat_security,
			"format not a string literal and no format arguments");
	  else if (params == 0 && warn_format_nonliteral)
	    warning_at (loc, OPT_Wformat_nonliteral,
			"format not a string literal and no format arguments");
	  else
	    warning_at (loc, OPT_Wformat_nonliteral,
			"format not a string literal, argument types not checked");
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
      && res.number_other == 0)
    {
      if (res.extra_arg_loc == UNKNOWN_LOCATION)
	res.extra_arg_loc = loc;
      warning_at (res.extra_arg_loc, OPT_Wformat_extra_args,
		  "too many arguments for format");
    }
  if (res.number_dollar_extra_args > 0 && res.number_non_literal == 0
      && res.number_other == 0)
    warning_at (loc, OPT_Wformat_extra_args, "unused arguments in $-style format");
  if (res.number_empty > 0 && res.number_non_literal == 0
      && res.number_other == 0)
    warning_at (loc, OPT_Wformat_zero_length, "zero-length %s format string",
	     format_types[info->format_type].name);

  if (res.number_wide > 0)
    warning_at (loc, OPT_Wformat_, "format is a wide character string");

  if (res.number_unterminated > 0)
    warning_at (loc, OPT_Wformat_, "unterminated format string");
}

/* Callback from check_function_arguments_recurse to check a
   format string.  FORMAT_TREE is the format parameter.  ARG_NUM
   is the number of the format argument.  CTX points to a
   format_check_context.  */

static void
check_format_arg (void *ctx, tree format_tree,
		  unsigned HOST_WIDE_INT arg_num)
{
  format_check_context *format_ctx = (format_check_context *) ctx;
  format_check_results *res = format_ctx->res;
  function_format_info *info = format_ctx->info;
  tree params = format_ctx->params;

  int format_length;
  HOST_WIDE_INT offset;
  const char *format_chars;
  tree array_size = 0;
  tree array_init;

  if (VAR_P (format_tree))
    {
      /* Pull out a constant value if the front end didn't.  */
      format_tree = decl_constant_value (format_tree);
      STRIP_NOPS (format_tree);
    }

  if (integer_zerop (format_tree))
    {
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
	{
	  if (res->number_extra_args == 0)
	    res->extra_arg_loc = EXPR_LOC_OR_LOC (TREE_VALUE (params),
						  input_location);
	  res->number_extra_args++;
	}
      return;
    }

  offset = 0;
  if (TREE_CODE (format_tree) == POINTER_PLUS_EXPR)
    {
      tree arg0, arg1;

      arg0 = TREE_OPERAND (format_tree, 0);
      arg1 = TREE_OPERAND (format_tree, 1);
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);
      if (TREE_CODE (arg1) == INTEGER_CST)
	format_tree = arg0;
      else
	{
	  res->number_non_literal++;
	  return;
	}
      /* POINTER_PLUS_EXPR offsets are to be interpreted signed.  */
      if (!cst_and_fits_in_hwi (arg1))
	{
	  res->number_non_literal++;
	  return;
	}
      offset = int_cst_value (arg1);
    }
  if (TREE_CODE (format_tree) != ADDR_EXPR)
    {
      res->number_non_literal++;
      return;
    }
  res->format_string_loc = EXPR_LOC_OR_LOC (format_tree, input_location);
  format_tree = TREE_OPERAND (format_tree, 0);
  if (format_types[info->format_type].flags 
      & (int) FMT_FLAG_PARSE_ARG_CONVERT_EXTERNAL)
    {
      bool objc_str = (info->format_type == gcc_objc_string_format_type);
      /* We cannot examine this string here - but we can check that it is
	 a valid type.  */
      if (TREE_CODE (format_tree) != CONST_DECL
	  || !((objc_str && objc_string_ref_type_p (TREE_TYPE (format_tree)))
		|| (*targetcm.string_object_ref_type_p) 
				     ((const_tree) TREE_TYPE (format_tree))))
	{
	  res->number_non_literal++;
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
      /* So, we have a valid literal string object and one or more params.
	 We need to use an external helper to parse the string into format
	 info.  For Objective-C variants we provide the resource within the
	 objc tree, for target variants, via a hook.  */
      if (objc_str)
	objc_check_format_arg (format_tree, params);
      else if (targetcm.check_string_object_format_arg)
	(*targetcm.check_string_object_format_arg) (format_tree, params);
      /* Else we can't handle it and retire quietly.  */
      return;
    }
  if (TREE_CODE (format_tree) == ARRAY_REF
      && tree_fits_shwi_p (TREE_OPERAND (format_tree, 1))
      && (offset += tree_to_shwi (TREE_OPERAND (format_tree, 1))) >= 0)
    format_tree = TREE_OPERAND (format_tree, 0);
  if (offset < 0)
    {
      res->number_non_literal++;
      return;
    }
  if (VAR_P (format_tree)
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
      gcc_assert (TREE_CODE (array_size) == INTEGER_CST);

      if (tree_fits_shwi_p (array_size))
	{
	  HOST_WIDE_INT array_size_value = tree_to_shwi (array_size);
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
  if (format_length < 1 || format_chars[--format_length] != 0)
    {
      res->number_unterminated++;
      return;
    }
  if (format_length == 0)
    {
      res->number_empty++;
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
  pool_allocator <format_wanted_type> fwt_pool ("format_wanted_type pool", 10);
  check_format_info_main (res, info, format_chars, format_length,
			  params, arg_num, fwt_pool);
}


/* Do the main part of checking a call to a format function.  FORMAT_CHARS
   is the NUL-terminated format string (which at this point may contain
   internal NUL characters); FORMAT_LENGTH is its length (excluding the
   terminating NUL character).  ARG_NUM is one less than the number of
   the first format argument to check; PARAMS points to that format
   argument in the list of arguments.  */

static void
check_format_info_main (format_check_results *res,
			function_format_info *info, const char *format_chars,
			int format_length, tree params,
			unsigned HOST_WIDE_INT arg_num,
			pool_allocator<format_wanted_type> &fwt_pool)
{
  const char *orig_format_chars = format_chars;
  tree first_fillin_param = params;

  const format_kind_info *fki = &format_types[info->format_type];
  const format_flag_spec *flag_specs = fki->flag_specs;
  const format_flag_pair *bad_flag_pairs = fki->bad_flag_pairs;
  location_t format_string_loc = res->format_string_loc;

  /* -1 if no conversions taking an operand have been found; 0 if one has
     and it didn't use $; 1 if $ formats are in use.  */
  int has_operand_number = -1;

  init_dollar_format_checking (info->first_arg_num, first_fillin_param);

  while (*format_chars != 0)
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
      int alloc_flag = 0;
      int scalar_identity_flag = 0;
      const char *format_start;

      if (*format_chars++ != '%')
	continue;
      if (*format_chars == 0)
	{
          warning_at (location_from_offset (format_string_loc,
					    format_chars - orig_format_chars),
		      OPT_Wformat_,
		      "spurious trailing %<%%%> in format");
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
	  opnum = maybe_read_dollar_number (&format_chars, 0,
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
      else if (fki->flags & FMT_FLAG_USE_DOLLAR)
	{
	  if (avoid_dollar_number (format_chars))
	    return;
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
	      warning_at (location_from_offset (format_string_loc,
						format_chars + 1
						- orig_format_chars),
			  OPT_Wformat_,
			  "repeated %s in format", _(s->name));
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
		  warning_at (format_string_loc, OPT_Wformat_,
			      "missing fill character at end of strfmon format");
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
	      if (has_operand_number != 0)
		{
		  int opnum;
		  opnum = maybe_read_dollar_number (&format_chars,
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
	      else
		{
		  if (avoid_dollar_number (format_chars))
		    return;
		}
	      if (info->first_arg_num != 0)
		{
		  if (params == 0)
                    cur_param = NULL;
                  else
                    {
                      cur_param = TREE_VALUE (params);
                      if (has_operand_number <= 0)
                        {
                          params = TREE_CHAIN (params);
                          ++arg_num;
                        }
                    }
		  width_wanted_type.wanted_type = *fki->width_type;
		  width_wanted_type.wanted_type_name = NULL;
		  width_wanted_type.pointer_count = 0;
		  width_wanted_type.char_lenient_flag = 0;
		  width_wanted_type.scalar_identity_flag = 0;
		  width_wanted_type.writing_in_flag = 0;
		  width_wanted_type.reading_from_flag = 0;
                  width_wanted_type.kind = CF_KIND_FIELD_WIDTH;
		  width_wanted_type.format_start = format_chars - 1;
		  width_wanted_type.format_length = 1;
		  width_wanted_type.param = cur_param;
		  width_wanted_type.arg_num = arg_num;
		  width_wanted_type.offset_loc =
		    format_chars - orig_format_chars;
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
		warning_at (format_string_loc, OPT_Wformat_,
			    "zero width in %s format", fki->name);
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
	    warning_at (location_from_offset (format_string_loc,
					      format_chars - orig_format_chars),
			OPT_Wformat_,
			"empty left precision in %s format", fki->name);
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
		  opnum = maybe_read_dollar_number (&format_chars,
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
	      else
		{
		  if (avoid_dollar_number (format_chars))
		    return;
		}
	      if (info->first_arg_num != 0)
		{
		  if (params == 0)
                    cur_param = NULL;
                  else
                    {
                      cur_param = TREE_VALUE (params);
                      if (has_operand_number <= 0)
                        {
                          params = TREE_CHAIN (params);
                          ++arg_num;
                        }
                    }
		  precision_wanted_type.wanted_type = *fki->precision_type;
		  precision_wanted_type.wanted_type_name = NULL;
		  precision_wanted_type.pointer_count = 0;
		  precision_wanted_type.char_lenient_flag = 0;
		  precision_wanted_type.scalar_identity_flag = 0;
		  precision_wanted_type.writing_in_flag = 0;
		  precision_wanted_type.reading_from_flag = 0;
                  precision_wanted_type.kind = CF_KIND_FIELD_PRECISION;
		  precision_wanted_type.param = cur_param;
		  precision_wanted_type.format_start = format_chars - 2;
		  precision_wanted_type.format_length = 2;
		  precision_wanted_type.arg_num = arg_num;
		  precision_wanted_type.offset_loc =
		    format_chars - orig_format_chars;
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
		warning_at (location_from_offset (format_string_loc,
						  format_chars - orig_format_chars),
			    OPT_Wformat_,
			    "empty precision in %s format", fki->name);
	      while (ISDIGIT (*format_chars))
		++format_chars;
	    }
	}

      format_start = format_chars;
      if (fki->alloc_char && fki->alloc_char == *format_chars)
	{
	  i = strlen (flag_chars);
	  flag_chars[i++] = fki->alloc_char;
	  flag_chars[i] = 0;
	  format_chars++;
	}

      /* Handle the scanf allocation kludge.  */
      if (fki->flags & (int) FMT_FLAG_SCANF_A_KLUDGE)
	{
	  if (*format_chars == 'a' && !flag_isoc99)
	    {
	      if (format_chars[1] == 's' || format_chars[1] == 'S'
		  || format_chars[1] == '[')
		{
		  /* 'a' is used as a flag.  */
		  i = strlen (flag_chars);
		  flag_chars[i++] = 'a';
		  flag_chars[i] = 0;
		  format_chars++;
		}
	    }
	}

      /* Read any length modifier, if this kind of format has them.  */
      fli = fki->length_char_specs;
      length_chars = NULL;
      length_chars_val = FMT_LEN_none;
      length_chars_std = STD_C89;
      scalar_identity_flag = 0;
      if (fli)
	{
	  while (fli->name != 0
 		 && strncmp (fli->name, format_chars, strlen (fli->name)))
	      fli++;
	  if (fli->name != 0)
	    {
 	      format_chars += strlen (fli->name);
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
		  scalar_identity_flag = fli->scalar_identity_flag;
		}
	      i = strlen (flag_chars);
	      flag_chars[i++] = fki->length_code_char;
	      flag_chars[i] = 0;
	    }
	  if (pedantic)
	    {
	      /* Warn if the length modifier is non-standard.  */
	      if (ADJ_STD (length_chars_std) > C_STD_VER)
		warning_at (format_string_loc, OPT_Wformat_,
			    "%s does not support the %qs %s length modifier",
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
		  warning_at (location_from_offset (format_string_loc,
						    format_chars 
						    - orig_format_chars),
			      OPT_Wformat_,
			      "repeated %s in format", _(s->name));
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

      format_char = *format_chars;
      if (format_char == 0
	  || (!(fki->flags & (int) FMT_FLAG_FANCY_PERCENT_OK)
	      && format_char == '%'))
	{
	  warning_at (location_from_offset (format_string_loc,
					    format_chars - orig_format_chars),
		      OPT_Wformat_,
		      "conversion lacks type at end of format");
	  continue;
	}
      format_chars++;
      fci = fki->conversion_specs;
      while (fci->format_chars != 0
	     && strchr (fci->format_chars, format_char) == 0)
	  ++fci;
      if (fci->format_chars == 0)
	{
	  if (ISGRAPH (format_char))
	    warning_at (location_from_offset (format_string_loc,
					      format_chars - orig_format_chars),
			OPT_Wformat_,
			"unknown conversion type character %qc in format",
			format_char);
	  else
	    warning_at (location_from_offset (format_string_loc,
					      format_chars - orig_format_chars),
			OPT_Wformat_,
			"unknown conversion type character 0x%x in format",
			format_char);
	  continue;
	}
      if (pedantic)
	{
	  if (ADJ_STD (fci->std) > C_STD_VER)
	    warning_at (location_from_offset (format_string_loc,
					      format_chars - orig_format_chars),
			OPT_Wformat_,
			"%s does not support the %<%%%c%> %s format",
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
		warning_at (location_from_offset (format_string_loc,
						  format_chars 
						  - orig_format_chars),
			    OPT_Wformat_, "%s used with %<%%%c%> %s format",
			    _(s->name), format_char, fki->name);
		d++;
		continue;
	      }
	    if (pedantic)
	      {
		const format_flag_spec *t;
		if (ADJ_STD (s->std) > C_STD_VER)
		  warning_at (format_string_loc, OPT_Wformat_,
			      "%s does not support %s",
                              C_STD_NAME (s->std), _(s->long_name));
		t = get_flag_spec (flag_specs, flag_chars[i], fci->flags2);
		if (t != NULL && ADJ_STD (t->std) > ADJ_STD (s->std))
		  {
		    const char *long_name = (t->long_name != NULL
					     ? t->long_name
					     : s->long_name);
		    if (ADJ_STD (t->std) > C_STD_VER)
		      warning_at (format_string_loc, OPT_Wformat_,
				  "%s does not support %s with the %<%%%c%> %s format",
				  C_STD_NAME (t->std), _(long_name),
				  format_char, fki->name);
		  }
	      }
	  }
	flag_chars[i - d] = 0;
      }

      if ((fki->flags & (int) FMT_FLAG_SCANF_A_KLUDGE)
	  && strchr (flag_chars, 'a') != 0)
	alloc_flag = 1;
      if (fki->alloc_char && strchr (flag_chars, fki->alloc_char) != 0)
	alloc_flag = 1;

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
		warning_at (format_string_loc, OPT_Wformat_,
			    "%s ignored with %s and %<%%%c%> %s format",
			    _(s->name), _(t->name), format_char,
			    fki->name);
	      else
		warning_at (format_string_loc, OPT_Wformat_,
			    "%s ignored with %s in %s format",
			    _(s->name), _(t->name), fki->name);
	    }
	  else
	    {
	      if (bad_flag_pairs[i].predicate != 0)
		warning_at (format_string_loc, OPT_Wformat_,
			    "use of %s and %s together with %<%%%c%> %s format",
			    _(s->name), _(t->name), format_char,
			    fki->name);
	      else
		warning_at (format_string_loc, OPT_Wformat_,
			    "use of %s and %s together in %s format",
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
	    warning_at (format_string_loc, OPT_Wformat_y2k,
			"%<%%%c%> yields only last 2 digits of "
			"year in some locales", format_char);
	  else if (y2k_level == 2)
	    warning_at (format_string_loc, OPT_Wformat_y2k,
			"%<%%%c%> yields only last 2 digits of year",
			format_char);
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
	    warning_at (location_from_offset (format_string_loc,
					      format_chars - orig_format_chars),
			OPT_Wformat_,
			"no closing %<]%> for %<%%[%> format");
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
	      warning_at (location_from_offset (format_string_loc,
						format_chars - orig_format_chars),
			  OPT_Wformat_,
			  "use of %qs length modifier with %qc type character"
			  " has either no effect or undefined behavior",
			  length_chars, format_char);
	      /* Heuristic: skip one argument when an invalid length/type
		 combination is encountered.  */
	      arg_num++;
	      if (params != 0)
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
		warning_at (location_from_offset (format_string_loc,
						  format_chars - orig_format_chars),
			    OPT_Wformat_,
			    "%s does not support the %<%%%s%c%> %s format",
			    C_STD_NAME (wanted_type_std), length_chars,
			    format_char, fki->name);
	    }
	}

      main_wanted_type.next = NULL;

      /* Finally. . .check type of argument against desired type!  */
      if (info->first_arg_num == 0)
	continue;
      if ((fci->pointer_count == 0 && wanted_type == void_type_node)
	  || suppressed)
	{
	  if (main_arg_num != 0)
	    {
	      if (suppressed)
		warning_at (format_string_loc, OPT_Wformat_,
			    "operand number specified with "
			    "suppressed assignment");
	      else
		warning_at (format_string_loc, OPT_Wformat_,
			    "operand number specified for format "
			    "taking no argument");
	    }
	}
      else
	{
	  format_wanted_type *wanted_type_ptr;

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
		  warning_at (format_string_loc, OPT_Wformat_,
			      "missing $ operand number in format");
		  return;
		}
	      else
		has_operand_number = 0;
	    }

	  wanted_type_ptr = &main_wanted_type;
	  while (fci)
	    {
	      if (params == 0)
                cur_param = NULL;
              else
                {
                  cur_param = TREE_VALUE (params);
                  params = TREE_CHAIN (params);
                }

	      wanted_type_ptr->wanted_type = wanted_type;
	      wanted_type_ptr->wanted_type_name = wanted_type_name;
	      wanted_type_ptr->pointer_count = fci->pointer_count + alloc_flag;
	      wanted_type_ptr->char_lenient_flag = 0;
	      if (strchr (fci->flags2, 'c') != 0)
		wanted_type_ptr->char_lenient_flag = 1;
	      wanted_type_ptr->scalar_identity_flag = 0;
	      if (scalar_identity_flag)
		wanted_type_ptr->scalar_identity_flag = 1;
	      wanted_type_ptr->writing_in_flag = 0;
	      wanted_type_ptr->reading_from_flag = 0;
	      if (alloc_flag)
		wanted_type_ptr->writing_in_flag = 1;
	      else
		{
		  if (strchr (fci->flags2, 'W') != 0)
		    wanted_type_ptr->writing_in_flag = 1;
		  if (strchr (fci->flags2, 'R') != 0)
		    wanted_type_ptr->reading_from_flag = 1;
		}
              wanted_type_ptr->kind = CF_KIND_FORMAT;
	      wanted_type_ptr->param = cur_param;
	      wanted_type_ptr->arg_num = arg_num;
	      wanted_type_ptr->format_start = format_start;
	      wanted_type_ptr->format_length = format_chars - format_start;
	      wanted_type_ptr->offset_loc = format_chars - orig_format_chars;
	      wanted_type_ptr->next = NULL;
	      if (last_wanted_type != 0)
		last_wanted_type->next = wanted_type_ptr;
	      if (first_wanted_type == 0)
		first_wanted_type = wanted_type_ptr;
	      last_wanted_type = wanted_type_ptr;

	      fci = fci->chain;
	      if (fci)
		{
		  wanted_type_ptr = fwt_pool.allocate ();
		  arg_num++;
		  wanted_type = *fci->types[length_chars_val].type;
		  wanted_type_name = fci->types[length_chars_val].name;
		}
	    }
	}

      if (first_wanted_type != 0)
        check_format_types (format_string_loc, first_wanted_type);
    }

  if (format_chars - orig_format_chars != format_length)
    warning_at (location_from_offset (format_string_loc,
				      format_chars + 1 - orig_format_chars),
		OPT_Wformat_contains_nul,
		"embedded %<\\0%> in format");
  if (info->first_arg_num != 0 && params != 0
      && has_operand_number <= 0)
    {
      res->number_other--;
      res->number_extra_args++;
    }
  if (has_operand_number > 0)
    finish_dollar_format_checking (res, fki->flags & (int) FMT_FLAG_DOLLAR_GAP_POINTER_OK);
}


/* Check the argument types from a single format conversion (possibly
   including width and precision arguments).  LOC is the location of
   the format string.  */
static void
check_format_types (location_t loc, format_wanted_type *types)
{
  for (; types != 0; types = types->next)
    {
      tree cur_param;
      tree cur_type;
      tree orig_cur_type;
      tree wanted_type;
      int arg_num;
      int i;
      int char_type_flag;

      wanted_type = types->wanted_type;
      arg_num = types->arg_num;

      /* The following should not occur here.  */
      gcc_assert (wanted_type);
      gcc_assert (wanted_type != void_type_node || types->pointer_count);

      if (types->pointer_count == 0)
	wanted_type = lang_hooks.types.type_promotes_to (wanted_type);

      wanted_type = TYPE_MAIN_VARIANT (wanted_type);

      cur_param = types->param;
      if (!cur_param)
        {
          format_type_warning (loc, types, wanted_type, NULL);
          continue;
        }

      cur_type = TREE_TYPE (cur_param);
      if (cur_type == error_mark_node)
	continue;
      orig_cur_type = cur_type;
      char_type_flag = 0;

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
		warning (OPT_Wformat_, "writing through null pointer "
			 "(argument %d)", arg_num);

	      /* Check for reading through a NULL pointer.  */
	      if (types->reading_from_flag
		  && i == 0
		  && cur_param != 0
		  && integer_zerop (cur_param))
		warning (OPT_Wformat_, "reading through null pointer "
			 "(argument %d)", arg_num);

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
			  && (CONSTANT_CLASS_P (cur_param)
			      || (DECL_P (cur_param)
				  && TREE_READONLY (cur_param))))))
		warning (OPT_Wformat_, "writing into constant object "
			 "(argument %d)", arg_num);

	      /* If there are extra type qualifiers beyond the first
		 indirection, then this makes the types technically
		 incompatible.  */
	      if (i > 0
		  && pedantic
		  && (TYPE_READONLY (cur_type)
		      || TYPE_VOLATILE (cur_type)
		      || TYPE_ATOMIC (cur_type)
		      || TYPE_RESTRICT (cur_type)))
		warning (OPT_Wformat_, "extra type qualifiers in format "
			 "argument (argument %d)",
			 arg_num);

	    }
	  else
	    {
              format_type_warning (loc, types, wanted_type, orig_cur_type);
	      break;
	    }
	}

      if (i < types->pointer_count)
	continue;

      cur_type = TYPE_MAIN_VARIANT (cur_type);

      /* Check whether the argument type is a character type.  This leniency
	 only applies to certain formats, flagged with 'c'.  */
      if (types->char_lenient_flag)
	char_type_flag = (cur_type == char_type_node
			  || cur_type == signed_char_type_node
			  || cur_type == unsigned_char_type_node);

      /* Check the type of the "real" argument, if there's a type we want.  */
      if (lang_hooks.types_compatible_p (wanted_type, cur_type))
	continue;
      /* If we want 'void *', allow any pointer type.
	 (Anything else would already have got a warning.)
	 With -Wpedantic, only allow pointers to void and to character
	 types.  */
      if (wanted_type == void_type_node
	  && (!pedantic || (i == 1 && char_type_flag)))
	continue;
      /* Don't warn about differences merely in signedness, unless
	 -Wpedantic.  With -Wpedantic, warn if the type is a pointer
	 target and not a character type, and for character types at
	 a second level of indirection.  */
      if (TREE_CODE (wanted_type) == INTEGER_TYPE
	  && TREE_CODE (cur_type) == INTEGER_TYPE
	  && ((!pedantic && !warn_format_signedness)
	      || (i == 0 && !warn_format_signedness)
	      || (i == 1 && char_type_flag))
	  && (TYPE_UNSIGNED (wanted_type)
	      ? wanted_type == c_common_unsigned_type (cur_type)
	      : wanted_type == c_common_signed_type (cur_type)))
	continue;
      /* Don't warn about differences merely in signedness if we know
	 that the current type is integer-promoted and its original type
	 was unsigned such as that it is in the range of WANTED_TYPE.  */
      if (TREE_CODE (wanted_type) == INTEGER_TYPE
	  && TREE_CODE (cur_type) == INTEGER_TYPE
	  && warn_format_signedness
	  && TYPE_UNSIGNED (wanted_type)
	  && cur_param != NULL_TREE
	  && TREE_CODE (cur_param) == NOP_EXPR)
	{
	  tree t = TREE_TYPE (TREE_OPERAND (cur_param, 0));
	  if (TYPE_UNSIGNED (t)
	      && cur_type == lang_hooks.types.type_promotes_to (t))
	    continue;
	}
      /* Likewise, "signed char", "unsigned char" and "char" are
	 equivalent but the above test won't consider them equivalent.  */
      if (wanted_type == char_type_node
	  && (!pedantic || i < 2)
	  && char_type_flag)
	continue;
      if (types->scalar_identity_flag
	  && (TREE_CODE (cur_type) == TREE_CODE (wanted_type)
	      || (INTEGRAL_TYPE_P (cur_type)
		  && INTEGRAL_TYPE_P (wanted_type)))
	  && TYPE_PRECISION (cur_type) == TYPE_PRECISION (wanted_type))
	continue;
      /* Now we have a type mismatch.  */
      format_type_warning (loc, types, wanted_type, orig_cur_type);
    }
}


/* Give a warning at LOC about a format argument of different type from that
   expected.  WANTED_TYPE is the type the argument should have, possibly
   stripped of pointer dereferences.  The description (such as "field
   precision"), the placement in the format string, a possibly more
   friendly name of WANTED_TYPE, and the number of pointer dereferences
   are taken from TYPE.  ARG_TYPE is the type of the actual argument,
   or NULL if it is missing.  */
static void
format_type_warning (location_t loc, format_wanted_type *type,
		     tree wanted_type, tree arg_type)
{
  int kind = type->kind;
  const char *wanted_type_name = type->wanted_type_name;
  const char *format_start = type->format_start;
  int format_length = type->format_length;
  int pointer_count = type->pointer_count;
  int arg_num = type->arg_num;
  unsigned int offset_loc = type->offset_loc;

  char *p;
  /* If ARG_TYPE is a typedef with a misleading name (for example,
     size_t but not the standard size_t expected by printf %zu), avoid
     printing the typedef name.  */
  if (wanted_type_name
      && arg_type
      && TYPE_NAME (arg_type)
      && TREE_CODE (TYPE_NAME (arg_type)) == TYPE_DECL
      && DECL_NAME (TYPE_NAME (arg_type))
      && !strcmp (wanted_type_name,
		  lang_hooks.decl_printable_name (TYPE_NAME (arg_type), 2)))
    arg_type = TYPE_MAIN_VARIANT (arg_type);
  /* The format type and name exclude any '*' for pointers, so those
     must be formatted manually.  For all the types we currently have,
     this is adequate, but formats taking pointers to functions or
     arrays would require the full type to be built up in order to
     print it with %T.  */
  p = (char *) alloca (pointer_count + 2);
  if (pointer_count == 0)
    p[0] = 0;
  else if (c_dialect_cxx ())
    {
      memset (p, '*', pointer_count);
      p[pointer_count] = 0;
    }
  else
    {
      p[0] = ' ';
      memset (p + 1, '*', pointer_count);
      p[pointer_count + 1] = 0;
    }

  loc = location_from_offset (loc, offset_loc);
		      
  if (wanted_type_name)
    {
      if (arg_type)
        warning_at (loc, OPT_Wformat_,
		    "%s %<%s%.*s%> expects argument of type %<%s%s%>, "
		    "but argument %d has type %qT",
		    gettext (kind_descriptions[kind]),
		    (kind == CF_KIND_FORMAT ? "%" : ""),
		    format_length, format_start, 
		    wanted_type_name, p, arg_num, arg_type);
      else
        warning_at (loc, OPT_Wformat_,
		    "%s %<%s%.*s%> expects a matching %<%s%s%> argument",
		    gettext (kind_descriptions[kind]),
		    (kind == CF_KIND_FORMAT ? "%" : ""),
		    format_length, format_start, wanted_type_name, p);
    }
  else
    {
      if (arg_type)
        warning_at (loc, OPT_Wformat_,
		    "%s %<%s%.*s%> expects argument of type %<%T%s%>, "
		    "but argument %d has type %qT",
		    gettext (kind_descriptions[kind]),
		    (kind == CF_KIND_FORMAT ? "%" : ""),
		    format_length, format_start, 
		    wanted_type, p, arg_num, arg_type);
      else
        warning_at (loc, OPT_Wformat_,
		    "%s %<%s%.*s%> expects a matching %<%T%s%> argument",
		    gettext (kind_descriptions[kind]),
		    (kind == CF_KIND_FORMAT ? "%" : ""),
		    format_length, format_start, wanted_type, p);
    }
}


/* Given a format_char_info array FCI, and a character C, this function
   returns the index into the conversion_specs where that specifier's
   data is located.  The character must exist.  */
static unsigned int
find_char_info_specifier_index (const format_char_info *fci, int c)
{
  unsigned i;

  for (i = 0; fci->format_chars; i++, fci++)
    if (strchr (fci->format_chars, c))
      return i;

  /* We shouldn't be looking for a non-existent specifier.  */
  gcc_unreachable ();
}

/* Given a format_length_info array FLI, and a character C, this
   function returns the index into the conversion_specs where that
   modifier's data is located.  The character must exist.  */
static unsigned int
find_length_info_modifier_index (const format_length_info *fli, int c)
{
  unsigned i;

  for (i = 0; fli->name; i++, fli++)
    if (strchr (fli->name, c))
      return i;

  /* We shouldn't be looking for a non-existent modifier.  */
  gcc_unreachable ();
}

/* Determine the type of HOST_WIDE_INT in the code being compiled for
   use in GCC's __asm_fprintf__ custom format attribute.  You must
   have set dynamic_format_types before calling this function.  */
static void
init_dynamic_asm_fprintf_info (void)
{
  static tree hwi;

  if (!hwi)
    {
      format_length_info *new_asm_fprintf_length_specs;
      unsigned int i;

      /* Find the underlying type for HOST_WIDE_INT.  For the %w
	 length modifier to work, one must have issued: "typedef
	 HOST_WIDE_INT __gcc_host_wide_int__;" in one's source code
	 prior to using that modifier.  */
      hwi = maybe_get_identifier ("__gcc_host_wide_int__");
      if (!hwi)
	{
	  error ("%<__gcc_host_wide_int__%> is not defined as a type");
	  return;
	}
      hwi = identifier_global_value (hwi);
      if (!hwi || TREE_CODE (hwi) != TYPE_DECL)
	{
	  error ("%<__gcc_host_wide_int__%> is not defined as a type");
	  return;
	}
      hwi = DECL_ORIGINAL_TYPE (hwi);
      gcc_assert (hwi);
      if (hwi != long_integer_type_node && hwi != long_long_integer_type_node)
	{
	  error ("%<__gcc_host_wide_int__%> is not defined as %<long%>"
		 " or %<long long%>");
	  return;
	}

      /* Create a new (writable) copy of asm_fprintf_length_specs.  */
      new_asm_fprintf_length_specs = (format_length_info *)
				     xmemdup (asm_fprintf_length_specs,
					      sizeof (asm_fprintf_length_specs),
					      sizeof (asm_fprintf_length_specs));

      /* HOST_WIDE_INT must be one of 'long' or 'long long'.  */
      i = find_length_info_modifier_index (new_asm_fprintf_length_specs, 'w');
      if (hwi == long_integer_type_node)
	new_asm_fprintf_length_specs[i].index = FMT_LEN_l;
      else if (hwi == long_long_integer_type_node)
	new_asm_fprintf_length_specs[i].index = FMT_LEN_ll;
      else
	gcc_unreachable ();

      /* Assign the new data for use.  */
      dynamic_format_types[asm_fprintf_format_type].length_char_specs =
	new_asm_fprintf_length_specs;
    }
}

/* Determine the type of a "locus" in the code being compiled for use
   in GCC's __gcc_gfc__ custom format attribute.  You must have set
   dynamic_format_types before calling this function.  */
static void
init_dynamic_gfc_info (void)
{
  static tree locus;

  if (!locus)
    {
      static format_char_info *gfc_fci;

      /* For the GCC __gcc_gfc__ custom format specifier to work, one
	 must have declared 'locus' prior to using this attribute.  If
	 we haven't seen this declarations then you shouldn't use the
	 specifier requiring that type.  */
      if ((locus = maybe_get_identifier ("locus")))
	{
	  locus = identifier_global_value (locus);
	  if (locus)
	    {
	      if (TREE_CODE (locus) != TYPE_DECL
		  || TREE_TYPE (locus) == error_mark_node)
		{
		  error ("%<locus%> is not defined as a type");
		  locus = 0;
		}
	      else
		locus = TREE_TYPE (locus);
	    }
	}

      /* Assign the new data for use.  */

      /* Handle the __gcc_gfc__ format specifics.  */
      if (!gfc_fci)
	dynamic_format_types[gcc_gfc_format_type].conversion_specs =
	  gfc_fci = (format_char_info *)
		     xmemdup (gcc_gfc_char_table,
			      sizeof (gcc_gfc_char_table),
			      sizeof (gcc_gfc_char_table));
      if (locus)
	{
	  const unsigned i = find_char_info_specifier_index (gfc_fci, 'L');
	  gfc_fci[i].types[0].type = &locus;
	  gfc_fci[i].pointer_count = 1;
	}
    }
}

/* Determine the types of "tree" and "location_t" in the code being
   compiled for use in GCC's diagnostic custom format attributes.  You
   must have set dynamic_format_types before calling this function.  */
static void
init_dynamic_diag_info (void)
{
  static tree t, loc, hwi;

  if (!loc || !t || !hwi)
    {
      static format_char_info *diag_fci, *tdiag_fci, *cdiag_fci, *cxxdiag_fci;
      static format_length_info *diag_ls;
      unsigned int i;

      /* For the GCC-diagnostics custom format specifiers to work, one
	 must have declared 'tree' and/or 'location_t' prior to using
	 those attributes.  If we haven't seen these declarations then
	 you shouldn't use the specifiers requiring these types.
	 However we don't force a hard ICE because we may see only one
	 or the other type.  */
      if ((loc = maybe_get_identifier ("location_t")))
	{
	  loc = identifier_global_value (loc);
	  if (loc)
	    {
	      if (TREE_CODE (loc) != TYPE_DECL)
		{
		  error ("%<location_t%> is not defined as a type");
		  loc = 0;
		}
	      else
		loc = TREE_TYPE (loc);
	    }
	}

      /* We need to grab the underlying 'union tree_node' so peek into
	 an extra type level.  */
      if ((t = maybe_get_identifier ("tree")))
	{
	  t = identifier_global_value (t);
	  if (t)
	    {
	      if (TREE_CODE (t) != TYPE_DECL)
		{
		  error ("%<tree%> is not defined as a type");
		  t = 0;
		}
	      else if (TREE_CODE (TREE_TYPE (t)) != POINTER_TYPE)
		{
		  error ("%<tree%> is not defined as a pointer type");
		  t = 0;
		}
	      else
		t = TREE_TYPE (TREE_TYPE (t));
	    }
	}

      /* Find the underlying type for HOST_WIDE_INT.  For the %w
	 length modifier to work, one must have issued: "typedef
	 HOST_WIDE_INT __gcc_host_wide_int__;" in one's source code
	 prior to using that modifier.  */
      if ((hwi = maybe_get_identifier ("__gcc_host_wide_int__")))
	{
	  hwi = identifier_global_value (hwi);
	  if (hwi)
	    {
	      if (TREE_CODE (hwi) != TYPE_DECL)
		{
		  error ("%<__gcc_host_wide_int__%> is not defined as a type");
		  hwi = 0;
		}
	      else
		{
		  hwi = DECL_ORIGINAL_TYPE (hwi);
		  gcc_assert (hwi);
		  if (hwi != long_integer_type_node
		      && hwi != long_long_integer_type_node)
		    {
		      error ("%<__gcc_host_wide_int__%> is not defined"
			     " as %<long%> or %<long long%>");
		      hwi = 0;
		    }
		}
	    }
	}

      /* Assign the new data for use.  */

      /* All the GCC diag formats use the same length specs.  */
      if (!diag_ls)
	dynamic_format_types[gcc_diag_format_type].length_char_specs =
	  dynamic_format_types[gcc_tdiag_format_type].length_char_specs =
	  dynamic_format_types[gcc_cdiag_format_type].length_char_specs =
	  dynamic_format_types[gcc_cxxdiag_format_type].length_char_specs =
	  diag_ls = (format_length_info *)
		    xmemdup (gcc_diag_length_specs,
			     sizeof (gcc_diag_length_specs),
			     sizeof (gcc_diag_length_specs));
      if (hwi)
	{
	  /* HOST_WIDE_INT must be one of 'long' or 'long long'.  */
	  i = find_length_info_modifier_index (diag_ls, 'w');
	  if (hwi == long_integer_type_node)
	    diag_ls[i].index = FMT_LEN_l;
	  else if (hwi == long_long_integer_type_node)
	    diag_ls[i].index = FMT_LEN_ll;
	  else
	    gcc_unreachable ();
	}

      /* Handle the __gcc_diag__ format specifics.  */
      if (!diag_fci)
	dynamic_format_types[gcc_diag_format_type].conversion_specs =
	  diag_fci = (format_char_info *)
		     xmemdup (gcc_diag_char_table,
			      sizeof (gcc_diag_char_table),
			      sizeof (gcc_diag_char_table));
      if (t)
	{
	  i = find_char_info_specifier_index (diag_fci, 'K');
	  diag_fci[i].types[0].type = &t;
	  diag_fci[i].pointer_count = 1;
	}

      /* Handle the __gcc_tdiag__ format specifics.  */
      if (!tdiag_fci)
	dynamic_format_types[gcc_tdiag_format_type].conversion_specs =
	  tdiag_fci = (format_char_info *)
		      xmemdup (gcc_tdiag_char_table,
			       sizeof (gcc_tdiag_char_table),
			       sizeof (gcc_tdiag_char_table));
      if (t)
	{
	  /* All specifiers taking a tree share the same struct.  */
	  i = find_char_info_specifier_index (tdiag_fci, 'D');
	  tdiag_fci[i].types[0].type = &t;
	  tdiag_fci[i].pointer_count = 1;
	  i = find_char_info_specifier_index (tdiag_fci, 'K');
	  tdiag_fci[i].types[0].type = &t;
	  tdiag_fci[i].pointer_count = 1;
	}

      /* Handle the __gcc_cdiag__ format specifics.  */
      if (!cdiag_fci)
	dynamic_format_types[gcc_cdiag_format_type].conversion_specs =
	  cdiag_fci = (format_char_info *)
		      xmemdup (gcc_cdiag_char_table,
			       sizeof (gcc_cdiag_char_table),
			       sizeof (gcc_cdiag_char_table));
      if (t)
	{
	  /* All specifiers taking a tree share the same struct.  */
	  i = find_char_info_specifier_index (cdiag_fci, 'D');
	  cdiag_fci[i].types[0].type = &t;
	  cdiag_fci[i].pointer_count = 1;
	  i = find_char_info_specifier_index (cdiag_fci, 'K');
	  cdiag_fci[i].types[0].type = &t;
	  cdiag_fci[i].pointer_count = 1;
	}

      /* Handle the __gcc_cxxdiag__ format specifics.  */
      if (!cxxdiag_fci)
	dynamic_format_types[gcc_cxxdiag_format_type].conversion_specs =
	  cxxdiag_fci = (format_char_info *)
			xmemdup (gcc_cxxdiag_char_table,
				 sizeof (gcc_cxxdiag_char_table),
				 sizeof (gcc_cxxdiag_char_table));
      if (t)
	{
	  /* All specifiers taking a tree share the same struct.  */
	  i = find_char_info_specifier_index (cxxdiag_fci, 'D');
	  cxxdiag_fci[i].types[0].type = &t;
	  cxxdiag_fci[i].pointer_count = 1;
	  i = find_char_info_specifier_index (cxxdiag_fci, 'K');
	  cxxdiag_fci[i].types[0].type = &t;
	  cxxdiag_fci[i].pointer_count = 1;
	}
    }
}

#ifdef TARGET_FORMAT_TYPES
extern const format_kind_info TARGET_FORMAT_TYPES[];
#endif

#ifdef TARGET_OVERRIDES_FORMAT_ATTRIBUTES
extern const target_ovr_attr TARGET_OVERRIDES_FORMAT_ATTRIBUTES[];
#endif
#ifdef TARGET_OVERRIDES_FORMAT_INIT
  extern void TARGET_OVERRIDES_FORMAT_INIT (void);
#endif

/* Attributes such as "printf" are equivalent to those such as
   "gnu_printf" unless this is overridden by a target.  */
static const target_ovr_attr gnu_target_overrides_format_attributes[] =
{
  { "gnu_printf",   "printf" },
  { "gnu_scanf",    "scanf" },
  { "gnu_strftime", "strftime" },
  { "gnu_strfmon",  "strfmon" },
  { NULL,           NULL }
};

/* Translate to unified attribute name. This is used in decode_format_type and
   decode_format_attr. In attr_name the user specified argument is passed. It
   returns the unified format name from TARGET_OVERRIDES_FORMAT_ATTRIBUTES
   or the attr_name passed to this function, if there is no matching entry.  */
static const char *
convert_format_name_to_system_name (const char *attr_name)
{
  int i;

  if (attr_name == NULL || *attr_name == 0
      || strncmp (attr_name, "gcc_", 4) == 0)
    return attr_name;
#ifdef TARGET_OVERRIDES_FORMAT_INIT
  TARGET_OVERRIDES_FORMAT_INIT ();
#endif

#ifdef TARGET_OVERRIDES_FORMAT_ATTRIBUTES
  /* Check if format attribute is overridden by target.  */
  if (TARGET_OVERRIDES_FORMAT_ATTRIBUTES != NULL
      && TARGET_OVERRIDES_FORMAT_ATTRIBUTES_COUNT > 0)
    {
      for (i = 0; i < TARGET_OVERRIDES_FORMAT_ATTRIBUTES_COUNT; ++i)
        {
          if (cmp_attribs (TARGET_OVERRIDES_FORMAT_ATTRIBUTES[i].named_attr_src,
			   attr_name))
            return attr_name;
          if (cmp_attribs (TARGET_OVERRIDES_FORMAT_ATTRIBUTES[i].named_attr_dst,
			   attr_name))
            return TARGET_OVERRIDES_FORMAT_ATTRIBUTES[i].named_attr_src;
        }
    }
#endif
  /* Otherwise default to gnu format.  */
  for (i = 0;
       gnu_target_overrides_format_attributes[i].named_attr_src != NULL;
       ++i)
    {
      if (cmp_attribs (gnu_target_overrides_format_attributes[i].named_attr_src,
		       attr_name))
        return attr_name;
      if (cmp_attribs (gnu_target_overrides_format_attributes[i].named_attr_dst,
		       attr_name))
        return gnu_target_overrides_format_attributes[i].named_attr_src;
    }

  return attr_name;
}

/* Return true if TATTR_NAME and ATTR_NAME are the same format attribute,
   counting "name" and "__name__" as the same, false otherwise.  */
static bool
cmp_attribs (const char *tattr_name, const char *attr_name)
{
  int alen = strlen (attr_name);
  int slen = (tattr_name ? strlen (tattr_name) : 0);
  if (alen > 4 && attr_name[0] == '_' && attr_name[1] == '_'
      && attr_name[alen - 1] == '_' && attr_name[alen - 2] == '_')
    {
      attr_name += 2;
      alen -= 4;
    }
  if (alen != slen || strncmp (tattr_name, attr_name, alen) != 0)
    return false;
  return true;
}

/* Handle a "format" attribute; arguments as in
   struct attribute_spec.handler.  */
tree
handle_format_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			 int flags, bool *no_add_attrs)
{
  tree type = *node;
  function_format_info info;

#ifdef TARGET_FORMAT_TYPES
  /* If the target provides additional format types, we need to
     add them to FORMAT_TYPES at first use.  */
  if (TARGET_FORMAT_TYPES != NULL && !dynamic_format_types)
    {
      dynamic_format_types = XNEWVEC (format_kind_info,
				      n_format_types + TARGET_N_FORMAT_TYPES);
      memcpy (dynamic_format_types, format_types_orig,
	      sizeof (format_types_orig));
      memcpy (&dynamic_format_types[n_format_types], TARGET_FORMAT_TYPES,
	      TARGET_N_FORMAT_TYPES * sizeof (dynamic_format_types[0]));

      format_types = dynamic_format_types;
      /* Provide a reference for the first potential external type.  */
      first_target_format_type = n_format_types;
      n_format_types += TARGET_N_FORMAT_TYPES;
    }
#endif

  if (!decode_format_attr (args, &info, 0))
    {
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (prototype_p (type))
    {
      if (!check_format_string (type, info.format_num, flags,
				no_add_attrs, info.format_type))
	return NULL_TREE;

      if (info.first_arg_num != 0)
	{
	  unsigned HOST_WIDE_INT arg_num = 1;
	  function_args_iterator iter;
	  tree arg_type;

	  /* Verify that first_arg_num points to the last arg,
	     the ...  */
	  FOREACH_FUNCTION_ARGS (type, arg_type, iter)
	    arg_num++;

	  if (arg_num != info.first_arg_num)
	    {
	      if (!(flags & (int) ATTR_FLAG_BUILT_IN))
		error ("args to be formatted is not %<...%>");
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }
	}
    }

  /* Check if this is a strftime variant. Just for this variant
     FMT_FLAG_ARG_CONVERT is not set.  */
  if ((format_types[info.format_type].flags & (int) FMT_FLAG_ARG_CONVERT) == 0
      && info.first_arg_num != 0)
    {
      error ("strftime formats cannot format arguments");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* If this is a custom GCC-internal format type, we have to
     initialize certain bits at runtime.  */
  if (info.format_type == asm_fprintf_format_type
      || info.format_type == gcc_gfc_format_type
      || info.format_type == gcc_diag_format_type
      || info.format_type == gcc_tdiag_format_type
      || info.format_type == gcc_cdiag_format_type
      || info.format_type == gcc_cxxdiag_format_type)
    {
      /* Our first time through, we have to make sure that our
	 format_type data is allocated dynamically and is modifiable.  */
      if (!dynamic_format_types)
	format_types = dynamic_format_types = (format_kind_info *)
	  xmemdup (format_types_orig, sizeof (format_types_orig),
		   sizeof (format_types_orig));

      /* If this is format __asm_fprintf__, we have to initialize
	 GCC's notion of HOST_WIDE_INT for checking %wd.  */
      if (info.format_type == asm_fprintf_format_type)
	init_dynamic_asm_fprintf_info ();
      /* If this is format __gcc_gfc__, we have to initialize GCC's
	 notion of 'locus' at runtime for %L.  */
      else if (info.format_type == gcc_gfc_format_type)
	init_dynamic_gfc_info ();
      /* If this is one of the diagnostic attributes, then we have to
	 initialize 'location_t' and 'tree' at runtime.  */
      else if (info.format_type == gcc_diag_format_type
	       || info.format_type == gcc_tdiag_format_type
	       || info.format_type == gcc_cdiag_format_type
	       || info.format_type == gcc_cxxdiag_format_type)
	init_dynamic_diag_info ();
      else
	gcc_unreachable ();
    }

  return NULL_TREE;
}
