/* Mainly the interface between cpplib and the C front ends.
   Copyright (C) 1987, 1988, 1989, 1992, 1994, 1995, 1996, 1997
   1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "tm.h"

#include "real.h"
#include "rtl.h"
#include "tree.h"
#include "expr.h"
#include "input.h"
#include "output.h"
#include "c-tree.h"
#include "c-common.h"
#include "flags.h"
#include "timevar.h"
#include "cpplib.h"
#include "c-pragma.h"
#include "toplev.h"
#include "intl.h"
#include "tm_p.h"
#include "splay-tree.h"
#include "debug.h"

/* The current line map.  */
static const struct line_map *map;

/* We may keep statistics about how long which files took to compile.  */
static int header_time, body_time;
static splay_tree file_info_tree;

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE TYPE_PRECISION (wchar_type_node)

/* Number of bytes in a wide character.  */
#define WCHAR_BYTES (WCHAR_TYPE_SIZE / BITS_PER_UNIT)

int pending_lang_change; /* If we need to switch languages - C++ only */
int c_header_level;	 /* depth in C headers - C++ only */

static tree interpret_integer (const cpp_token *, unsigned int);
static tree interpret_float (const cpp_token *, unsigned int);
static enum integer_type_kind
  narrowest_unsigned_type (tree, unsigned int);
static enum integer_type_kind
  narrowest_signed_type (tree, unsigned int);
static enum cpp_ttype lex_string (const cpp_token *, tree *, bool);
static tree lex_charconst (const cpp_token *);
static void update_header_times (const char *);
static int dump_one_header (splay_tree_node, void *);
static void cb_line_change (cpp_reader *, const cpp_token *, int);
static void cb_ident (cpp_reader *, unsigned int, const cpp_string *);
static void cb_def_pragma (cpp_reader *, unsigned int);
static void cb_define (cpp_reader *, unsigned int, cpp_hashnode *);
static void cb_undef (cpp_reader *, unsigned int, cpp_hashnode *);

void
init_c_lex (void)
{
  struct cpp_callbacks *cb;
  struct c_fileinfo *toplevel;

  /* Set up filename timing.  Must happen before cpp_read_main_file.  */
  file_info_tree = splay_tree_new ((splay_tree_compare_fn)strcmp,
				   0,
				   (splay_tree_delete_value_fn)free);
  toplevel = get_fileinfo ("<top level>");
  if (flag_detailed_statistics)
    {
      header_time = 0;
      body_time = get_run_time ();
      toplevel->time = body_time;
    }

  cb = cpp_get_callbacks (parse_in);

  cb->line_change = cb_line_change;
  cb->ident = cb_ident;
  cb->def_pragma = cb_def_pragma;
  cb->valid_pch = c_common_valid_pch;
  cb->read_pch = c_common_read_pch;

  /* Set the debug callbacks if we can use them.  */
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && (write_symbols == DWARF_DEBUG || write_symbols == DWARF2_DEBUG
          || write_symbols == VMS_AND_DWARF2_DEBUG))
    {
      cb->define = cb_define;
      cb->undef = cb_undef;
    }
}

struct c_fileinfo *
get_fileinfo (const char *name)
{
  splay_tree_node n;
  struct c_fileinfo *fi;

  n = splay_tree_lookup (file_info_tree, (splay_tree_key) name);
  if (n)
    return (struct c_fileinfo *) n->value;

  fi = xmalloc (sizeof (struct c_fileinfo));
  fi->time = 0;
  fi->interface_only = 0;
  fi->interface_unknown = 1;
  splay_tree_insert (file_info_tree, (splay_tree_key) name,
		     (splay_tree_value) fi);
  return fi;
}

static void
update_header_times (const char *name)
{
  /* Changing files again.  This means currently collected time
     is charged against header time, and body time starts back at 0.  */
  if (flag_detailed_statistics)
    {
      int this_time = get_run_time ();
      struct c_fileinfo *file = get_fileinfo (name);
      header_time += this_time - body_time;
      file->time += this_time - body_time;
      body_time = this_time;
    }
}

static int
dump_one_header (splay_tree_node n, void *dummy ATTRIBUTE_UNUSED)
{
  print_time ((const char *) n->key,
	      ((struct c_fileinfo *) n->value)->time);
  return 0;
}

void
dump_time_statistics (void)
{
  struct c_fileinfo *file = get_fileinfo (input_filename);
  int this_time = get_run_time ();
  file->time += this_time - body_time;

  fprintf (stderr, "\n******\n");
  print_time ("header files (total)", header_time);
  print_time ("main file (total)", this_time - body_time);
  fprintf (stderr, "ratio = %g : 1\n",
	   (double)header_time / (double)(this_time - body_time));
  fprintf (stderr, "\n******\n");

  splay_tree_foreach (file_info_tree, dump_one_header, 0);
}

static void
cb_ident (cpp_reader *pfile ATTRIBUTE_UNUSED,
	  unsigned int line ATTRIBUTE_UNUSED,
	  const cpp_string *str ATTRIBUTE_UNUSED)
{
#ifdef ASM_OUTPUT_IDENT
  if (! flag_no_ident)
    {
      /* Convert escapes in the string.  */
      cpp_string cstr = { 0, 0 };
      if (cpp_interpret_string (pfile, str, 1, &cstr, false))
	{
	  ASM_OUTPUT_IDENT (asm_out_file, (const char *) cstr.text);
	  free ((void *)cstr.text);
	}
    }
#endif
}

/* Called at the start of every non-empty line.  TOKEN is the first
   lexed token on the line.  Used for diagnostic line numbers.  */
static void
cb_line_change (cpp_reader *pfile ATTRIBUTE_UNUSED, const cpp_token *token,
		int parsing_args)
{
  if (token->type == CPP_EOF || parsing_args)
    return;

  input_line = SOURCE_LINE (map, token->line);
}

void
fe_file_change (const struct line_map *new_map)
{
  if (new_map == NULL)
    {
      map = NULL;
      return;
    }

  if (new_map->reason == LC_ENTER)
    {
      /* Don't stack the main buffer on the input stack;
	 we already did in compile_file.  */
      if (map != NULL)
	{
          int included_at = SOURCE_LINE (new_map - 1, new_map->from_line - 1);

	  input_line = included_at;
	  push_srcloc (new_map->to_file, 1);
	  (*debug_hooks->start_source_file) (included_at, new_map->to_file);
#ifndef NO_IMPLICIT_EXTERN_C
	  if (c_header_level)
	    ++c_header_level;
	  else if (new_map->sysp == 2)
	    {
	      c_header_level = 1;
	      ++pending_lang_change;
	    }
#endif
	}
    }
  else if (new_map->reason == LC_LEAVE)
    {
#ifndef NO_IMPLICIT_EXTERN_C
      if (c_header_level && --c_header_level == 0)
	{
	  if (new_map->sysp == 2)
	    warning ("badly nested C headers from preprocessor");
	  --pending_lang_change;
	}
#endif
      pop_srcloc ();

      (*debug_hooks->end_source_file) (new_map->to_line);
    }

  update_header_times (new_map->to_file);
  in_system_header = new_map->sysp != 0;
  input_filename = new_map->to_file;
  input_line = new_map->to_line;
  map = new_map;

  /* Hook for C++.  */
  extract_interface_info ();
}

static void
cb_def_pragma (cpp_reader *pfile, unsigned int line)
{
  /* Issue a warning message if we have been asked to do so.  Ignore
     unknown pragmas in system headers unless an explicit
     -Wunknown-pragmas has been given.  */
  if (warn_unknown_pragmas > in_system_header)
    {
      const unsigned char *space, *name;
      const cpp_token *s;

      space = name = (const unsigned char *) "";
      s = cpp_get_token (pfile);
      if (s->type != CPP_EOF)
	{
	  space = cpp_token_as_text (pfile, s);
	  s = cpp_get_token (pfile);
	  if (s->type == CPP_NAME)
	    name = cpp_token_as_text (pfile, s);
	}

      input_line = SOURCE_LINE (map, line);
      warning ("ignoring #pragma %s %s", space, name);
    }
}

/* #define callback for DWARF and DWARF2 debug info.  */
static void
cb_define (cpp_reader *pfile, unsigned int line, cpp_hashnode *node)
{
  (*debug_hooks->define) (SOURCE_LINE (map, line),
			  (const char *) cpp_macro_definition (pfile, node));
}

/* #undef callback for DWARF and DWARF2 debug info.  */
static void
cb_undef (cpp_reader *pfile ATTRIBUTE_UNUSED, unsigned int line,
	  cpp_hashnode *node)
{
  (*debug_hooks->undef) (SOURCE_LINE (map, line),
			 (const char *) NODE_NAME (node));
}

static inline const cpp_token *
get_nonpadding_token (void)
{
  const cpp_token *tok;
  timevar_push (TV_CPP);
  do
    tok = cpp_get_token (parse_in);
  while (tok->type == CPP_PADDING);
  timevar_pop (TV_CPP);

  return tok;
}  

int
c_lex_with_flags (tree *value, unsigned char *cpp_flags)
{
  const cpp_token *tok;
  location_t atloc;
  static bool no_more_pch;

 retry:
  tok = get_nonpadding_token ();

 retry_after_at:
  switch (tok->type)
    {
    case CPP_NAME:
      *value = HT_IDENT_TO_GCC_IDENT (HT_NODE (tok->val.node));
      break;

    case CPP_NUMBER:
      {
	unsigned int flags = cpp_classify_number (parse_in, tok);

	switch (flags & CPP_N_CATEGORY)
	  {
	  case CPP_N_INVALID:
	    /* cpplib has issued an error.  */
	    *value = error_mark_node;
	    break;

	  case CPP_N_INTEGER:
	    *value = interpret_integer (tok, flags);
	    break;

	  case CPP_N_FLOATING:
	    *value = interpret_float (tok, flags);
	    break;

	  default:
	    abort ();
	  }
      }
      break;

    case CPP_ATSIGN:
      /* An @ may give the next token special significance in Objective-C.  */
      atloc = input_location;
      tok = get_nonpadding_token ();
      if (c_dialect_objc ())
	{
	  tree val;
	  switch (tok->type)
	    {
	    case CPP_NAME:
	      val = HT_IDENT_TO_GCC_IDENT (HT_NODE (tok->val.node));
	      if (C_IS_RESERVED_WORD (val)
		  && OBJC_IS_AT_KEYWORD (C_RID_CODE (val)))
		{
		  *value = val;
		  return CPP_AT_NAME;
		}
	      break;

	    case CPP_STRING:
	    case CPP_WSTRING:
	      return lex_string (tok, value, true);

	    default: break;
	    }
	}

      /* ... or not.  */
      error ("%Hstray '@' in program", &atloc);
      goto retry_after_at;

    case CPP_OTHER:
      {
	cppchar_t c = tok->val.str.text[0];

	if (c == '"' || c == '\'')
	  error ("missing terminating %c character", (int) c);
	else if (ISGRAPH (c))
	  error ("stray '%c' in program", (int) c);
	else
	  error ("stray '\\%o' in program", (int) c);
      }
      goto retry;

    case CPP_CHAR:
    case CPP_WCHAR:
      *value = lex_charconst (tok);
      break;

    case CPP_STRING:
    case CPP_WSTRING:
      return lex_string (tok, value, false);
      break;

      /* These tokens should not be visible outside cpplib.  */
    case CPP_HEADER_NAME:
    case CPP_COMMENT:
    case CPP_MACRO_ARG:
      abort ();

    default:
      *value = NULL_TREE;
      break;
    }

  if (! no_more_pch)
    {
      no_more_pch = true;
      c_common_no_more_pch ();
    }

  if (cpp_flags)
    *cpp_flags = tok->flags;
  return tok->type;
}

int
c_lex (tree *value)
{
  return c_lex_with_flags (value, NULL);
}

/* Returns the narrowest C-visible unsigned type, starting with the
   minimum specified by FLAGS, that can fit VALUE, or itk_none if
   there isn't one.  */
static enum integer_type_kind
narrowest_unsigned_type (tree value, unsigned int flags)
{
  enum integer_type_kind itk;

  if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
    itk = itk_unsigned_int;
  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
    itk = itk_unsigned_long;
  else
    itk = itk_unsigned_long_long;

  /* int_fits_type_p must think the type of its first argument is
     wider than its second argument, or it won't do the proper check.  */
  TREE_TYPE (value) = widest_unsigned_literal_type_node;

  for (; itk < itk_none; itk += 2 /* skip unsigned types */)
    if (int_fits_type_p (value, integer_types[itk]))
      return itk;

  return itk_none;
}

/* Ditto, but narrowest signed type.  */
static enum integer_type_kind
narrowest_signed_type (tree value, unsigned int flags)
{
  enum integer_type_kind itk;

  if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
    itk = itk_int;
  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
    itk = itk_long;
  else
    itk = itk_long_long;

  /* int_fits_type_p must think the type of its first argument is
     wider than its second argument, or it won't do the proper check.  */
  TREE_TYPE (value) = widest_unsigned_literal_type_node;

  for (; itk < itk_none; itk += 2 /* skip signed types */)
    if (int_fits_type_p (value, integer_types[itk]))
      return itk;

  return itk_none;
}

/* Interpret TOKEN, an integer with FLAGS as classified by cpplib.  */
static tree
interpret_integer (const cpp_token *token, unsigned int flags)
{
  tree value, type;
  enum integer_type_kind itk;
  cpp_num integer;
  cpp_options *options = cpp_get_options (parse_in);

  integer = cpp_interpret_integer (parse_in, token, flags);
  integer = cpp_num_sign_extend (integer, options->precision);
  value = build_int_2_wide (integer.low, integer.high);

  /* The type of a constant with a U suffix is straightforward.  */
  if (flags & CPP_N_UNSIGNED)
    itk = narrowest_unsigned_type (value, flags);
  else
    {
      /* The type of a potentially-signed integer constant varies
	 depending on the base it's in, the standard in use, and the
	 length suffixes.  */
      enum integer_type_kind itk_u = narrowest_unsigned_type (value, flags);
      enum integer_type_kind itk_s = narrowest_signed_type (value, flags);

      /* In both C89 and C99, octal and hex constants may be signed or
	 unsigned, whichever fits tighter.  We do not warn about this
	 choice differing from the traditional choice, as the constant
	 is probably a bit pattern and either way will work.  */
      if ((flags & CPP_N_RADIX) != CPP_N_DECIMAL)
	itk = MIN (itk_u, itk_s);
      else
	{
	  /* In C99, decimal constants are always signed.
	     In C89, decimal constants that don't fit in long have
	     undefined behavior; we try to make them unsigned long.
	     In GCC's extended C89, that last is true of decimal
	     constants that don't fit in long long, too.  */

	  itk = itk_s;
	  if (itk_s > itk_u && itk_s > itk_long)
	    {
	      if (!flag_isoc99)
		{
		  if (itk_u < itk_unsigned_long)
		    itk_u = itk_unsigned_long;
		  itk = itk_u;
		  warning ("this decimal constant is unsigned only in ISO C90");
		}
	      else if (warn_traditional)
		warning ("this decimal constant would be unsigned in ISO C90");
	    }
	}
    }

  if (itk == itk_none)
    /* cpplib has already issued a warning for overflow.  */
    type = ((flags & CPP_N_UNSIGNED)
	    ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);
  else
    type = integer_types[itk];

  if (itk > itk_unsigned_long
      && (flags & CPP_N_WIDTH) != CPP_N_LARGE
      && ! in_system_header && ! flag_isoc99)
    pedwarn ("integer constant is too large for \"%s\" type",
	     (flags & CPP_N_UNSIGNED) ? "unsigned long" : "long");

  TREE_TYPE (value) = type;

  /* Convert imaginary to a complex type.  */
  if (flags & CPP_N_IMAGINARY)
    value = build_complex (NULL_TREE, convert (type, integer_zero_node), value);

  return value;
}

/* Interpret TOKEN, a floating point number with FLAGS as classified
   by cpplib.  */
static tree
interpret_float (const cpp_token *token, unsigned int flags)
{
  tree type;
  tree value;
  REAL_VALUE_TYPE real;
  char *copy;
  size_t copylen;
  const char *typename;

  /* FIXME: make %T work in error/warning, then we don't need typename.  */
  if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
    {
      type = long_double_type_node;
      typename = "long double";
    }
  else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL
	   || flag_single_precision_constant)
    {
      type = float_type_node;
      typename = "float";
    }
  else
    {
      type = double_type_node;
      typename = "double";
    }

  /* Copy the constant to a nul-terminated buffer.  If the constant
     has any suffixes, cut them off; REAL_VALUE_ATOF/ REAL_VALUE_HTOF
     can't handle them.  */
  copylen = token->val.str.len;
  if ((flags & CPP_N_WIDTH) != CPP_N_MEDIUM)
    /* Must be an F or L suffix.  */
    copylen--;
  if (flags & CPP_N_IMAGINARY)
    /* I or J suffix.  */
    copylen--;

  copy = alloca (copylen + 1);
  memcpy (copy, token->val.str.text, copylen);
  copy[copylen] = '\0';

  real_from_string (&real, copy);
  real_convert (&real, TYPE_MODE (type), &real);

  /* A diagnostic is required for "soft" overflow by some ISO C
     testsuites.  This is not pedwarn, because some people don't want
     an error for this.
     ??? That's a dubious reason... is this a mandatory diagnostic or
     isn't it?   -- zw, 2001-08-21.  */
  if (REAL_VALUE_ISINF (real) && pedantic)
    warning ("floating constant exceeds range of \"%s\"", typename);

  /* Create a node with determined type and value.  */
  value = build_real (type, real);
  if (flags & CPP_N_IMAGINARY)
    value = build_complex (NULL_TREE, convert (type, integer_zero_node), value);

  return value;
}

/* Convert a series of STRING and/or WSTRING tokens into a tree,
   performing string constant concatenation.  TOK is the first of
   these.  VALP is the location to write the string into.  OBJC_STRING
   indicates whether an '@' token preceded the incoming token.
   Returns the CPP token type of the result (CPP_STRING, CPP_WSTRING,
   or CPP_OBJC_STRING).

   This is unfortunately more work than it should be.  If any of the
   strings in the series has an L prefix, the result is a wide string
   (6.4.5p4).  Whether or not the result is a wide string affects the
   meaning of octal and hexadecimal escapes (6.4.4.4p6,9).  But escape
   sequences do not continue across the boundary between two strings in
   a series (6.4.5p7), so we must not lose the boundaries.  Therefore
   cpp_interpret_string takes a vector of cpp_string structures, which
   we must arrange to provide.  */

static enum cpp_ttype
lex_string (const cpp_token *tok, tree *valp, bool objc_string)
{
  tree value;
  bool wide = false;
  size_t count = 1;
  struct obstack str_ob;
  cpp_string istr;

  /* Try to avoid the overhead of creating and destroying an obstack
     for the common case of just one string.  */
  cpp_string str = tok->val.str;
  cpp_string *strs = &str;

  if (tok->type == CPP_WSTRING)
    wide = true;

  tok = get_nonpadding_token ();
  if (c_dialect_objc () && tok->type == CPP_ATSIGN)
    {
      objc_string = true;
      tok = get_nonpadding_token ();
    }
  if (tok->type == CPP_STRING || tok->type == CPP_WSTRING)
    {
      gcc_obstack_init (&str_ob);
      obstack_grow (&str_ob, &str, sizeof (cpp_string));

      do
	{
	  count++;
	  if (tok->type == CPP_WSTRING)
	    wide = true;
	  obstack_grow (&str_ob, &tok->val.str, sizeof (cpp_string));
	  
	  tok = get_nonpadding_token ();
	  if (c_dialect_objc () && tok->type == CPP_ATSIGN)
	    {
	      objc_string = true;
	      tok = get_nonpadding_token ();
	    }
	}
      while (tok->type == CPP_STRING || tok->type == CPP_WSTRING);
      strs = obstack_finish (&str_ob);
    }

  /* We have read one more token than we want.  */
  _cpp_backup_tokens (parse_in, 1);

  if (count > 1 && !objc_string && warn_traditional && !in_system_header)
    warning ("traditional C rejects string constant concatenation");

  if (cpp_interpret_string (parse_in, strs, count, &istr, wide))
    {
      value = build_string (istr.len, (char *)istr.text);
      free ((void *)istr.text);
    }
  else
    {
      /* Callers cannot generally handle error_mark_node in this context,
	 so return the empty string instead.  cpp_interpret_string has
	 issued an error.  */
      if (wide)
	value = build_string (TYPE_PRECISION (wchar_type_node)
			      / TYPE_PRECISION (char_type_node),
			      "\0\0\0");  /* widest supported wchar_t
					     is 32 bits */
      else
	value = build_string (1, "");
    }

  TREE_TYPE (value) = wide ? wchar_array_type_node : char_array_type_node;
  *valp = fix_string_type (value);

  if (strs != &str)
    obstack_free (&str_ob, 0);

  return objc_string ? CPP_OBJC_STRING : wide ? CPP_WSTRING : CPP_STRING;
}

/* Converts a (possibly wide) character constant token into a tree.  */
static tree
lex_charconst (const cpp_token *token)
{
  cppchar_t result;
  tree type, value;
  unsigned int chars_seen;
  int unsignedp;

  result = cpp_interpret_charconst (parse_in, token,
				    &chars_seen, &unsignedp);

  /* Cast to cppchar_signed_t to get correct sign-extension of RESULT
     before possibly widening to HOST_WIDE_INT for build_int_2.  */
  if (unsignedp || (cppchar_signed_t) result >= 0)
    value = build_int_2 (result, 0);
  else
    value = build_int_2 ((cppchar_signed_t) result, -1);

  if (token->type == CPP_WCHAR)
    type = wchar_type_node;
  /* In C, a character constant has type 'int'.
     In C++ 'char', but multi-char charconsts have type 'int'.  */
  else if (!c_dialect_cxx () || chars_seen > 1)
    type = integer_type_node;
  else
    type = char_type_node;

  TREE_TYPE (value) = type;
  return value;
}
