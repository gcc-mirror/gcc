/* Mainly the interface between cpplib and the C front ends.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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
#include "timevar.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "c-pragma.h"
#include "debug.h"
#include "file-prefix-map.h" /* remap_macro_filename()  */

#include "attribs.h"

/* We may keep statistics about how long which files took to compile.  */
static int header_time, body_time;
static splay_tree file_info_tree;

int pending_lang_change; /* If we need to switch languages - C++ only */
int c_header_level;	 /* depth in C headers - C++ only */

static tree interpret_integer (const cpp_token *, unsigned int,
			       enum overflow_type *);
static tree interpret_float (const cpp_token *, unsigned int, const char *,
			     enum overflow_type *);
static tree interpret_fixed (const cpp_token *, unsigned int);
static enum integer_type_kind narrowest_unsigned_type
	(const widest_int &, unsigned int);
static enum integer_type_kind narrowest_signed_type
	(const widest_int &, unsigned int);
static enum cpp_ttype lex_string (const cpp_token *, tree *, bool, bool);
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

  /* The get_fileinfo data structure must be initialized before
     cpp_read_main_file is called.  */
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
  cb->has_attribute = c_common_has_attribute;
  cb->get_source_date_epoch = cb_get_source_date_epoch;
  cb->get_suggestion = cb_get_suggestion;
  cb->remap_filename = remap_macro_filename;

  /* Set the debug callbacks if we can use them.  */
  if ((debug_info_level == DINFO_LEVEL_VERBOSE
       && (write_symbols == DWARF2_DEBUG
	   || write_symbols == VMS_AND_DWARF2_DEBUG))
      || flag_dump_go_spec != NULL)
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

  if (!file_info_tree)
    file_info_tree = splay_tree_new (splay_tree_compare_strings,
				     0,
				     splay_tree_delete_pointers);

  n = splay_tree_lookup (file_info_tree, (splay_tree_key) name);
  if (n)
    return (struct c_fileinfo *) n->value;

  fi = XNEW (struct c_fileinfo);
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
dump_one_header (splay_tree_node n, void * ARG_UNUSED (dummy))
{
  print_time ((const char *) n->key,
	      ((struct c_fileinfo *) n->value)->time);
  return 0;
}

void
dump_time_statistics (void)
{
  struct c_fileinfo *file = get_fileinfo (LOCATION_FILE (input_location));
  int this_time = get_run_time ();
  file->time += this_time - body_time;

  fprintf (stderr, "\n******\n");
  print_time ("header files (total)", header_time);
  print_time ("main file (total)", this_time - body_time);
  fprintf (stderr, "ratio = %g : 1\n",
	   (double) header_time / (double) (this_time - body_time));
  fprintf (stderr, "\n******\n");

  splay_tree_foreach (file_info_tree, dump_one_header, 0);
}

static void
cb_ident (cpp_reader * ARG_UNUSED (pfile),
	  unsigned int ARG_UNUSED (line),
	  const cpp_string * ARG_UNUSED (str))
{
  if (!flag_no_ident)
    {
      /* Convert escapes in the string.  */
      cpp_string cstr = { 0, 0 };
      if (cpp_interpret_string (pfile, str, 1, &cstr, CPP_STRING))
	{
	  targetm.asm_out.output_ident ((const char *) cstr.text);
	  free (CONST_CAST (unsigned char *, cstr.text));
	}
    }
}

/* Called at the start of every non-empty line.  TOKEN is the first
   lexed token on the line.  Used for diagnostic line numbers.  */
static void
cb_line_change (cpp_reader * ARG_UNUSED (pfile), const cpp_token *token,
		int parsing_args)
{
  if (token->type != CPP_EOF && !parsing_args)
    input_location = token->src_loc;
}

void
fe_file_change (const line_map_ordinary *new_map)
{
  if (new_map == NULL)
    return;

  if (new_map->reason == LC_ENTER)
    {
      /* Don't stack the main buffer on the input stack;
	 we already did in compile_file.  */
      if (!MAIN_FILE_P (new_map))
	{
	  unsigned int included_at = LAST_SOURCE_LINE_LOCATION (new_map - 1);
	  int line = 0;
	  if (included_at > BUILTINS_LOCATION)
	    line = SOURCE_LINE (new_map - 1, included_at);

	  input_location = new_map->start_location;
	  (*debug_hooks->start_source_file) (line, LINEMAP_FILE (new_map));
#ifndef NO_IMPLICIT_EXTERN_C
	  if (c_header_level)
	    ++c_header_level;
	  else if (LINEMAP_SYSP (new_map) == 2)
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
	  if (LINEMAP_SYSP (new_map) == 2)
	    warning (0, "badly nested C headers from preprocessor");
	  --pending_lang_change;
	}
#endif
      input_location = new_map->start_location;

      (*debug_hooks->end_source_file) (LINEMAP_LINE (new_map));
    }

  update_header_times (LINEMAP_FILE (new_map));
  input_location = new_map->start_location;
}

static void
cb_def_pragma (cpp_reader *pfile, source_location loc)
{
  /* Issue a warning message if we have been asked to do so.  Ignore
     unknown pragmas in system headers unless an explicit
     -Wunknown-pragmas has been given.  */
  if (warn_unknown_pragmas > in_system_header_at (input_location))
    {
      const unsigned char *space, *name;
      const cpp_token *s;
      location_t fe_loc = loc;

      space = name = (const unsigned char *) "";
      s = cpp_get_token (pfile);
      if (s->type != CPP_EOF)
	{
	  space = cpp_token_as_text (pfile, s);
	  s = cpp_get_token (pfile);
	  if (s->type == CPP_NAME)
	    name = cpp_token_as_text (pfile, s);
	}

      warning_at (fe_loc, OPT_Wunknown_pragmas, "ignoring #pragma %s %s",
		  space, name);
    }
}

/* #define callback for DWARF and DWARF2 debug info.  */
static void
cb_define (cpp_reader *pfile, source_location loc, cpp_hashnode *node)
{
  const struct line_map *map = linemap_lookup (line_table, loc);
  (*debug_hooks->define) (SOURCE_LINE (linemap_check_ordinary (map), loc),
			  (const char *) cpp_macro_definition (pfile, node));
}

/* #undef callback for DWARF and DWARF2 debug info.  */
static void
cb_undef (cpp_reader * ARG_UNUSED (pfile), source_location loc,
	  cpp_hashnode *node)
{
  const struct line_map *map = linemap_lookup (line_table, loc);
  (*debug_hooks->undef) (SOURCE_LINE (linemap_check_ordinary (map), loc),
			 (const char *) NODE_NAME (node));
}

/* Wrapper around cpp_get_token to skip CPP_PADDING tokens
   and not consume CPP_EOF.  */
static const cpp_token *
get_token_no_padding (cpp_reader *pfile)
{
  for (;;)
    {
      const cpp_token *ret = cpp_peek_token (pfile, 0);
      if (ret->type == CPP_EOF)
	return ret;
      ret = cpp_get_token (pfile);
      if (ret->type != CPP_PADDING)
	return ret;
    }
}

/* Callback for has_attribute.  */
int
c_common_has_attribute (cpp_reader *pfile)
{
  int result = 0;
  tree attr_name = NULL_TREE;
  const cpp_token *token;

  token = get_token_no_padding (pfile);
  if (token->type != CPP_OPEN_PAREN)
    {
      cpp_error (pfile, CPP_DL_ERROR,
		 "missing '(' after \"__has_attribute\"");
      return 0;
    }
  token = get_token_no_padding (pfile);
  if (token->type == CPP_NAME)
    {
      attr_name = get_identifier ((const char *)
				  cpp_token_as_text (pfile, token));
      attr_name = canonicalize_attr_name (attr_name);
      if (c_dialect_cxx ())
	{
	  int idx = 0;
	  const cpp_token *nxt_token;
	  do
	    nxt_token = cpp_peek_token (pfile, idx++);
	  while (nxt_token->type == CPP_PADDING);
	  if (nxt_token->type == CPP_SCOPE)
	    {
	      get_token_no_padding (pfile); // Eat scope.
	      nxt_token = get_token_no_padding (pfile);
	      if (nxt_token->type == CPP_NAME)
		{
		  tree attr_ns = attr_name;
		  tree attr_id
		    = get_identifier ((const char *)
				      cpp_token_as_text (pfile, nxt_token));
		  attr_name = build_tree_list (attr_ns, attr_id);
		}
	      else
		{
		  cpp_error (pfile, CPP_DL_ERROR,
			     "attribute identifier required after scope");
		  attr_name = NULL_TREE;
		}
	    }
	  else
	    {
	      /* Some standard attributes need special handling.  */
	      if (is_attribute_p ("noreturn", attr_name))
		result = 200809;
	      else if (is_attribute_p ("deprecated", attr_name))
		result = 201309;
	      else if (is_attribute_p ("maybe_unused", attr_name)
		       || is_attribute_p ("nodiscard", attr_name)
		       || is_attribute_p ("fallthrough", attr_name))
		result = 201603;
	      if (result)
		attr_name = NULL_TREE;
	    }
	}
      if (attr_name)
	{
	  init_attributes ();
	  const struct attribute_spec *attr = lookup_attribute_spec (attr_name);
	  if (attr)
	    result = 1;
	}
    }
  else
    {
      cpp_error (pfile, CPP_DL_ERROR,
		 "macro \"__has_attribute\" requires an identifier");
      return 0;
    }

  if (get_token_no_padding (pfile)->type != CPP_CLOSE_PAREN)
    cpp_error (pfile, CPP_DL_ERROR,
	       "missing ')' after \"__has_attribute\"");

  return result;
}

/* Read a token and return its type.  Fill *VALUE with its value, if
   applicable.  Fill *CPP_FLAGS with the token's flags, if it is
   non-NULL.  */

enum cpp_ttype
c_lex_with_flags (tree *value, location_t *loc, unsigned char *cpp_flags,
		  int lex_flags)
{
  static bool no_more_pch;
  const cpp_token *tok;
  enum cpp_ttype type;
  unsigned char add_flags = 0;
  enum overflow_type overflow = OT_NONE;

  timevar_push (TV_CPP);
 retry:
  tok = cpp_get_token_with_location (parse_in, loc);
  type = tok->type;

 retry_after_at:
  switch (type)
    {
    case CPP_PADDING:
      goto retry;

    case CPP_NAME:
      *value = HT_IDENT_TO_GCC_IDENT (HT_NODE (tok->val.node.node));
      break;

    case CPP_NUMBER:
      {
	const char *suffix = NULL;
	unsigned int flags = cpp_classify_number (parse_in, tok, &suffix, *loc);

	switch (flags & CPP_N_CATEGORY)
	  {
	  case CPP_N_INVALID:
	    /* cpplib has issued an error.  */
	    *value = error_mark_node;
	    break;

	  case CPP_N_INTEGER:
	    /* C++ uses '0' to mark virtual functions as pure.
	       Set PURE_ZERO to pass this information to the C++ parser.  */
	    if (tok->val.str.len == 1 && *tok->val.str.text == '0')
	      add_flags = PURE_ZERO;
	    *value = interpret_integer (tok, flags, &overflow);
	    break;

	  case CPP_N_FLOATING:
	    *value = interpret_float (tok, flags, suffix, &overflow);
	    break;

	  default:
	    gcc_unreachable ();
	  }

	if (flags & CPP_N_USERDEF)
	  {
	    char *str;
	    tree literal;
	    tree suffix_id = get_identifier (suffix);
	    int len = tok->val.str.len - strlen (suffix);
	    /* If this is going to be used as a C string to pass to a
	       raw literal operator, we need to add a trailing NUL.  */
	    tree num_string = build_string (len + 1,
					    (const char *) tok->val.str.text);
	    TREE_TYPE (num_string) = char_array_type_node;
	    num_string = fix_string_type (num_string);
	    str = CONST_CAST (char *, TREE_STRING_POINTER (num_string));
	    str[len] = '\0';
	    literal = build_userdef_literal (suffix_id, *value, overflow,
					     num_string);
	    *value = literal;
	  }
      }
      break;

    case CPP_ATSIGN:
      /* An @ may give the next token special significance in Objective-C.  */
      if (c_dialect_objc ())
	{
	  location_t atloc = *loc;
	  location_t newloc;

	retry_at:
	  tok = cpp_get_token_with_location (parse_in, &newloc);
	  type = tok->type;
	  switch (type)
	    {
	    case CPP_PADDING:
	      goto retry_at;

	    case CPP_STRING:
	    case CPP_WSTRING:
	    case CPP_STRING16:
	    case CPP_STRING32:
	    case CPP_UTF8STRING:
	      type = lex_string (tok, value, true, true);
	      break;

	    case CPP_NAME:
	      *value = HT_IDENT_TO_GCC_IDENT (HT_NODE (tok->val.node.node));
	      if (OBJC_IS_AT_KEYWORD (C_RID_CODE (*value))
		  || OBJC_IS_CXX_KEYWORD (C_RID_CODE (*value)))
		{
		  type = CPP_AT_NAME;
		  /* Note the complication: if we found an OBJC_CXX
		     keyword, for example, 'class', we will be
		     returning a token of type CPP_AT_NAME and rid
		     code RID_CLASS (not RID_AT_CLASS).  The language
		     parser needs to convert that to RID_AT_CLASS.
		  */
		  break;
		}
	      /* FALLTHROUGH */

	    default:
	      /* ... or not.  */
	      error_at (atloc, "stray %<@%> in program");
	      *loc = newloc;
	      goto retry_after_at;
	    }
	  break;
	}

      /* FALLTHROUGH */
    case CPP_HASH:
    case CPP_PASTE:
      {
	unsigned char name[8];

	*cpp_spell_token (parse_in, tok, name, true) = 0;

	error_at (*loc, "stray %qs in program", name);
      }

      goto retry;

    case CPP_OTHER:
      {
	cppchar_t c = tok->val.str.text[0];

	if (c == '"' || c == '\'')
	  error_at (*loc, "missing terminating %c character", (int) c);
	else if (ISGRAPH (c))
	  error_at (*loc, "stray %qc in program", (int) c);
	else
	  error_at (*loc, "stray %<\\%o%> in program", (int) c);
      }
      goto retry;

    case CPP_CHAR_USERDEF:
    case CPP_WCHAR_USERDEF:
    case CPP_CHAR16_USERDEF:
    case CPP_CHAR32_USERDEF:
    case CPP_UTF8CHAR_USERDEF:
      {
	tree literal;
	cpp_token temp_tok = *tok;
	const char *suffix = cpp_get_userdef_suffix (tok);
	temp_tok.val.str.len -= strlen (suffix);
	temp_tok.type = cpp_userdef_char_remove_type (type);
	literal = build_userdef_literal (get_identifier (suffix),
					 lex_charconst (&temp_tok),
					 OT_NONE, NULL_TREE);
	*value = literal;
      }
      break;

    case CPP_CHAR:
    case CPP_WCHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_UTF8CHAR:
      *value = lex_charconst (tok);
      break;

    case CPP_STRING_USERDEF:
    case CPP_WSTRING_USERDEF:
    case CPP_STRING16_USERDEF:
    case CPP_STRING32_USERDEF:
    case CPP_UTF8STRING_USERDEF:
      {
	tree literal, string;
	const char *suffix = cpp_get_userdef_suffix (tok);
	string = build_string (tok->val.str.len - strlen (suffix),
			       (const char *) tok->val.str.text);
	literal = build_userdef_literal (get_identifier (suffix),
					 string, OT_NONE, NULL_TREE);
	*value = literal;
      }
      break;

    case CPP_STRING:
    case CPP_WSTRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_UTF8STRING:
      if ((lex_flags & C_LEX_STRING_NO_JOIN) == 0)
	{
	  type = lex_string (tok, value, false,
			     (lex_flags & C_LEX_STRING_NO_TRANSLATE) == 0);
	  break;
	}
      *value = build_string (tok->val.str.len, (const char *) tok->val.str.text);
      break;

    case CPP_PRAGMA:
      *value = build_int_cst (integer_type_node, tok->val.pragma);
      break;

      /* These tokens should not be visible outside cpplib.  */
    case CPP_HEADER_NAME:
    case CPP_MACRO_ARG:
      gcc_unreachable ();

    /* CPP_COMMENT will appear when compiling with -C.  Ignore, except
       when it is a FALLTHROUGH comment, in that case set
       PREV_FALLTHROUGH flag on the next non-comment token.  */
    case CPP_COMMENT:
      if (tok->flags & PREV_FALLTHROUGH)
	{
	  do
	    {
	      tok = cpp_get_token_with_location (parse_in, loc);
	      type = tok->type;
	    }
	  while (type == CPP_PADDING || type == CPP_COMMENT);
	  add_flags |= PREV_FALLTHROUGH;
	  goto retry_after_at;
	}
       goto retry;

    default:
      *value = NULL_TREE;
      break;
    }

  if (cpp_flags)
    *cpp_flags = tok->flags | add_flags;

  if (!no_more_pch)
    {
      no_more_pch = true;
      c_common_no_more_pch ();
    }

  timevar_pop (TV_CPP);

  return type;
}

/* Returns the narrowest C-visible unsigned type, starting with the
   minimum specified by FLAGS, that can fit HIGH:LOW, or itk_none if
   there isn't one.  */

static enum integer_type_kind
narrowest_unsigned_type (const widest_int &val, unsigned int flags)
{
  int itk;

  if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
    itk = itk_unsigned_int;
  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
    itk = itk_unsigned_long;
  else
    itk = itk_unsigned_long_long;

  for (; itk < itk_none; itk += 2 /* skip unsigned types */)
    {
      tree upper;

      if (integer_types[itk] == NULL_TREE)
	continue;
      upper = TYPE_MAX_VALUE (integer_types[itk]);

      if (wi::geu_p (wi::to_widest (upper), val))
	return (enum integer_type_kind) itk;
    }

  return itk_none;
}

/* Ditto, but narrowest signed type.  */
static enum integer_type_kind
narrowest_signed_type (const widest_int &val, unsigned int flags)
{
  int itk;

  if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
    itk = itk_int;
  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
    itk = itk_long;
  else
    itk = itk_long_long;

  for (; itk < itk_none; itk += 2 /* skip signed types */)
    {
      tree upper;

      if (integer_types[itk] == NULL_TREE)
	continue;
      upper = TYPE_MAX_VALUE (integer_types[itk]);

      if (wi::geu_p (wi::to_widest (upper), val))
	return (enum integer_type_kind) itk;
    }

  return itk_none;
}

/* Interpret TOKEN, an integer with FLAGS as classified by cpplib.  */
static tree
interpret_integer (const cpp_token *token, unsigned int flags,
		   enum overflow_type *overflow)
{
  tree value, type;
  enum integer_type_kind itk;
  cpp_num integer;
  HOST_WIDE_INT ival[3];

  *overflow = OT_NONE;

  integer = cpp_interpret_integer (parse_in, token, flags);
  if (integer.overflow)
    *overflow = OT_OVERFLOW;

  ival[0] = integer.low;
  ival[1] = integer.high;
  ival[2] = 0;
  widest_int wval = widest_int::from_array (ival, 3);

  /* The type of a constant with a U suffix is straightforward.  */
  if (flags & CPP_N_UNSIGNED)
    itk = narrowest_unsigned_type (wval, flags);
  else
    {
      /* The type of a potentially-signed integer constant varies
	 depending on the base it's in, the standard in use, and the
	 length suffixes.  */
      enum integer_type_kind itk_u
	= narrowest_unsigned_type (wval, flags);
      enum integer_type_kind itk_s
	= narrowest_signed_type (wval, flags);

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
		  warning (0, "this decimal constant is unsigned only in ISO C90");
		}
	      else
		warning (OPT_Wtraditional,
			 "this decimal constant would be unsigned in ISO C90");
	    }
	}
    }

  if (itk == itk_none)
    /* cpplib has already issued a warning for overflow.  */
    type = ((flags & CPP_N_UNSIGNED)
	    ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);
  else
    {
      type = integer_types[itk];
      if (itk > itk_unsigned_long
	  && (flags & CPP_N_WIDTH) != CPP_N_LARGE)
	emit_diagnostic
	  ((c_dialect_cxx () ? cxx_dialect == cxx98 : !flag_isoc99)
	   ? DK_PEDWARN : DK_WARNING,
	   input_location, OPT_Wlong_long,
	   (flags & CPP_N_UNSIGNED)
	   ? "integer constant is too large for %<unsigned long%> type"
	   : "integer constant is too large for %<long%> type");
    }

  value = wide_int_to_tree (type, wval);

  /* Convert imaginary to a complex type.  */
  if (flags & CPP_N_IMAGINARY)
    value = build_complex (NULL_TREE, build_int_cst (type, 0), value);

  return value;
}

/* Interpret TOKEN, a floating point number with FLAGS as classified
   by cpplib.  For C++11 SUFFIX may contain a user-defined literal suffix.  */
static tree
interpret_float (const cpp_token *token, unsigned int flags,
		 const char *suffix, enum overflow_type *overflow)
{
  tree type;
  tree const_type;
  tree value;
  REAL_VALUE_TYPE real;
  REAL_VALUE_TYPE real_trunc;
  char *copy;
  size_t copylen;

  *overflow = OT_NONE;

  /* Default (no suffix) depends on whether the FLOAT_CONST_DECIMAL64
     pragma has been used and is either double or _Decimal64.  Types
     that are not allowed with decimal float default to double.  */
  if (flags & CPP_N_DEFAULT)
    {
      flags ^= CPP_N_DEFAULT;
      flags |= CPP_N_MEDIUM;

      if (((flags & CPP_N_HEX) == 0) && ((flags & CPP_N_IMAGINARY) == 0))
	{
	  warning (OPT_Wunsuffixed_float_constants,
		   "unsuffixed float constant");
	  if (float_const_decimal64_p ())
	    flags |= CPP_N_DFLOAT;
	}
    }

  /* Decode _Fract and _Accum.  */
  if (flags & CPP_N_FRACT || flags & CPP_N_ACCUM)
    return interpret_fixed (token, flags);

  /* Decode type based on width and properties. */
  if (flags & CPP_N_DFLOAT)
    if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
      type = dfloat128_type_node;
    else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
      type = dfloat32_type_node;
    else
      type = dfloat64_type_node;
  else
    if (flags & CPP_N_WIDTH_MD)
      {
	char suffix;
	machine_mode mode;

	if ((flags & CPP_N_WIDTH_MD) == CPP_N_MD_W)
	  suffix = 'w';
	else
	  suffix = 'q';

	mode = targetm.c.mode_for_suffix (suffix);
	if (mode == VOIDmode)
	  {
	    error ("unsupported non-standard suffix on floating constant");

	    return error_mark_node;
	  }
	else
	  pedwarn (input_location, OPT_Wpedantic, "non-standard suffix on floating constant");

	type = c_common_type_for_mode (mode, 0);
	gcc_assert (type);
      }
    else if ((flags & (CPP_N_FLOATN | CPP_N_FLOATNX)) != 0)
      {
	unsigned int n = (flags & CPP_N_WIDTH_FLOATN_NX) >> CPP_FLOATN_SHIFT;
	bool extended = (flags & CPP_N_FLOATNX) != 0;
	type = NULL_TREE;
	for (int i = 0; i < NUM_FLOATN_NX_TYPES; i++)
	  if (floatn_nx_types[i].n == (int) n
	      && floatn_nx_types[i].extended == extended)
	    {
	      type = FLOATN_NX_TYPE_NODE (i);
	      break;
	    }
	if (type == NULL_TREE)
	  {
	    error ("unsupported non-standard suffix on floating constant");
	    return error_mark_node;
	  }
	else
	  pedwarn (input_location, OPT_Wpedantic, "non-standard suffix on floating constant");
      }
    else if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
      type = long_double_type_node;
    else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL
	     || flag_single_precision_constant)
      type = float_type_node;
    else
      type = double_type_node;

  const_type = excess_precision_type (type);
  if (!const_type)
    const_type = type;

  /* Copy the constant to a nul-terminated buffer.  If the constant
     has any suffixes, cut them off; REAL_VALUE_ATOF/ REAL_VALUE_HTOF
     can't handle them.  */
  copylen = token->val.str.len;
  if (flags & CPP_N_USERDEF)
    copylen -= strlen (suffix);
  else if (flags & CPP_N_DFLOAT)
    copylen -= 2;
  else
    {
      if ((flags & CPP_N_WIDTH) != CPP_N_MEDIUM)
	/* Must be an F or L or machine defined suffix.  */
	copylen--;
      if (flags & CPP_N_IMAGINARY)
	/* I or J suffix.  */
	copylen--;
      if (flags & CPP_N_FLOATNX)
	copylen--;
      if (flags & (CPP_N_FLOATN | CPP_N_FLOATNX))
	{
	  unsigned int n = (flags & CPP_N_WIDTH_FLOATN_NX) >> CPP_FLOATN_SHIFT;
	  while (n > 0)
	    {
	      copylen--;
	      n /= 10;
	    }
	}
    }

  copy = (char *) alloca (copylen + 1);
  if (cxx_dialect > cxx11)
    {
      size_t maxlen = 0;
      for (size_t i = 0; i < copylen; ++i)
        if (token->val.str.text[i] != '\'')
          copy[maxlen++] = token->val.str.text[i];
      copy[maxlen] = '\0';
    }
  else
    {
      memcpy (copy, token->val.str.text, copylen);
      copy[copylen] = '\0';
    }

  real_from_string3 (&real, copy, TYPE_MODE (const_type));
  if (const_type != type)
    /* Diagnosing if the result of converting the value with excess
       precision to the semantic type would overflow (with associated
       double rounding) is more appropriate than diagnosing if the
       result of converting the string directly to the semantic type
       would overflow.  */
    real_convert (&real_trunc, TYPE_MODE (type), &real);

  /* Both C and C++ require a diagnostic for a floating constant
     outside the range of representable values of its type.  Since we
     have __builtin_inf* to produce an infinity, this is now a
     mandatory pedwarn if the target does not support infinities.  */
  if (REAL_VALUE_ISINF (real)
      || (const_type != type && REAL_VALUE_ISINF (real_trunc)))
    {
      *overflow = OT_OVERFLOW;
      if (!(flags & CPP_N_USERDEF))
	{
	  if (!MODE_HAS_INFINITIES (TYPE_MODE (type)))
	    pedwarn (input_location, 0,
		     "floating constant exceeds range of %qT", type);
	  else
	    warning (OPT_Woverflow,
		     "floating constant exceeds range of %qT", type);
	}
    }
  /* We also give a warning if the value underflows.  */
  else if (real_equal (&real, &dconst0)
	   || (const_type != type
	       && real_equal (&real_trunc, &dconst0)))
    {
      REAL_VALUE_TYPE realvoidmode;
      int oflow = real_from_string (&realvoidmode, copy);
      *overflow = (oflow == 0 ? OT_NONE
			      : (oflow < 0 ? OT_UNDERFLOW : OT_OVERFLOW));
      if (!(flags & CPP_N_USERDEF))
	{
	  if (oflow < 0 || !real_equal (&realvoidmode, &dconst0))
	    warning (OPT_Woverflow, "floating constant truncated to zero");
	}
    }

  /* Create a node with determined type and value.  */
  value = build_real (const_type, real);
  if (flags & CPP_N_IMAGINARY)
    {
      value = build_complex (NULL_TREE,
			     fold_convert (const_type,
					   integer_zero_node), value);
      if (type != const_type)
	{
	  const_type = TREE_TYPE (value);
	  type = build_complex_type (type);
	}
    }

  if (type != const_type)
    value = build1_loc (token->src_loc, EXCESS_PRECISION_EXPR, type, value);

  return value;
}

/* Interpret TOKEN, a fixed-point number with FLAGS as classified
   by cpplib.  */

static tree
interpret_fixed (const cpp_token *token, unsigned int flags)
{
  tree type;
  tree value;
  FIXED_VALUE_TYPE fixed;
  char *copy;
  size_t copylen;

  copylen = token->val.str.len;

  if (flags & CPP_N_FRACT) /* _Fract.  */
    {
      if (flags & CPP_N_UNSIGNED) /* Unsigned _Fract.  */
	{
	  if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
	    {
	      type = unsigned_long_long_fract_type_node;
	      copylen -= 4;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
	    {
	      type = unsigned_long_fract_type_node;
	      copylen -= 3;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
	    {
	      type = unsigned_short_fract_type_node;
	      copylen -= 3;
	    }
          else
	    {
	      type = unsigned_fract_type_node;
	      copylen -= 2;
	    }
	}
      else /* Signed _Fract.  */
	{
	  if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
	    {
	      type = long_long_fract_type_node;
	      copylen -= 3;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
	    {
	      type = long_fract_type_node;
	      copylen -= 2;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
	    {
	      type = short_fract_type_node;
	      copylen -= 2;
	    }
          else
	    {
	      type = fract_type_node;
	      copylen --;
	    }
	  }
    }
  else /* _Accum.  */
    {
      if (flags & CPP_N_UNSIGNED) /* Unsigned _Accum.  */
	{
	  if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
	    {
	      type = unsigned_long_long_accum_type_node;
	      copylen -= 4;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
	    {
	      type = unsigned_long_accum_type_node;
	      copylen -= 3;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
	    {
	      type = unsigned_short_accum_type_node;
	      copylen -= 3;
	     }
	  else
	    {
	      type = unsigned_accum_type_node;
	      copylen -= 2;
	    }
	}
      else /* Signed _Accum.  */
        {
	  if ((flags & CPP_N_WIDTH) == CPP_N_LARGE)
	    {
	      type = long_long_accum_type_node;
	      copylen -= 3;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_MEDIUM)
	    {
	      type = long_accum_type_node;
	      copylen -= 2;
	    }
	  else if ((flags & CPP_N_WIDTH) == CPP_N_SMALL)
	    {
	      type = short_accum_type_node;
	      copylen -= 2;
	    }
	  else
	    {
	      type = accum_type_node;
	      copylen --;
	    }
	}
    }

  copy = (char *) alloca (copylen + 1);
  memcpy (copy, token->val.str.text, copylen);
  copy[copylen] = '\0';

  fixed_from_string (&fixed, copy, SCALAR_TYPE_MODE (type));

  /* Create a node with determined type and value.  */
  value = build_fixed (type, fixed);

  return value;
}

/* Convert a series of STRING, WSTRING, STRING16, STRING32 and/or
   UTF8STRING tokens into a tree, performing string constant
   concatenation.  TOK is the first of these.  VALP is the location to
   write the string into.  OBJC_STRING indicates whether an '@' token
   preceded the incoming token (in that case, the strings can either
   be ObjC strings, preceded by a single '@', or normal strings, not
   preceded by '@'.  The result will be a CPP_OBJC_STRING).  Returns
   the CPP token type of the result (CPP_STRING, CPP_WSTRING,
   CPP_STRING32, CPP_STRING16, CPP_UTF8STRING, or CPP_OBJC_STRING).

   This is unfortunately more work than it should be.  If any of the
   strings in the series has an L prefix, the result is a wide string
   (6.4.5p4).  Whether or not the result is a wide string affects the
   meaning of octal and hexadecimal escapes (6.4.4.4p6,9).  But escape
   sequences do not continue across the boundary between two strings in
   a series (6.4.5p7), so we must not lose the boundaries.  Therefore
   cpp_interpret_string takes a vector of cpp_string structures, which
   we must arrange to provide.  */

static enum cpp_ttype
lex_string (const cpp_token *tok, tree *valp, bool objc_string, bool translate)
{
  tree value;
  size_t concats = 0;
  struct obstack str_ob;
  struct obstack loc_ob;
  cpp_string istr;
  enum cpp_ttype type = tok->type;

  /* Try to avoid the overhead of creating and destroying an obstack
     for the common case of just one string.  */
  cpp_string str = tok->val.str;
  location_t init_loc = tok->src_loc;
  cpp_string *strs = &str;
  location_t *locs = NULL;

  /* objc_at_sign_was_seen is only used when doing Objective-C string
     concatenation.  It is 'true' if we have seen an '@' before the
     current string, and 'false' if not.  We must see exactly one or
     zero '@' before each string.  */
  bool objc_at_sign_was_seen = false;

 retry:
  tok = cpp_get_token (parse_in);
  switch (tok->type)
    {
    case CPP_PADDING:
      goto retry;
    case CPP_ATSIGN:
      if (objc_string)
	{
	  if (objc_at_sign_was_seen)
	    error ("repeated %<@%> before Objective-C string");

	  objc_at_sign_was_seen = true;
	  goto retry;
	}
      /* FALLTHROUGH */

    default:
      break;

    case CPP_WSTRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_UTF8STRING:
      if (type != tok->type)
	{
	  if (type == CPP_STRING)
	    type = tok->type;
	  else
	    error ("unsupported non-standard concatenation of string literals");
	}
      /* FALLTHROUGH */

    case CPP_STRING:
      if (!concats)
	{
	  gcc_obstack_init (&str_ob);
	  gcc_obstack_init (&loc_ob);
	  obstack_grow (&str_ob, &str, sizeof (cpp_string));
	  obstack_grow (&loc_ob, &init_loc, sizeof (location_t));
	}

      concats++;
      obstack_grow (&str_ob, &tok->val.str, sizeof (cpp_string));
      obstack_grow (&loc_ob, &tok->src_loc, sizeof (location_t));

      if (objc_string)
	objc_at_sign_was_seen = false;
      goto retry;
    }

  /* It is an error if we saw a '@' with no following string.  */
  if (objc_at_sign_was_seen)
    error ("stray %<@%> in program");

  /* We have read one more token than we want.  */
  _cpp_backup_tokens (parse_in, 1);
  if (concats)
    {
      strs = XOBFINISH (&str_ob, cpp_string *);
      locs = XOBFINISH (&loc_ob, location_t *);
    }

  if (concats && !objc_string && !in_system_header_at (input_location))
    warning (OPT_Wtraditional,
	     "traditional C rejects string constant concatenation");

  if ((translate
       ? cpp_interpret_string : cpp_interpret_string_notranslate)
      (parse_in, strs, concats + 1, &istr, type))
    {
      value = build_string (istr.len, (const char *) istr.text);
      free (CONST_CAST (unsigned char *, istr.text));
      if (concats)
	{
	  gcc_assert (locs);
	  gcc_assert (g_string_concat_db);
	  g_string_concat_db->record_string_concatenation (concats + 1, locs);
	}
    }
  else
    {
      /* Callers cannot generally handle error_mark_node in this context,
	 so return the empty string instead.  cpp_interpret_string has
	 issued an error.  */
      switch (type)
	{
	default:
	case CPP_STRING:
	case CPP_UTF8STRING:
	  value = build_string (1, "");
	  break;
	case CPP_STRING16:
	  value = build_string (TYPE_PRECISION (char16_type_node)
				/ TYPE_PRECISION (char_type_node),
				"\0");  /* char16_t is 16 bits */
	  break;
	case CPP_STRING32:
	  value = build_string (TYPE_PRECISION (char32_type_node)
				/ TYPE_PRECISION (char_type_node),
				"\0\0\0");  /* char32_t is 32 bits */
	  break;
	case CPP_WSTRING:
	  value = build_string (TYPE_PRECISION (wchar_type_node)
				/ TYPE_PRECISION (char_type_node),
				"\0\0\0");  /* widest supported wchar_t
					       is 32 bits */
	  break;
        }
    }

  switch (type)
    {
    default:
    case CPP_STRING:
    case CPP_UTF8STRING:
      TREE_TYPE (value) = char_array_type_node;
      break;
    case CPP_STRING16:
      TREE_TYPE (value) = char16_array_type_node;
      break;
    case CPP_STRING32:
      TREE_TYPE (value) = char32_array_type_node;
      break;
    case CPP_WSTRING:
      TREE_TYPE (value) = wchar_array_type_node;
    }
  *valp = fix_string_type (value);

  if (concats)
    {
      obstack_free (&str_ob, 0);
      obstack_free (&loc_ob, 0);
    }

  return objc_string ? CPP_OBJC_STRING : type;
}

/* Converts a (possibly wide) character constant token into a tree.  */
static tree
lex_charconst (const cpp_token *token)
{
  cppchar_t result;
  tree type, value;
  unsigned int chars_seen;
  int unsignedp = 0;

  result = cpp_interpret_charconst (parse_in, token,
				    &chars_seen, &unsignedp);

  if (token->type == CPP_WCHAR)
    type = wchar_type_node;
  else if (token->type == CPP_CHAR32)
    type = char32_type_node;
  else if (token->type == CPP_CHAR16)
    type = char16_type_node;
  else if (token->type == CPP_UTF8CHAR)
    type = char_type_node;
  /* In C, a character constant has type 'int'.
     In C++ 'char', but multi-char charconsts have type 'int'.  */
  else if (!c_dialect_cxx () || chars_seen > 1)
    type = integer_type_node;
  else
    type = char_type_node;

  /* Cast to cppchar_signed_t to get correct sign-extension of RESULT
     before possibly widening to HOST_WIDE_INT for build_int_cst.  */
  if (unsignedp || (cppchar_signed_t) result >= 0)
    value = build_int_cst (type, result);
  else
    value = build_int_cst (type, (cppchar_signed_t) result);

  return value;
}

/* Helper function for c_parser_peek_conflict_marker
   and cp_lexer_peek_conflict_marker.
   Given a possible conflict marker token of kind TOK1_KIND
   consisting of a pair of characters, get the token kind for the
   standalone final character.  */

enum cpp_ttype
conflict_marker_get_final_tok_kind (enum cpp_ttype tok1_kind)
{
  switch (tok1_kind)
    {
    default: gcc_unreachable ();
    case CPP_LSHIFT:
      /* "<<" and '<' */
      return CPP_LESS;

    case CPP_EQ_EQ:
      /* "==" and '=' */
      return CPP_EQ;

    case CPP_RSHIFT:
      /* ">>" and '>' */
      return CPP_GREATER;
    }
}
