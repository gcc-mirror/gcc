/* Mainly the interface between cpplib and the C front ends.
   Copyright (C) 1987, 1988, 1989, 1992, 1994, 1995, 1996, 1997
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif /* MULTIBYTE_CHARS */

/* The current line map.  */
static const struct line_map *map;

/* The line used to refresh the lineno global variable after each token.  */
static unsigned int src_lineno;

/* We may keep statistics about how long which files took to compile.  */
static int header_time, body_time;
static splay_tree file_info_tree;

/* File used for outputting assembler code.  */
extern FILE *asm_out_file;

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE TYPE_PRECISION (wchar_type_node)

/* Number of bytes in a wide character.  */
#define WCHAR_BYTES (WCHAR_TYPE_SIZE / BITS_PER_UNIT)

int pending_lang_change; /* If we need to switch languages - C++ only */
int c_header_level;	 /* depth in C headers - C++ only */

/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag;

static tree interpret_integer	PARAMS ((const cpp_token *, unsigned int));
static tree interpret_float	PARAMS ((const cpp_token *, unsigned int));
static enum integer_type_kind
  narrowest_unsigned_type	PARAMS ((tree, unsigned int));
static enum integer_type_kind
  narrowest_signed_type		PARAMS ((tree, unsigned int));
static tree lex_string		PARAMS ((const unsigned char *, unsigned int,
					 int));
static tree lex_charconst	PARAMS ((const cpp_token *));
static void update_header_times	PARAMS ((const char *));
static int dump_one_header	PARAMS ((splay_tree_node, void *));
static void cb_line_change     PARAMS ((cpp_reader *, const cpp_token *, int));
static void cb_ident		PARAMS ((cpp_reader *, unsigned int,
					 const cpp_string *));
static void cb_file_change    PARAMS ((cpp_reader *, const struct line_map *));
static void cb_def_pragma	PARAMS ((cpp_reader *, unsigned int));
static void cb_define		PARAMS ((cpp_reader *, unsigned int,
					 cpp_hashnode *));
static void cb_undef		PARAMS ((cpp_reader *, unsigned int,
					 cpp_hashnode *));

const char *
init_c_lex (filename)
     const char *filename;
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
  
#ifdef MULTIBYTE_CHARS
  /* Change to the native locale for multibyte conversions.  */
  setlocale (LC_CTYPE, "");
  GET_ENVIRONMENT (literal_codeset, "LANG");
#endif

  cb = cpp_get_callbacks (parse_in);

  cb->line_change = cb_line_change;
  cb->ident = cb_ident;
  cb->file_change = cb_file_change;
  cb->def_pragma = cb_def_pragma;

  /* Set the debug callbacks if we can use them.  */
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && (write_symbols == DWARF_DEBUG || write_symbols == DWARF2_DEBUG
          || write_symbols == VMS_AND_DWARF2_DEBUG))
    {
      cb->define = cb_define;
      cb->undef = cb_undef;
    }

  /* Start it at 0.  */
  lineno = 0;

  return cpp_read_main_file (parse_in, filename, ident_hash);
}

/* A thin wrapper around the real parser that initializes the 
   integrated preprocessor after debug output has been initialized.
   Also, make sure the start_source_file debug hook gets called for
   the primary source file.  */

void
c_common_parse_file (set_yydebug)
     int set_yydebug ATTRIBUTE_UNUSED;
{
#if YYDEBUG != 0
  yydebug = set_yydebug;
#else
  warning ("YYDEBUG not defined");
#endif

  (*debug_hooks->start_source_file) (lineno, input_filename);
  cpp_finish_options (parse_in);

  yyparse ();
  free_parser_stacks ();
}

struct c_fileinfo *
get_fileinfo (name)
     const char *name;
{
  splay_tree_node n;
  struct c_fileinfo *fi;

  n = splay_tree_lookup (file_info_tree, (splay_tree_key) name);
  if (n)
    return (struct c_fileinfo *) n->value;

  fi = (struct c_fileinfo *) xmalloc (sizeof (struct c_fileinfo));
  fi->time = 0;
  fi->interface_only = 0;
  fi->interface_unknown = 1;
  splay_tree_insert (file_info_tree, (splay_tree_key) name,
		     (splay_tree_value) fi);
  return fi;
}

static void
update_header_times (name)
     const char *name;
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
dump_one_header (n, dummy)
     splay_tree_node n;
     void *dummy ATTRIBUTE_UNUSED;
{
  print_time ((const char *) n->key,
	      ((struct c_fileinfo *) n->value)->time);
  return 0;
}

void
dump_time_statistics ()
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
cb_ident (pfile, line, str)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     unsigned int line ATTRIBUTE_UNUSED;
     const cpp_string *str ATTRIBUTE_UNUSED;
{
#ifdef ASM_OUTPUT_IDENT
  if (! flag_no_ident)
    {
      /* Convert escapes in the string.  */
      tree value = lex_string (str->text, str->len, 0);
      ASM_OUTPUT_IDENT (asm_out_file, TREE_STRING_POINTER (value));
    }
#endif
}

/* Called at the start of every non-empty line.  TOKEN is the first
   lexed token on the line.  Used for diagnostic line numbers.  */
static void
cb_line_change (pfile, token, parsing_args)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const cpp_token *token;
     int parsing_args ATTRIBUTE_UNUSED;
{
  src_lineno = SOURCE_LINE (map, token->line);
}

static void
cb_file_change (pfile, new_map)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const struct line_map *new_map;
{
  unsigned int to_line = SOURCE_LINE (new_map, new_map->to_line);

  if (new_map->reason == LC_ENTER)
    {
      /* Don't stack the main buffer on the input stack;
	 we already did in compile_file.  */
      if (map == NULL)
	main_input_filename = new_map->to_file;
      else
	{
          int included_at = SOURCE_LINE (new_map - 1, new_map->from_line - 1);

	  lineno = included_at;
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
      
      (*debug_hooks->end_source_file) (to_line);
    }

  update_header_times (new_map->to_file);
  in_system_header = new_map->sysp != 0;
  input_filename = new_map->to_file;
  lineno = to_line;
  map = new_map;

  /* Hook for C++.  */
  extract_interface_info ();
}

static void
cb_def_pragma (pfile, line)
     cpp_reader *pfile;
     unsigned int line;
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

      lineno = SOURCE_LINE (map, line);
      warning ("ignoring #pragma %s %s", space, name);
    }
}

/* #define callback for DWARF and DWARF2 debug info.  */
static void
cb_define (pfile, line, node)
     cpp_reader *pfile;
     unsigned int line;
     cpp_hashnode *node;
{
  (*debug_hooks->define) (SOURCE_LINE (map, line),
			  (const char *) cpp_macro_definition (pfile, node));
}

/* #undef callback for DWARF and DWARF2 debug info.  */
static void
cb_undef (pfile, line, node)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     unsigned int line;
     cpp_hashnode *node;
{
  (*debug_hooks->undef) (SOURCE_LINE (map, line),
			 (const char *) NODE_NAME (node));
}

#if 0 /* not yet */
/* Returns nonzero if C is a universal-character-name.  Give an error if it
   is not one which may appear in an identifier, as per [extendid].

   Note that extended character support in identifiers has not yet been
   implemented.  It is my personal opinion that this is not a desirable
   feature.  Portable code cannot count on support for more than the basic
   identifier character set.  */

static inline int
is_extended_char (c)
     int c;
{
#ifdef TARGET_EBCDIC
  return 0;
#else
  /* ASCII.  */
  if (c < 0x7f)
    return 0;

  /* None of the valid chars are outside the Basic Multilingual Plane (the
     low 16 bits).  */
  if (c > 0xffff)
    {
      error ("universal-character-name '\\U%08x' not valid in identifier", c);
      return 1;
    }
  
  /* Latin */
  if ((c >= 0x00c0 && c <= 0x00d6)
      || (c >= 0x00d8 && c <= 0x00f6)
      || (c >= 0x00f8 && c <= 0x01f5)
      || (c >= 0x01fa && c <= 0x0217)
      || (c >= 0x0250 && c <= 0x02a8)
      || (c >= 0x1e00 && c <= 0x1e9a)
      || (c >= 0x1ea0 && c <= 0x1ef9))
    return 1;

  /* Greek */
  if ((c == 0x0384)
      || (c >= 0x0388 && c <= 0x038a)
      || (c == 0x038c)
      || (c >= 0x038e && c <= 0x03a1)
      || (c >= 0x03a3 && c <= 0x03ce)
      || (c >= 0x03d0 && c <= 0x03d6)
      || (c == 0x03da)
      || (c == 0x03dc)
      || (c == 0x03de)
      || (c == 0x03e0)
      || (c >= 0x03e2 && c <= 0x03f3)
      || (c >= 0x1f00 && c <= 0x1f15)
      || (c >= 0x1f18 && c <= 0x1f1d)
      || (c >= 0x1f20 && c <= 0x1f45)
      || (c >= 0x1f48 && c <= 0x1f4d)
      || (c >= 0x1f50 && c <= 0x1f57)
      || (c == 0x1f59)
      || (c == 0x1f5b)
      || (c == 0x1f5d)
      || (c >= 0x1f5f && c <= 0x1f7d)
      || (c >= 0x1f80 && c <= 0x1fb4)
      || (c >= 0x1fb6 && c <= 0x1fbc)
      || (c >= 0x1fc2 && c <= 0x1fc4)
      || (c >= 0x1fc6 && c <= 0x1fcc)
      || (c >= 0x1fd0 && c <= 0x1fd3)
      || (c >= 0x1fd6 && c <= 0x1fdb)
      || (c >= 0x1fe0 && c <= 0x1fec)
      || (c >= 0x1ff2 && c <= 0x1ff4)
      || (c >= 0x1ff6 && c <= 0x1ffc))
    return 1;

  /* Cyrillic */
  if ((c >= 0x0401 && c <= 0x040d)
      || (c >= 0x040f && c <= 0x044f)
      || (c >= 0x0451 && c <= 0x045c)
      || (c >= 0x045e && c <= 0x0481)
      || (c >= 0x0490 && c <= 0x04c4)
      || (c >= 0x04c7 && c <= 0x04c8)
      || (c >= 0x04cb && c <= 0x04cc)
      || (c >= 0x04d0 && c <= 0x04eb)
      || (c >= 0x04ee && c <= 0x04f5)
      || (c >= 0x04f8 && c <= 0x04f9))
    return 1;

  /* Armenian */
  if ((c >= 0x0531 && c <= 0x0556)
      || (c >= 0x0561 && c <= 0x0587))
    return 1;

  /* Hebrew */
  if ((c >= 0x05d0 && c <= 0x05ea)
      || (c >= 0x05f0 && c <= 0x05f4))
    return 1;

  /* Arabic */
  if ((c >= 0x0621 && c <= 0x063a)
      || (c >= 0x0640 && c <= 0x0652)
      || (c >= 0x0670 && c <= 0x06b7)
      || (c >= 0x06ba && c <= 0x06be)
      || (c >= 0x06c0 && c <= 0x06ce)
      || (c >= 0x06e5 && c <= 0x06e7))
    return 1;

  /* Devanagari */
  if ((c >= 0x0905 && c <= 0x0939)
      || (c >= 0x0958 && c <= 0x0962))
    return 1;

  /* Bengali */
  if ((c >= 0x0985 && c <= 0x098c)
      || (c >= 0x098f && c <= 0x0990)
      || (c >= 0x0993 && c <= 0x09a8)
      || (c >= 0x09aa && c <= 0x09b0)
      || (c == 0x09b2)
      || (c >= 0x09b6 && c <= 0x09b9)
      || (c >= 0x09dc && c <= 0x09dd)
      || (c >= 0x09df && c <= 0x09e1)
      || (c >= 0x09f0 && c <= 0x09f1))
    return 1;

  /* Gurmukhi */
  if ((c >= 0x0a05 && c <= 0x0a0a)
      || (c >= 0x0a0f && c <= 0x0a10)
      || (c >= 0x0a13 && c <= 0x0a28)
      || (c >= 0x0a2a && c <= 0x0a30)
      || (c >= 0x0a32 && c <= 0x0a33)
      || (c >= 0x0a35 && c <= 0x0a36)
      || (c >= 0x0a38 && c <= 0x0a39)
      || (c >= 0x0a59 && c <= 0x0a5c)
      || (c == 0x0a5e))
    return 1;

  /* Gujarati */
  if ((c >= 0x0a85 && c <= 0x0a8b)
      || (c == 0x0a8d)
      || (c >= 0x0a8f && c <= 0x0a91)
      || (c >= 0x0a93 && c <= 0x0aa8)
      || (c >= 0x0aaa && c <= 0x0ab0)
      || (c >= 0x0ab2 && c <= 0x0ab3)
      || (c >= 0x0ab5 && c <= 0x0ab9)
      || (c == 0x0ae0))
    return 1;

  /* Oriya */
  if ((c >= 0x0b05 && c <= 0x0b0c)
      || (c >= 0x0b0f && c <= 0x0b10)
      || (c >= 0x0b13 && c <= 0x0b28)
      || (c >= 0x0b2a && c <= 0x0b30)
      || (c >= 0x0b32 && c <= 0x0b33)
      || (c >= 0x0b36 && c <= 0x0b39)
      || (c >= 0x0b5c && c <= 0x0b5d)
      || (c >= 0x0b5f && c <= 0x0b61))
    return 1;

  /* Tamil */
  if ((c >= 0x0b85 && c <= 0x0b8a)
      || (c >= 0x0b8e && c <= 0x0b90)
      || (c >= 0x0b92 && c <= 0x0b95)
      || (c >= 0x0b99 && c <= 0x0b9a)
      || (c == 0x0b9c)
      || (c >= 0x0b9e && c <= 0x0b9f)
      || (c >= 0x0ba3 && c <= 0x0ba4)
      || (c >= 0x0ba8 && c <= 0x0baa)
      || (c >= 0x0bae && c <= 0x0bb5)
      || (c >= 0x0bb7 && c <= 0x0bb9))
    return 1;

  /* Telugu */
  if ((c >= 0x0c05 && c <= 0x0c0c)
      || (c >= 0x0c0e && c <= 0x0c10)
      || (c >= 0x0c12 && c <= 0x0c28)
      || (c >= 0x0c2a && c <= 0x0c33)
      || (c >= 0x0c35 && c <= 0x0c39)
      || (c >= 0x0c60 && c <= 0x0c61))
    return 1;

  /* Kannada */
  if ((c >= 0x0c85 && c <= 0x0c8c)
      || (c >= 0x0c8e && c <= 0x0c90)
      || (c >= 0x0c92 && c <= 0x0ca8)
      || (c >= 0x0caa && c <= 0x0cb3)
      || (c >= 0x0cb5 && c <= 0x0cb9)
      || (c >= 0x0ce0 && c <= 0x0ce1))
    return 1;

  /* Malayalam */
  if ((c >= 0x0d05 && c <= 0x0d0c)
      || (c >= 0x0d0e && c <= 0x0d10)
      || (c >= 0x0d12 && c <= 0x0d28)
      || (c >= 0x0d2a && c <= 0x0d39)
      || (c >= 0x0d60 && c <= 0x0d61))
    return 1;

  /* Thai */
  if ((c >= 0x0e01 && c <= 0x0e30)
      || (c >= 0x0e32 && c <= 0x0e33)
      || (c >= 0x0e40 && c <= 0x0e46)
      || (c >= 0x0e4f && c <= 0x0e5b))
    return 1;

  /* Lao */
  if ((c >= 0x0e81 && c <= 0x0e82)
      || (c == 0x0e84)
      || (c == 0x0e87)
      || (c == 0x0e88)
      || (c == 0x0e8a)
      || (c == 0x0e0d)
      || (c >= 0x0e94 && c <= 0x0e97)
      || (c >= 0x0e99 && c <= 0x0e9f)
      || (c >= 0x0ea1 && c <= 0x0ea3)
      || (c == 0x0ea5)
      || (c == 0x0ea7)
      || (c == 0x0eaa)
      || (c == 0x0eab)
      || (c >= 0x0ead && c <= 0x0eb0)
      || (c == 0x0eb2)
      || (c == 0x0eb3)
      || (c == 0x0ebd)
      || (c >= 0x0ec0 && c <= 0x0ec4)
      || (c == 0x0ec6))
    return 1;

  /* Georgian */
  if ((c >= 0x10a0 && c <= 0x10c5)
      || (c >= 0x10d0 && c <= 0x10f6))
    return 1;

  /* Hiragana */
  if ((c >= 0x3041 && c <= 0x3094)
      || (c >= 0x309b && c <= 0x309e))
    return 1;

  /* Katakana */
  if ((c >= 0x30a1 && c <= 0x30fe))
    return 1;

  /* Bopmofo */
  if ((c >= 0x3105 && c <= 0x312c))
    return 1;

  /* Hangul */
  if ((c >= 0x1100 && c <= 0x1159)
      || (c >= 0x1161 && c <= 0x11a2)
      || (c >= 0x11a8 && c <= 0x11f9))
    return 1;

  /* CJK Unified Ideographs */
  if ((c >= 0xf900 && c <= 0xfa2d)
      || (c >= 0xfb1f && c <= 0xfb36)
      || (c >= 0xfb38 && c <= 0xfb3c)
      || (c == 0xfb3e)
      || (c >= 0xfb40 && c <= 0xfb41)
      || (c >= 0xfb42 && c <= 0xfb44)
      || (c >= 0xfb46 && c <= 0xfbb1)
      || (c >= 0xfbd3 && c <= 0xfd3f)
      || (c >= 0xfd50 && c <= 0xfd8f)
      || (c >= 0xfd92 && c <= 0xfdc7)
      || (c >= 0xfdf0 && c <= 0xfdfb)
      || (c >= 0xfe70 && c <= 0xfe72)
      || (c == 0xfe74)
      || (c >= 0xfe76 && c <= 0xfefc)
      || (c >= 0xff21 && c <= 0xff3a)
      || (c >= 0xff41 && c <= 0xff5a)
      || (c >= 0xff66 && c <= 0xffbe)
      || (c >= 0xffc2 && c <= 0xffc7)
      || (c >= 0xffca && c <= 0xffcf)
      || (c >= 0xffd2 && c <= 0xffd7)
      || (c >= 0xffda && c <= 0xffdc)
      || (c >= 0x4e00 && c <= 0x9fa5))
    return 1;

  error ("universal-character-name '\\u%04x' not valid in identifier", c);
  return 1;
#endif
}

/* Add the UTF-8 representation of C to the token_buffer.  */

static void
utf8_extend_token (c)
     int c;
{
  int shift, mask;

  if      (c <= 0x0000007f)
    {
      extend_token (c);
      return;
    }
  else if (c <= 0x000007ff)
    shift = 6, mask = 0xc0;
  else if (c <= 0x0000ffff)
    shift = 12, mask = 0xe0;
  else if (c <= 0x001fffff)
    shift = 18, mask = 0xf0;
  else if (c <= 0x03ffffff)
    shift = 24, mask = 0xf8;
  else
    shift = 30, mask = 0xfc;

  extend_token (mask | (c >> shift));
  do
    {
      shift -= 6;
      extend_token ((unsigned char) (0x80 | (c >> shift)));
    }
  while (shift);
}
#endif

int
c_lex (value)
     tree *value;
{
  const cpp_token *tok;

  retry:
  timevar_push (TV_CPP);
  do
    tok = cpp_get_token (parse_in);
  while (tok->type == CPP_PADDING);
  timevar_pop (TV_CPP);

  /* The C++ front end does horrible things with the current line
     number.  To ensure an accurate line number, we must reset it
     every time we return a token.  */
  lineno = src_lineno;

  *value = NULL_TREE;
  switch (tok->type)
    {
    /* Issue this error here, where we can get at tok->val.c.  */
    case CPP_OTHER:
      if (ISGRAPH (tok->val.c))
	error ("stray '%c' in program", tok->val.c);
      else
	error ("stray '\\%o' in program", tok->val.c);
      goto retry;
      
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

    case CPP_CHAR:
    case CPP_WCHAR:
      *value = lex_charconst (tok);
      break;

    case CPP_STRING:
    case CPP_WSTRING:
      *value = lex_string (tok->val.str.text, tok->val.str.len,
			   tok->type == CPP_WSTRING);
      break;

      /* These tokens should not be visible outside cpplib.  */
    case CPP_HEADER_NAME:
    case CPP_COMMENT:
    case CPP_MACRO_ARG:
      abort ();

    default: break;
    }

  return tok->type;
}

/* Returns the narrowest C-visible unsigned type, starting with the
   minimum specified by FLAGS, that can fit VALUE, or itk_none if
   there isn't one.  */
static enum integer_type_kind
narrowest_unsigned_type (value, flags)
     tree value;
     unsigned int flags;
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
narrowest_signed_type (value, flags)
     tree value;
     unsigned int flags;
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
interpret_integer (token, flags)
     const cpp_token *token;
     unsigned int flags;
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
interpret_float (token, flags)
     const cpp_token *token;
     unsigned int flags;
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

static tree
lex_string (str, len, wide)
     const unsigned char *str;
     unsigned int len;
     int wide;
{
  tree value;
  char *buf = alloca ((len + 1) * (wide ? WCHAR_BYTES : 1));
  char *q = buf;
  const unsigned char *p = str, *limit = str + len;
  cppchar_t c;

#ifdef MULTIBYTE_CHARS
  /* Reset multibyte conversion state.  */
  (void) local_mbtowc (NULL, NULL, 0);
#endif

  while (p < limit)
    {
#ifdef MULTIBYTE_CHARS
      wchar_t wc;
      int char_len;

      char_len = local_mbtowc (&wc, (const char *) p, limit - p);
      if (char_len == -1)
	{
	  warning ("ignoring invalid multibyte character");
	  char_len = 1;
	  c = *p++;
	}
      else
	{
	  p += char_len;
	  c = wc;
	}
#else
      c = *p++;
#endif

      if (c == '\\' && !ignore_escape_flag)
	c = cpp_parse_escape (parse_in, &p, limit, wide);
	
      /* Add this single character into the buffer either as a wchar_t,
	 a multibyte sequence, or as a single byte.  */
      if (wide)
	{
	  unsigned charwidth = TYPE_PRECISION (char_type_node);
	  unsigned bytemask = (1 << charwidth) - 1;
	  int byte;

	  for (byte = 0; byte < WCHAR_BYTES; ++byte)
	    {
	      int n;
	      if (byte >= (int) sizeof (c))
		n = 0;
	      else
		n = (c >> (byte * charwidth)) & bytemask;
	      if (BYTES_BIG_ENDIAN)
		q[WCHAR_BYTES - byte - 1] = n;
	      else
		q[byte] = n;
	    }
	  q += WCHAR_BYTES;
	}
#ifdef MULTIBYTE_CHARS
      else if (char_len > 1)
	{
	  /* We're dealing with a multibyte character.  */
	  for ( ; char_len >0; --char_len)
	    {
	      *q++ = *(p - char_len);
	    }
	}
#endif
      else
	{
	  *q++ = c;
	}
    }

  /* Terminate the string value, either with a single byte zero
     or with a wide zero.  */

  if (wide)
    {
      memset (q, 0, WCHAR_BYTES);
      q += WCHAR_BYTES;
    }
  else
    {
      *q++ = '\0';
    }

  value = build_string (q - buf, buf);

  if (wide)
    TREE_TYPE (value) = wchar_array_type_node;
  else
    TREE_TYPE (value) = char_array_type_node;
  return value;
}

/* Converts a (possibly wide) character constant token into a tree.  */
static tree
lex_charconst (token)
     const cpp_token *token;
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
  else if ((c_language == clk_c) || chars_seen > 1)
    type = integer_type_node;
  else
    type = char_type_node;

  TREE_TYPE (value) = type;
  return value;
}
