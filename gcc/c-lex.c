/* Lexical analyzer for C and Objective C.
   Copyright (C) 1987, 1988, 1989, 1992, 1994, 1995, 1996, 1997
   1998, 1999, 2000 Free Software Foundation, Inc.

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

#include "rtl.h"
#include "tree.h"
#include "expr.h"
#include "input.h"
#include "output.h"
#include "c-lex.h"
#include "c-tree.h"
#include "flags.h"
#include "timevar.h"
#include "cpplib.h"
#include "c-pragma.h"
#include "toplev.h"
#include "intl.h"
#include "tm_p.h"
#include "splay-tree.h"
#include "debug.h"

/* MULTIBYTE_CHARS support only works for native compilers.
   ??? Ideally what we want is to model widechar support after
   the current floating point support.  */
#ifdef CROSS_COMPILE
#undef MULTIBYTE_CHARS
#endif

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif /* MULTIBYTE_CHARS */
#ifndef GET_ENVIRONMENT
#define GET_ENVIRONMENT(ENV_VALUE,ENV_NAME) ((ENV_VALUE) = getenv (ENV_NAME))
#endif

/* The current line map.  */
static const struct line_map *map;

/* The line used to refresh the lineno global variable after each token.  */
static unsigned int src_lineno;

/* We may keep statistics about how long which files took to compile.  */
static int header_time, body_time;
static splay_tree file_info_tree;

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

/* File used for outputting assembler code.  */
extern FILE *asm_out_file;

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE TYPE_PRECISION (wchar_type_node)

/* Number of bytes in a wide character.  */
#define WCHAR_BYTES (WCHAR_TYPE_SIZE / BITS_PER_UNIT)

int indent_level;        /* Number of { minus number of }.  */
int pending_lang_change; /* If we need to switch languages - C++ only */
int c_header_level;	 /* depth in C headers - C++ only */

/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag;

static void parse_float		PARAMS ((PTR));
static tree lex_number		PARAMS ((const char *, unsigned int));
static tree lex_string		PARAMS ((const char *, unsigned int, int));
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

  if (filename == NULL || !strcmp (filename, "-"))
    filename = "";

  return cpp_read_main_file (parse_in, filename, ident_hash);
}

/* A thin wrapper around the real parser that initializes the 
   integrated preprocessor after debug output has been initialized.
   Also, make sure the start_source_file debug hook gets called for
   the primary source file.  */

int
yyparse()
{
  (*debug_hooks->start_source_file) (lineno, input_filename);
  cpp_finish_options (parse_in);

  return yyparse_1();
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

/* Not yet handled: #pragma, #define, #undef.
   No need to deal with linemarkers under normal conditions.  */

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
      tree value = lex_string ((const char *)str->text, str->len, 0);
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
	  lineno = SOURCE_LINE (new_map - 1, new_map->from_line - 1);
	  push_srcloc (new_map->to_file, 1);
	  input_file_stack->indent_level = indent_level;
	  (*debug_hooks->start_source_file) (lineno, new_map->to_file);
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
#if 0
      if (indent_level != input_file_stack->indent_level)
	{
	  warning_with_file_and_line
	    (input_filename, lineno,
	     "this file contains more '%c's than '%c's",
	     indent_level > input_file_stack->indent_level ? '{' : '}',
	     indent_level > input_file_stack->indent_level ? '}' : '{');
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
      const unsigned char *space, *name = 0;
      const cpp_token *s;

      s = cpp_get_token (pfile);
      space = cpp_token_as_text (pfile, s);
      s = cpp_get_token (pfile);
      if (s->type == CPP_NAME)
	name = cpp_token_as_text (pfile, s);

      lineno = SOURCE_LINE (map, line);
      if (name)
	warning ("ignoring #pragma %s %s", space, name);
      else
	warning ("ignoring #pragma %s", space);
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

#if 0
struct try_type
{
  tree *const node_var;
  const char unsigned_flag;
  const char long_flag;
  const char long_long_flag;
};

struct try_type type_sequence[] =
{
  { &integer_type_node, 0, 0, 0},
  { &unsigned_type_node, 1, 0, 0},
  { &long_integer_type_node, 0, 1, 0},
  { &long_unsigned_type_node, 1, 1, 0},
  { &long_long_integer_type_node, 0, 1, 1},
  { &long_long_unsigned_type_node, 1, 1, 1}
};
#endif /* 0 */

struct pf_args
{
  /* Input */
  const char *str;
  int fflag;
  int lflag;
  int base;
  /* Output */
  int conversion_errno;
  REAL_VALUE_TYPE value;
  tree type;
};
 
static void
parse_float (data)
  PTR data;
{
  struct pf_args * args = (struct pf_args *) data;
  const char *typename;

  args->conversion_errno = 0;
  args->type = double_type_node;
  typename = "double";

  /* The second argument, machine_mode, of REAL_VALUE_ATOF
     tells the desired precision of the binary result
     of decimal-to-binary conversion.  */

  if (args->fflag)
    {
      if (args->lflag)
	error ("both 'f' and 'l' suffixes on floating constant");

      args->type = float_type_node;
      typename = "float";
    }
  else if (args->lflag)
    {
      args->type = long_double_type_node;
      typename = "long double";
    }
  else if (flag_single_precision_constant)
    {
      args->type = float_type_node;
      typename = "float";
    }

  errno = 0;
  if (args->base == 16)
    args->value = REAL_VALUE_HTOF (args->str, TYPE_MODE (args->type));
  else
    args->value = REAL_VALUE_ATOF (args->str, TYPE_MODE (args->type));

  args->conversion_errno = errno;
  /* A diagnostic is required here by some ISO C testsuites.
     This is not pedwarn, because some people don't want
     an error for this.  */
  if (REAL_VALUE_ISINF (args->value) && pedantic)
    warning ("floating point number exceeds range of '%s'", typename);
}
 
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
    case CPP_OPEN_BRACE:  indent_level++;  break;
    case CPP_CLOSE_BRACE: indent_level--;  break;

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
      *value = lex_number ((const char *)tok->val.str.text, tok->val.str.len);
      break;

    case CPP_CHAR:
    case CPP_WCHAR:
      *value = lex_charconst (tok);
      break;

    case CPP_STRING:
    case CPP_WSTRING:
      *value = lex_string ((const char *)tok->val.str.text,
			   tok->val.str.len, tok->type == CPP_WSTRING);
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

#define ERROR(msgid) do { error(msgid); goto syntax_error; } while(0)

static tree
lex_number (str, len)
     const char *str;
     unsigned int len;
{
  int base = 10;
  int count = 0;
  int largest_digit = 0;
  int numdigits = 0;
  int overflow = 0;
  int c;
  tree value;
  const char *p;
  enum anon1 { NOT_FLOAT = 0, AFTER_POINT, AFTER_EXPON } floatflag = NOT_FLOAT;
  
  /* We actually store only HOST_BITS_PER_CHAR bits in each part.
     The code below which fills the parts array assumes that a host
     int is at least twice as wide as a host char, and that 
     HOST_BITS_PER_WIDE_INT is an even multiple of HOST_BITS_PER_CHAR.
     Two HOST_WIDE_INTs is the largest int literal we can store.
     In order to detect overflow below, the number of parts (TOTAL_PARTS)
     must be exactly the number of parts needed to hold the bits
     of two HOST_WIDE_INTs.  */
#define TOTAL_PARTS ((HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR) * 2)
  unsigned int parts[TOTAL_PARTS];
  
  /* Optimize for most frequent case.  */
  if (len == 1)
    {
      if (*str == '0')
	return integer_zero_node;
      else if (*str == '1')
	return integer_one_node;
      else
	return build_int_2 (*str - '0', 0);
    }

  for (count = 0; count < TOTAL_PARTS; count++)
    parts[count] = 0;

  /* len is known to be >1 at this point.  */
  p = str;

  if (len > 2 && str[0] == '0' && (str[1] == 'x' || str[1] == 'X'))
    {
      base = 16;
      p = str + 2;
    }
  /* The ISDIGIT check is so we are not confused by a suffix on 0.  */
  else if (str[0] == '0' && ISDIGIT (str[1]))
    {
      base = 8;
      p = str + 1;
    }

  do
    {
      c = *p++;

      if (c == '.')
	{
	  if (floatflag == AFTER_POINT)
	    ERROR ("too many decimal points in floating constant");
	  else if (floatflag == AFTER_EXPON)
	    ERROR ("decimal point in exponent - impossible!");
	  else
	    floatflag = AFTER_POINT;

	  if (base == 8)
	    base = 10;
	}
      else if (c == '_')
	/* Possible future extension: silently ignore _ in numbers,
	   permitting cosmetic grouping - e.g. 0x8000_0000 == 0x80000000
	   but somewhat easier to read.  Ada has this?  */
	ERROR ("underscore in number");
      else
	{
	  int n;
	  /* It is not a decimal point.
	     It should be a digit (perhaps a hex digit).  */

	  if (ISDIGIT (c)
	      || (base == 16 && ISXDIGIT (c)))
	    {
	      n = hex_value (c);
	    }
	  else if (base <= 10 && (c == 'e' || c == 'E'))
	    {
	      base = 10;
	      floatflag = AFTER_EXPON;
	      break;
	    }
	  else if (base == 16 && (c == 'p' || c == 'P'))
	    {
	      floatflag = AFTER_EXPON;
	      break;   /* start of exponent */
	    }
	  else
	    {
	      p--;
	      break;  /* start of suffix */
	    }

	  if (n >= largest_digit)
	    largest_digit = n;
	  numdigits++;

	  for (count = 0; count < TOTAL_PARTS; count++)
	    {
	      parts[count] *= base;
	      if (count)
		{
		  parts[count]
		    += (parts[count-1] >> HOST_BITS_PER_CHAR);
		  parts[count-1]
		    &= (1 << HOST_BITS_PER_CHAR) - 1;
		}
	      else
		parts[0] += n;
	    }

	  /* If the highest-order part overflows (gets larger than
	     a host char will hold) then the whole number has 
	     overflowed.  Record this and truncate the highest-order
	     part.  */
	  if (parts[TOTAL_PARTS - 1] >> HOST_BITS_PER_CHAR)
	    {
	      overflow = 1;
	      parts[TOTAL_PARTS - 1] &= (1 << HOST_BITS_PER_CHAR) - 1;
	    }
	}
    }
  while (p < str + len);

  /* This can happen on input like `int i = 0x;' */
  if (numdigits == 0)
    ERROR ("numeric constant with no digits");

  if (largest_digit >= base)
    ERROR ("numeric constant contains digits beyond the radix");

  if (floatflag != NOT_FLOAT)
    {
      tree type;
      int imag, fflag, lflag, conversion_errno;
      REAL_VALUE_TYPE real;
      struct pf_args args;
      char *copy;

      if (base == 16 && pedantic && !flag_isoc99)
	pedwarn ("floating constant may not be in radix 16");

      if (base == 16 && floatflag != AFTER_EXPON)
	ERROR ("hexadecimal floating constant has no exponent");

      /* Read explicit exponent if any, and put it in tokenbuf.  */
      if ((base == 10 && ((c == 'e') || (c == 'E')))
	  || (base == 16 && (c == 'p' || c == 'P')))
	{
	  if (p < str + len)
	    c = *p++;
	  if (p < str + len && (c == '+' || c == '-'))
	    c = *p++;
	  /* Exponent is decimal, even if string is a hex float.  */
	  if (! ISDIGIT (c))
	    ERROR ("floating constant exponent has no digits");
	  while (p < str + len && ISDIGIT (c))
	    c = *p++;
	  if (! ISDIGIT (c))
	    p--;
	}

      /* Copy the float constant now; we don't want any suffixes in the
	 string passed to parse_float.  */
      copy = alloca (p - str + 1);
      memcpy (copy, str, p - str);
      copy[p - str] = '\0';

      /* Now parse suffixes.  */
      fflag = lflag = imag = 0;
      while (p < str + len)
	switch (*p++)
	  {
	  case 'f': case 'F':
	    if (fflag)
	      ERROR ("more than one 'f' suffix on floating constant");
	    else if (warn_traditional && !in_system_header
		     && ! cpp_sys_macro_p (parse_in))
	      warning ("traditional C rejects the 'f' suffix");

	    fflag = 1;
	    break;

	  case 'l': case 'L':
	    if (lflag)
	      ERROR ("more than one 'l' suffix on floating constant");
	    else if (warn_traditional && !in_system_header
		     && ! cpp_sys_macro_p (parse_in))
	      warning ("traditional C rejects the 'l' suffix");

	    lflag = 1;
	    break;

	  case 'i': case 'I':
	  case 'j': case 'J':
	    if (imag)
	      ERROR ("more than one 'i' or 'j' suffix on floating constant");
	    else if (pedantic)
	      pedwarn ("ISO C forbids imaginary numeric constants");
	    imag = 1;
	    break;

	  default:
	    ERROR ("invalid suffix on floating constant");
	  }

      /* Setup input for parse_float() */
      args.str = copy;
      args.fflag = fflag;
      args.lflag = lflag;
      args.base = base;

      /* Convert string to a double, checking for overflow.  */
      if (do_float_handler (parse_float, (PTR) &args))
	{
	  /* Receive output from parse_float() */
	  real = args.value;
	}
      else
	  /* We got an exception from parse_float() */
	  ERROR ("floating constant out of range");

      /* Receive output from parse_float() */
      conversion_errno = args.conversion_errno;
      type = args.type;
	    
#ifdef ERANGE
      /* ERANGE is also reported for underflow,
	 so test the value to distinguish overflow from that.  */
      if (conversion_errno == ERANGE && !flag_traditional && pedantic
	  && (REAL_VALUES_LESS (dconst1, real)
	      || REAL_VALUES_LESS (real, dconstm1)))
	warning ("floating point number exceeds range of 'double'");
#endif

      /* Create a node with determined type and value.  */
      if (imag)
	value = build_complex (NULL_TREE, convert (type, integer_zero_node),
			       build_real (type, real));
      else
	value = build_real (type, real);
    }
  else
    {
      tree trad_type, ansi_type, type;
      HOST_WIDE_INT high, low;
      int spec_unsigned = 0;
      int spec_long = 0;
      int spec_long_long = 0;
      int spec_imag = 0;
      int suffix_lu = 0;
      int warn = 0, i;

      trad_type = ansi_type = type = NULL_TREE;
      while (p < str + len)
	{
	  c = *p++;
	  switch (c)
	    {
	    case 'u': case 'U':
	      if (spec_unsigned)
		error ("two 'u' suffixes on integer constant");
	      else if (warn_traditional && !in_system_header
		       && ! cpp_sys_macro_p (parse_in))
		warning ("traditional C rejects the 'u' suffix");

	      spec_unsigned = 1;
	      if (spec_long)
		suffix_lu = 1;
	      break;

	    case 'l': case 'L':
	      if (spec_long)
		{
		  if (spec_long_long)
		    error ("three 'l' suffixes on integer constant");
		  else if (suffix_lu)
		    error ("'lul' is not a valid integer suffix");
		  else if (c != spec_long)
		    error ("'Ll' and 'lL' are not valid integer suffixes");
		  else if (pedantic && ! flag_isoc99
			   && ! in_system_header && warn_long_long)
		    pedwarn ("ISO C89 forbids long long integer constants");
		  spec_long_long = 1;
		}
	      spec_long = c;
	      break;

	    case 'i': case 'I': case 'j': case 'J':
	      if (spec_imag)
		error ("more than one 'i' or 'j' suffix on integer constant");
	      else if (pedantic)
		pedwarn ("ISO C forbids imaginary numeric constants");
	      spec_imag = 1;
	      break;

	    default:
	      ERROR ("invalid suffix on integer constant");
	    }
	}

      /* If the literal overflowed, pedwarn about it now.  */
      if (overflow)
	{
	  warn = 1;
	  pedwarn ("integer constant is too large for this configuration of the compiler - truncated to %d bits", HOST_BITS_PER_WIDE_INT * 2);
	}

      /* This is simplified by the fact that our constant
	 is always positive.  */

      high = low = 0;

      for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR; i++)
	{
	  high |= ((HOST_WIDE_INT) parts[i + (HOST_BITS_PER_WIDE_INT
					      / HOST_BITS_PER_CHAR)]
		   << (i * HOST_BITS_PER_CHAR));
	  low |= (HOST_WIDE_INT) parts[i] << (i * HOST_BITS_PER_CHAR);
	}

      value = build_int_2 (low, high);
      TREE_TYPE (value) = long_long_unsigned_type_node;

      /* If warn_traditional, calculate both the ISO type and the
	 traditional type, then see if they disagree.
	 Otherwise, calculate only the type for the dialect in use.  */
      if (warn_traditional || flag_traditional)
	{
	  /* Calculate the traditional type.  */
	  /* Traditionally, any constant is signed; but if unsigned is
	     specified explicitly, obey that.  Use the smallest size
	     with the right number of bits, except for one special
	     case with decimal constants.  */
	  if (! spec_long && base != 10
	      && int_fits_type_p (value, unsigned_type_node))
	    trad_type = spec_unsigned ? unsigned_type_node : integer_type_node;
	  /* A decimal constant must be long if it does not fit in
	     type int.  I think this is independent of whether the
	     constant is signed.  */
	  else if (! spec_long && base == 10
		   && int_fits_type_p (value, integer_type_node))
	    trad_type = spec_unsigned ? unsigned_type_node : integer_type_node;
	  else if (! spec_long_long)
	    trad_type = (spec_unsigned
			 ? long_unsigned_type_node
			 : long_integer_type_node);
	  else if (int_fits_type_p (value,
				    spec_unsigned 
				    ? long_long_unsigned_type_node
				    : long_long_integer_type_node)) 
	    trad_type = (spec_unsigned
			 ? long_long_unsigned_type_node
			 : long_long_integer_type_node);
	  else
	    trad_type = (spec_unsigned
			 ? widest_unsigned_literal_type_node
			 : widest_integer_literal_type_node);
	}
      if (warn_traditional || ! flag_traditional)
	{
	  /* Calculate the ISO type.  */
	  if (! spec_long && ! spec_unsigned
	      && int_fits_type_p (value, integer_type_node))
	    ansi_type = integer_type_node;
	  else if (! spec_long && (base != 10 || spec_unsigned)
		   && int_fits_type_p (value, unsigned_type_node))
	    ansi_type = unsigned_type_node;
	  else if (! spec_unsigned && !spec_long_long
		   && int_fits_type_p (value, long_integer_type_node))
	    ansi_type = long_integer_type_node;
	  else if (! spec_long_long
		   && int_fits_type_p (value, long_unsigned_type_node))
	    ansi_type = long_unsigned_type_node;
	  else if (! spec_unsigned
		   && int_fits_type_p (value, long_long_integer_type_node))
	    ansi_type = long_long_integer_type_node;
	  else if (int_fits_type_p (value, long_long_unsigned_type_node))
	    ansi_type = long_long_unsigned_type_node;
	  else if (! spec_unsigned
		   && int_fits_type_p (value, widest_integer_literal_type_node))
	    ansi_type = widest_integer_literal_type_node;
	  else
	    ansi_type = widest_unsigned_literal_type_node;
	}

      type = flag_traditional ? trad_type : ansi_type;

      /* We assume that constants specified in a non-decimal
	 base are bit patterns, and that the programmer really
	 meant what they wrote.  */
      if (warn_traditional && !in_system_header
	  && base == 10 && trad_type != ansi_type)
	{
	  if (TYPE_PRECISION (trad_type) != TYPE_PRECISION (ansi_type))
	    warning ("width of integer constant changes with -traditional");
	  else if (TREE_UNSIGNED (trad_type) != TREE_UNSIGNED (ansi_type))
	    warning ("integer constant is unsigned in ISO C, signed with -traditional");
	  else
	    warning ("width of integer constant may change on other systems with -traditional");
	}

      if (pedantic && !flag_traditional && (flag_isoc99 || !spec_long_long)
	  && !warn
	  && ((flag_isoc99
	       ? TYPE_PRECISION (long_long_integer_type_node)
	       : TYPE_PRECISION (long_integer_type_node)) < TYPE_PRECISION (type)))
	{
	  warn = 1;
	  pedwarn ("integer constant larger than the maximum value of %s",
		   (flag_isoc99
		    ? (TREE_UNSIGNED (type)
		       ? _("an unsigned long long int")
		       : _("a long long int"))
		    : _("an unsigned long int")));
	}

      if (base == 10 && ! spec_unsigned && TREE_UNSIGNED (type))
	warning ("decimal constant is so large that it is unsigned");

      if (spec_imag)
	{
	  if (TYPE_PRECISION (type)
	      <= TYPE_PRECISION (integer_type_node))
	    value = build_complex (NULL_TREE, integer_zero_node,
				   convert (integer_type_node, value));
	  else
	    ERROR ("complex integer constant is too wide for 'complex int'");
	}
      else if (flag_traditional && !int_fits_type_p (value, type))
	/* The traditional constant 0x80000000 is signed
	   but doesn't fit in the range of int.
	   This will change it to -0x80000000, which does fit.  */
	{
	  TREE_TYPE (value) = unsigned_type (type);
	  value = convert (type, value);
	  TREE_OVERFLOW (value) = TREE_CONSTANT_OVERFLOW (value) = 0;
	}
      else
	TREE_TYPE (value) = type;

      /* If it's still an integer (not a complex), and it doesn't
	 fit in the type we choose for it, then pedwarn.  */

      if (! warn
	  && TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
	  && ! int_fits_type_p (value, TREE_TYPE (value)))
	pedwarn ("integer constant is larger than the maximum value for its type");
    }

  if (p < str + len)
    error ("missing white space after number '%.*s'", (int) (p - str), str);

  return value;

 syntax_error:
  return integer_zero_node;
}

static tree
lex_string (str, len, wide)
     const char *str;
     unsigned int len;
     int wide;
{
  tree value;
  char *buf = alloca ((len + 1) * (wide ? WCHAR_BYTES : 1));
  char *q = buf;
  const char *p = str, *limit = str + len;
  unsigned int c;
  unsigned width = wide ? WCHAR_TYPE_SIZE
			: TYPE_PRECISION (char_type_node);

#ifdef MULTIBYTE_CHARS
  /* Reset multibyte conversion state.  */
  (void) local_mbtowc (NULL, NULL, 0);
#endif

  while (p < limit)
    {
#ifdef MULTIBYTE_CHARS
      wchar_t wc;
      int char_len;

      char_len = local_mbtowc (&wc, p, limit - p);
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
	{
	  unsigned int mask;

	  if (width < HOST_BITS_PER_INT)
	    mask = ((unsigned int) 1 << width) - 1;
	  else
	    mask = ~0;
	  c = cpp_parse_escape (parse_in, (const unsigned char **) &p,
				(const unsigned char *) limit,
				mask, flag_traditional);
	}
	
      /* Add this single character into the buffer either as a wchar_t
	 or as a single byte.  */
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
  HOST_WIDE_INT result;
  tree value;
  unsigned int chars_seen;
 
  result = cpp_interpret_charconst (parse_in, token, warn_multichar,
 				    flag_traditional, &chars_seen);
  if (token->type == CPP_WCHAR)
    {
      value = build_int_2 (result, 0);
      TREE_TYPE (value) = wchar_type_node;
    }
  else
    {
      if (result < 0)
 	value = build_int_2 (result, -1);
      else
 	value = build_int_2 (result, 0);
 
      /* In C, a character constant has type 'int'.
 	 In C++ 'char', but multi-char charconsts have type 'int'.  */
      if (c_language == clk_cplusplus && chars_seen <= 1)
 	TREE_TYPE (value) = char_type_node;
      else
 	TREE_TYPE (value) = integer_type_node;
    }
 
  return value;
}
