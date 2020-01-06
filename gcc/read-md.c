/* MD reader for GCC.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.

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

/* This file is compiled twice: once for the generator programs
   once for the compiler.  */
#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif
#include "system.h"
#include "coretypes.h"
#ifdef GENERATOR_FILE
#include "errors.h"
#endif /* #ifdef GENERATOR_FILE */
#include "statistics.h"
#include "vec.h"
#include "read-md.h"

#ifndef GENERATOR_FILE

/* Minimal reimplementation of errors.c for use by RTL frontend
   within cc1.  */

int have_error = 0;

#endif /* #ifndef GENERATOR_FILE */


/* This callback will be invoked whenever an md include directive is
   processed.  To be used for creation of the dependency file.  */
void (*include_callback) (const char *);

/* Global singleton.  */

md_reader *md_reader_ptr;

/* Given an object that starts with a char * name field, return a hash
   code for its name.  */

hashval_t
leading_string_hash (const void *def)
{
  return htab_hash_string (*(const char *const *) def);
}

/* Given two objects that start with char * name fields, return true if
   they have the same name.  */

int
leading_string_eq_p (const void *def1, const void *def2)
{
  return strcmp (*(const char *const *) def1,
		 *(const char *const *) def2) == 0;
}

/* Return a hash value for the pointer pointed to by DEF.  */

static hashval_t
leading_ptr_hash (const void *def)
{
  return htab_hash_pointer (*(const void *const *) def);
}

/* Return true if DEF1 and DEF2 are pointers to the same pointer.  */

static int
leading_ptr_eq_p (const void *def1, const void *def2)
{
  return *(const void *const *) def1 == *(const void *const *) def2;
}

/* Associate PTR with the file position given by FILE_LOC.  */

void
md_reader::set_md_ptr_loc (const void *ptr, file_location file_loc)
{
  struct ptr_loc *loc;

  loc = (struct ptr_loc *) obstack_alloc (&m_ptr_loc_obstack,
					  sizeof (struct ptr_loc));
  loc->ptr = ptr;
  loc->loc = file_loc;
  *htab_find_slot (m_ptr_locs, loc, INSERT) = loc;
}

/* Return the position associated with pointer PTR.  Return null if no
   position was set.  */

const md_reader::ptr_loc *
md_reader::get_md_ptr_loc (const void *ptr)
{
  return (const struct ptr_loc *) htab_find (m_ptr_locs, &ptr);
}

/* Associate NEW_PTR with the same file position as OLD_PTR.  */

void
md_reader::copy_md_ptr_loc (const void *new_ptr, const void *old_ptr)
{
  const struct ptr_loc *loc = get_md_ptr_loc (old_ptr);
  if (loc != 0)
    set_md_ptr_loc (new_ptr, loc->loc);
}

/* If PTR is associated with a known file position, print a #line
   directive for it to OUTF.  */

void
md_reader::fprint_md_ptr_loc (FILE *outf, const void *ptr)
{
  const struct ptr_loc *loc = get_md_ptr_loc (ptr);
  if (loc != 0)
    fprintf (outf, "#line %d \"%s\"\n", loc->loc.lineno, loc->loc.filename);
}

/* Special fprint_md_ptr_loc for writing to STDOUT.  */
void
md_reader::print_md_ptr_loc (const void *ptr)
{
  fprint_md_ptr_loc (stdout, ptr);
}

/* Return a condition that satisfies both COND1 and COND2.  Either string
   may be null or empty.  */

const char *
md_reader::join_c_conditions (const char *cond1, const char *cond2)
{
  char *result;
  const void **entry;

  if (cond1 == 0 || cond1[0] == 0)
    return cond2;

  if (cond2 == 0 || cond2[0] == 0)
    return cond1;

  if (strcmp (cond1, cond2) == 0)
    return cond1;

  result = concat ("(", cond1, ") && (", cond2, ")", NULL);
  obstack_ptr_grow (&m_joined_conditions_obstack, result);
  obstack_ptr_grow (&m_joined_conditions_obstack, cond1);
  obstack_ptr_grow (&m_joined_conditions_obstack, cond2);
  entry = XOBFINISH (&m_joined_conditions_obstack, const void **);
  *htab_find_slot (m_joined_conditions, entry, INSERT) = entry;
  return result;
}

/* Print condition COND to OUTF, wrapped in brackets.  If COND was created
   by join_c_conditions, recursively invoke this function for the original
   conditions and join the result with "&&".  Otherwise print a #line
   directive for COND if its original file position is known.  */

void
md_reader::fprint_c_condition (FILE *outf, const char *cond)
{
  const char **halves = (const char **) htab_find (m_joined_conditions, &cond);
  if (halves != 0)
    {
      fprintf (outf, "(");
      fprint_c_condition (outf, halves[1]);
      fprintf (outf, " && ");
      fprint_c_condition (outf, halves[2]);
      fprintf (outf, ")");
    }
  else
    {
      fputc ('\n', outf);
      fprint_md_ptr_loc (outf, cond);
      fprintf (outf, "(%s)", cond);
    }
}

/* Special fprint_c_condition for writing to STDOUT.  */

void
md_reader::print_c_condition (const char *cond)
{
  fprint_c_condition (stdout, cond);
}

/* A vfprintf-like function for reporting an error against line LINENO
   of the current MD file.  */

static void ATTRIBUTE_PRINTF(2,0)
message_at_1 (file_location loc, const char *msg, va_list ap)
{
  fprintf (stderr, "%s:%d:%d: ", loc.filename, loc.lineno, loc.colno);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);
}

/* A printf-like function for reporting a message against location LOC.  */

void
message_at (file_location loc, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  message_at_1 (loc, msg, ap);
  va_end (ap);
}

/* Like message_at, but treat the condition as an error.  */

void
error_at (file_location loc, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  message_at_1 (loc, msg, ap);
  va_end (ap);
  have_error = 1;
}

/* Like message_at, but treat the condition as a fatal error.  */

void
fatal_at (file_location loc, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  message_at_1 (loc, msg, ap);
  va_end (ap);
  exit (1);
}

/* A printf-like function for reporting an error against the current
   position in the MD file.  */

void
fatal_with_file_and_line (const char *msg, ...)
{
  char context[64];
  size_t i;
  int c;
  va_list ap;

  va_start (ap, msg);

  fprintf (stderr, "%s:%d:%d: error: ", md_reader_ptr->get_filename (),
	   md_reader_ptr->get_lineno (),
	   md_reader_ptr->get_colno ());
  vfprintf (stderr, msg, ap);
  putc ('\n', stderr);

  /* Gather some following context.  */
  for (i = 0; i < sizeof (context)-1; ++i)
    {
      c = read_char ();
      if (c == EOF)
	break;
      if (c == '\r' || c == '\n')
	{
	  unread_char (c);
	  break;
	}
      context[i] = c;
    }
  context[i] = '\0';

  fprintf (stderr, "%s:%d:%d: note: following context is `%s'\n",
	   md_reader_ptr->get_filename (),
	   md_reader_ptr->get_lineno (),
	   md_reader_ptr->get_colno (), context);

  va_end (ap);
  exit (1);
}

/* Report that we found character ACTUAL when we expected to find
   character EXPECTED.  */

void
fatal_expected_char (int expected, int actual)
{
  if (actual == EOF)
    fatal_with_file_and_line ("expected character `%c', found EOF",
			      expected);
  else
    fatal_with_file_and_line ("expected character `%c', found `%c'",
			      expected, actual);
}

/* Read chars from the MD file until a non-whitespace char and return that.
   Comments, both Lisp style and C style, are treated as whitespace.  */

int
read_skip_spaces (void)
{
  int c;

  while (1)
    {
      c = read_char ();
      switch (c)
	{
	case ' ': case '\t': case '\f': case '\r': case '\n':
	  break;

	case ';':
	  do
	    c = read_char ();
	  while (c != '\n' && c != EOF);
	  break;

	case '/':
	  {
	    int prevc;
	    c = read_char ();
	    if (c != '*')
	      {
		unread_char (c);
		fatal_with_file_and_line ("stray '/' in file");
	      }

	    prevc = 0;
	    while ((c = read_char ()) && c != EOF)
	      {
		if (prevc == '*' && c == '/')
		  break;
	        prevc = c;
	      }
	  }
	  break;

	default:
	  return c;
	}
    }
}

/* Consume the next character, issuing a fatal error if it is not
   EXPECTED.  */

void
md_reader::require_char (char expected)
{
  int ch = read_char ();
  if (ch != expected)
    fatal_expected_char (expected, ch);
}

/* Consume any whitespace, then consume the next non-whitespace
   character, issuing a fatal error if it is not EXPECTED.  */

void
md_reader::require_char_ws (char expected)
{
  int ch = read_skip_spaces ();
  if (ch != expected)
    fatal_expected_char (expected, ch);
}

/* Consume any whitespace, then consume the next word (as per read_name),
   issuing a fatal error if it is not EXPECTED.  */

void
md_reader::require_word_ws (const char *expected)
{
  struct md_name name;
  read_name (&name);
  if (strcmp (name.string, expected))
    fatal_with_file_and_line ("missing '%s'", expected);
}

/* Read the next character from the file.  */

int
md_reader::read_char (void)
{
  int ch;

  ch = getc (m_read_md_file);
  if (ch == '\n')
    {
      m_read_md_lineno++;
      m_last_line_colno = m_read_md_colno;
      m_read_md_colno = 0;
    }
  else
    m_read_md_colno++;

  /* If we're filtering lines, treat everything before the range of
     interest as a space, and as EOF for everything after.  */
  if (m_first_line && m_last_line)
    {
      if (m_read_md_lineno < m_first_line)
	return ' ';
      if (m_read_md_lineno > m_last_line)
	return EOF;
    }

  return ch;
}

/* Put back CH, which was the last character read from the file.  */

void
md_reader::unread_char (int ch)
{
  if (ch == '\n')
    {
      m_read_md_lineno--;
      m_read_md_colno = m_last_line_colno;
    }
  else
    m_read_md_colno--;
  ungetc (ch, m_read_md_file);
}

/* Peek at the next character from the file without consuming it.  */

int
md_reader::peek_char (void)
{
  int ch = read_char ();
  unread_char (ch);
  return ch;
}

/* Read an rtx code name into NAME.  It is terminated by any of the
   punctuation chars of rtx printed syntax.  */

bool
md_reader::read_name_1 (struct md_name *name, file_location *out_loc)
{
  int c;
  size_t i;
  int angle_bracket_depth;

  c = read_skip_spaces ();

  *out_loc = get_current_location ();

  i = 0;
  angle_bracket_depth = 0;
  while (1)
    {
      if (c == '<')
	angle_bracket_depth++;

      if ((c == '>') && (angle_bracket_depth > 0))
	  angle_bracket_depth--;

      if (c == ' ' || c == '\n' || c == '\t' || c == '\f' || c == '\r'
	  || c == EOF)
	break;
      if (angle_bracket_depth == 0)
	{
	  if (c == ':' || c == ')' || c == ']'
	      || c == '"' || c == '/' || c == '(' || c == '[')
	    {
	      unread_char (c);
	      break;
	    }
	}

      if (i == sizeof (name->buffer) - 1)
	fatal_with_file_and_line ("name too long");
      name->buffer[i++] = c;

      c = read_char ();
    }

  if (i == 0)
    return false;

  name->buffer[i] = 0;
  name->string = name->buffer;

  if (m_md_constants)
    {
      /* Do constant expansion.  */
      struct md_constant *def;

      do
	{
	  struct md_constant tmp_def;

	  tmp_def.name = name->string;
	  def = (struct md_constant *) htab_find (m_md_constants, &tmp_def);
	  if (def)
	    name->string = def->value;
	}
      while (def);
    }

  return true;
}

/* Read an rtx code name into NAME.  It is terminated by any of the
   punctuation chars of rtx printed syntax.  */

file_location
md_reader::read_name (struct md_name *name)
{
  file_location loc;
  if (!read_name_1 (name, &loc))
    fatal_with_file_and_line ("missing name or number");
  return loc;
}

file_location
md_reader::read_name_or_nil (struct md_name *name)
{
  file_location loc;
  if (!read_name_1 (name, &loc))
    {
      file_location loc = get_current_location ();
      read_skip_construct (0, loc);
      /* Skip the ')'.  */
      read_char ();
      name->buffer[0] = 0;
      name->string = name->buffer;
    }
  return loc;
}

/* Subroutine of the string readers.  Handles backslash escapes.
   Caller has read the backslash, but not placed it into the obstack.  */

void
md_reader::read_escape ()
{
  int c = read_char ();

  switch (c)
    {
      /* Backslash-newline is replaced by nothing, as in C.  */
    case '\n':
      return;

      /* \" \' \\ are replaced by the second character.  */
    case '\\':
    case '"':
    case '\'':
      break;

      /* Standard C string escapes:
	 \a \b \f \n \r \t \v
	 \[0-7] \x
	 all are passed through to the output string unmolested.
	 In normal use these wind up in a string constant processed
	 by the C compiler, which will translate them appropriately.
	 We do not bother checking that \[0-7] are followed by up to
	 two octal digits, or that \x is followed by N hex digits.
	 \? \u \U are left out because they are not in traditional C.  */
    case 'a': case 'b': case 'f': case 'n': case 'r': case 't': case 'v':
    case '0': case '1': case '2': case '3': case '4': case '5': case '6':
    case '7': case 'x':
      obstack_1grow (&m_string_obstack, '\\');
      break;

      /* \; makes stuff for a C string constant containing
	 newline and tab.  */
    case ';':
      obstack_grow (&m_string_obstack, "\\n\\t", 4);
      return;

      /* pass anything else through, but issue a warning.  */
    default:
      fprintf (stderr, "%s:%d: warning: unrecognized escape \\%c\n",
	       get_filename (), get_lineno (),
	       c);
      obstack_1grow (&m_string_obstack, '\\');
      break;
    }

  obstack_1grow (&m_string_obstack, c);
}

/* Read a double-quoted string onto the obstack.  Caller has scanned
   the leading quote.  */

char *
md_reader::read_quoted_string ()
{
  int c;

  while (1)
    {
      c = read_char (); /* Read the string  */
      if (c == '\\')
	{
	  read_escape ();
	  continue;
	}
      else if (c == '"' || c == EOF)
	break;

      obstack_1grow (&m_string_obstack, c);
    }

  obstack_1grow (&m_string_obstack, 0);
  return XOBFINISH (&m_string_obstack, char *);
}

/* Read a braced string (a la Tcl) onto the string obstack.  Caller
   has scanned the leading brace.  Note that unlike quoted strings,
   the outermost braces _are_ included in the string constant.  */

char *
md_reader::read_braced_string ()
{
  int c;
  int brace_depth = 1;  /* caller-processed */
  unsigned long starting_read_md_lineno = get_lineno ();

  obstack_1grow (&m_string_obstack, '{');
  while (brace_depth)
    {
      c = read_char (); /* Read the string  */

      if (c == '{')
	brace_depth++;
      else if (c == '}')
	brace_depth--;
      else if (c == '\\')
	{
	  read_escape ();
	  continue;
	}
      else if (c == EOF)
	fatal_with_file_and_line
	  ("missing closing } for opening brace on line %lu",
	   starting_read_md_lineno);

      obstack_1grow (&m_string_obstack, c);
    }

  obstack_1grow (&m_string_obstack, 0);
  return XOBFINISH (&m_string_obstack, char *);
}

/* Read some kind of string constant.  This is the high-level routine
   used by read_rtx.  It handles surrounding parentheses, leading star,
   and dispatch to the appropriate string constant reader.  */

char *
md_reader::read_string (int star_if_braced)
{
  char *stringbuf;
  int saw_paren = 0;
  int c;

  c = read_skip_spaces ();
  if (c == '(')
    {
      saw_paren = 1;
      c = read_skip_spaces ();
    }

  file_location loc = get_current_location ();
  if (c == '"')
    stringbuf = read_quoted_string ();
  else if (c == '{')
    {
      if (star_if_braced)
	obstack_1grow (&m_string_obstack, '*');
      stringbuf = read_braced_string ();
    }
  else if (saw_paren && c == 'n')
    {
      /* Handle (nil) by returning NULL.  */
      require_char ('i');
      require_char ('l');
      require_char_ws (')');
      return NULL;
    }
  else
    fatal_with_file_and_line ("expected `\"' or `{', found `%c'", c);

  if (saw_paren)
    require_char_ws (')');

  set_md_ptr_loc (stringbuf, loc);
  return stringbuf;
}

/* Skip the rest of a construct that started at line LINENO and that
   is currently nested by DEPTH levels of parentheses.  */

void
md_reader::read_skip_construct (int depth, file_location loc)
{
  struct md_name name;
  int c;

  do
    {
      c = read_skip_spaces ();
      if (c == EOF)
	{
	  error_at (loc, "unterminated construct");
	  exit (1);
	}
      switch (c)
	{
	case '(':
	  depth++;
	  break;

	case ')':
	  depth--;
	  break;

	case ':':
	case '[':
	case ']':
	case '/':
	  break;

	case '\"':
	case '{':
	  unread_char (c);
	  read_string (false);
	  break;

	default:
	  unread_char (c);
	  read_name (&name);
	  break;
	}
    }
  while (depth > 0);
  unread_char (c);
}

/* Given a string, return the number of comma-separated elements in it.
   Return 0 for the null string.  */

int
n_comma_elts (const char *s)
{
  int n;

  if (*s == '\0')
    return 0;

  for (n = 1; *s; s++)
    if (*s == ',')
      n++;

  return n;
}

/* Given a pointer to a (char *), return a pointer to the beginning of the
   next comma-separated element in the string.  Advance the pointer given
   to the end of that element.  Return NULL if at end of string.  Caller
   is responsible for copying the string if necessary.  White space between
   a comma and an element is ignored.  */

const char *
scan_comma_elt (const char **pstr)
{
  const char *start;
  const char *p = *pstr;

  if (*p == ',')
    p++;
  while (ISSPACE (*p))
    p++;

  if (*p == '\0')
    return NULL;

  start = p;

  while (*p != ',' && *p != '\0')
    p++;

  *pstr = p;
  return start;
}

/* Convert STRING to uppercase.  */

void
upcase_string (char *string)
{
  int i;

  for (i = 0; string[i]; i++)
    string[i] = TOUPPER (string[i]);
}

/* Add a NAME = VALUE definition to md_constants-style hash table DEFS,
   where both NAME and VALUE are malloc()ed strings.  PARENT_ENUM is the
   enum to which NAME belongs, or null if NAME is a stand-alone constant.  */

static struct md_constant *
add_constant (htab_t defs, char *name, char *value,
	      struct enum_type *parent_enum)
{
  struct md_constant *def, tmp_def;
  void **entry_ptr;

  tmp_def.name = name;
  entry_ptr = htab_find_slot (defs, &tmp_def, INSERT);
  if (*entry_ptr)
    {
      def = (struct md_constant *) *entry_ptr;
      if (strcmp (def->value, value) != 0)
	fatal_with_file_and_line ("redefinition of `%s', was `%s', now `%s'",
				  def->name, def->value, value);
      else if (parent_enum || def->parent_enum)
	fatal_with_file_and_line ("redefinition of `%s'", def->name);
      free (name);
      free (value);
    }
  else
    {
      def = XNEW (struct md_constant);
      def->name = name;
      def->value = value;
      def->parent_enum = parent_enum;
      *entry_ptr = def;
    }
  return def;
}

/* Process a define_constants directive, starting with the optional space
   after the "define_constants".  */

void
md_reader::handle_constants ()
{
  int c;
  htab_t defs;

  require_char_ws ('[');

  /* Disable constant expansion during definition processing.  */
  defs = m_md_constants;
  m_md_constants = 0;
  while ( (c = read_skip_spaces ()) != ']')
    {
      struct md_name name, value;

      if (c != '(')
	fatal_expected_char ('(', c);

      read_name (&name);
      read_name (&value);
      add_constant (defs, xstrdup (name.string), xstrdup (value.string), 0);

      require_char_ws (')');
    }
  m_md_constants = defs;
}

/* For every constant definition, call CALLBACK with two arguments:
   a pointer a pointer to the constant definition and INFO.
   Stop when CALLBACK returns zero.  */

void
md_reader::traverse_md_constants (htab_trav callback, void *info)
{
  htab_traverse (get_md_constants (), callback, info);
}

/* Return a malloc()ed decimal string that represents number NUMBER.  */

static char *
md_decimal_string (int number)
{
  /* A safe overestimate.  +1 for sign, +1 for null terminator.  */
  char buffer[sizeof (int) * CHAR_BIT + 1 + 1];

  sprintf (buffer, "%d", number);
  return xstrdup (buffer);
}

/* Process a define_enum or define_c_enum directive, starting with
   the optional space after the "define_enum".  LINENO is the line
   number on which the directive started and MD_P is true if the
   directive is a define_enum rather than a define_c_enum.  */

void
md_reader::handle_enum (file_location loc, bool md_p)
{
  char *enum_name, *value_name;
  struct md_name name;
  struct enum_type *def;
  struct enum_value *ev;
  void **slot;
  int c;

  enum_name = read_string (false);
  slot = htab_find_slot (m_enum_types, &enum_name, INSERT);
  if (*slot)
    {
      def = (struct enum_type *) *slot;
      if (def->md_p != md_p)
	error_at (loc, "redefining `%s' as a different type of enum",
		  enum_name);
    }
  else
    {
      def = XNEW (struct enum_type);
      def->name = enum_name;
      def->md_p = md_p;
      def->values = 0;
      def->tail_ptr = &def->values;
      def->num_values = 0;
      *slot = def;
    }

  require_char_ws ('[');

  while ((c = read_skip_spaces ()) != ']')
    {
      if (c == EOF)
	{
	  error_at (loc, "unterminated construct");
	  exit (1);
	}
      unread_char (c);
      read_name (&name);

      ev = XNEW (struct enum_value);
      ev->next = 0;
      if (md_p)
	{
	  value_name = concat (def->name, "_", name.string, NULL);
	  upcase_string (value_name);
	  ev->name = xstrdup (name.string);
	}
      else
	{
	  value_name = xstrdup (name.string);
	  ev->name = value_name;
	}
      ev->def = add_constant (get_md_constants (), value_name,
			      md_decimal_string (def->num_values), def);

      *def->tail_ptr = ev;
      def->tail_ptr = &ev->next;
      def->num_values++;
    }
}

/* Try to find the definition of the given enum.  Return null on failure.  */

struct enum_type *
md_reader::lookup_enum_type (const char *name)
{
  return (struct enum_type *) htab_find (m_enum_types, &name);
}

/* For every enum definition, call CALLBACK with two arguments:
   a pointer to the constant definition and INFO.  Stop when CALLBACK
   returns zero.  */

void
md_reader::traverse_enum_types (htab_trav callback, void *info)
{
  htab_traverse (m_enum_types, callback, info);
}


/* Constructor for md_reader.  */

md_reader::md_reader (bool compact)
: m_compact (compact),
  m_toplevel_fname (NULL),
  m_base_dir (NULL),
  m_read_md_file (NULL),
  m_read_md_filename (NULL),
  m_read_md_lineno (0),
  m_read_md_colno (0),
  m_first_dir_md_include (NULL),
  m_last_dir_md_include_ptr (&m_first_dir_md_include),
  m_first_line (0),
  m_last_line (0),
  m_first_overload (NULL),
  m_next_overload_ptr (&m_first_overload),
  m_overloads_htab (NULL)
{
  /* Set the global singleton pointer.  */
  md_reader_ptr = this;

  obstack_init (&m_string_obstack);

  m_ptr_locs = htab_create (161, leading_ptr_hash, leading_ptr_eq_p, 0);
  obstack_init (&m_ptr_loc_obstack);

  m_joined_conditions = htab_create (161, leading_ptr_hash, leading_ptr_eq_p, 0);
  obstack_init (&m_joined_conditions_obstack);

  m_md_constants = htab_create (31, leading_string_hash,
				leading_string_eq_p, (htab_del) 0);

  m_enum_types = htab_create (31, leading_string_hash,
			      leading_string_eq_p, (htab_del) 0);

  /* Unlock the stdio streams.  */
  unlock_std_streams ();
}

/* md_reader's destructor.  */

md_reader::~md_reader ()
{
  free (m_base_dir);

  htab_delete (m_enum_types);

  htab_delete (m_md_constants);

  obstack_free (&m_joined_conditions_obstack, NULL);
  htab_delete (m_joined_conditions);

  obstack_free (&m_ptr_loc_obstack, NULL);
  htab_delete (m_ptr_locs);

  obstack_free (&m_string_obstack, NULL);

  /* Clear the global singleton pointer.  */
  md_reader_ptr = NULL;
}

/* Process an "include" directive, starting with the optional space
   after the "include".  Read in the file and use HANDLE_DIRECTIVE
   to process each unknown directive.  LINENO is the line number on
   which the "include" occurred.  */

void
md_reader::handle_include (file_location loc)
{
  const char *filename;
  const char *old_filename;
  int old_lineno, old_colno;
  char *pathname;
  FILE *input_file, *old_file;

  filename = read_string (false);
  input_file = NULL;

  /* If the specified file name is absolute, skip the include stack.  */
  if (!IS_ABSOLUTE_PATH (filename))
    {
      struct file_name_list *stackp;

      /* Search the directory path, trying to open the file.  */
      for (stackp = m_first_dir_md_include; stackp; stackp = stackp->next)
	{
	  static const char sep[2] = { DIR_SEPARATOR, '\0' };

	  pathname = concat (stackp->fname, sep, filename, NULL);
	  input_file = fopen (pathname, "r");
	  if (input_file != NULL)
	    break;
	  free (pathname);
	}
    }

  /* If we haven't managed to open the file yet, try combining the
     filename with BASE_DIR.  */
  if (input_file == NULL)
    {
      if (m_base_dir)
	pathname = concat (m_base_dir, filename, NULL);
      else
	pathname = xstrdup (filename);
      input_file = fopen (pathname, "r");
    }

  if (input_file == NULL)
    {
      free (pathname);
      error_at (loc, "include file `%s' not found", filename);
      return;
    }

  /* Save the old cursor.  Note that the LINENO argument to this
     function is the beginning of the include statement, while
     read_md_lineno has already been advanced.  */
  old_file = m_read_md_file;
  old_filename = m_read_md_filename;
  old_lineno = m_read_md_lineno;
  old_colno = m_read_md_colno;

  if (include_callback)
    include_callback (pathname);

  m_read_md_file = input_file;
  m_read_md_filename = pathname;

  handle_file ();

  /* Restore the old cursor.  */
  m_read_md_file = old_file;
  m_read_md_filename = old_filename;
  m_read_md_lineno = old_lineno;
  m_read_md_colno = old_colno;

  /* Do not free the pathname.  It is attached to the various rtx
     queue elements.  */
}

/* Process the current file, assuming that read_md_file and
   read_md_filename are valid.  Use HANDLE_DIRECTIVE to handle
   unknown directives.  */

void
md_reader::handle_file ()
{
  struct md_name directive;
  int c;

  m_read_md_lineno = 1;
  m_read_md_colno = 0;
  while ((c = read_skip_spaces ()) != EOF)
    {
      file_location loc = get_current_location ();
      if (c != '(')
	fatal_expected_char ('(', c);

      read_name (&directive);
      if (strcmp (directive.string, "define_constants") == 0)
	handle_constants ();
      else if (strcmp (directive.string, "define_enum") == 0)
	handle_enum (loc, true);
      else if (strcmp (directive.string, "define_c_enum") == 0)
	handle_enum (loc, false);
      else if (strcmp (directive.string, "include") == 0)
	handle_include (loc);
      else
	handle_unknown_directive (loc, directive.string);

      require_char_ws (')');
    }
  fclose (m_read_md_file);
}

/* Like handle_file, but for top-level files.  Set up m_toplevel_fname
   and m_base_dir accordingly.  */

void
md_reader::handle_toplevel_file ()
{
  const char *base;

  m_toplevel_fname = m_read_md_filename;
  base = lbasename (m_toplevel_fname);
  if (base == m_toplevel_fname)
    m_base_dir = NULL;
  else
    m_base_dir = xstrndup (m_toplevel_fname, base - m_toplevel_fname);

  handle_file ();
}

file_location
md_reader::get_current_location () const
{
  return file_location (m_read_md_filename, m_read_md_lineno, m_read_md_colno);
}

/* Parse a -I option with argument ARG.  */

void
md_reader::add_include_path (const char *arg)
{
  struct file_name_list *dirtmp;

  dirtmp = XNEW (struct file_name_list);
  dirtmp->next = 0;
  dirtmp->fname = arg;
  *m_last_dir_md_include_ptr = dirtmp;
  m_last_dir_md_include_ptr = &dirtmp->next;
}

#ifdef GENERATOR_FILE

/* The main routine for reading .md files.  Try to process all the .md
   files specified on the command line and return true if no error occurred.

   ARGC and ARGV are the arguments to main.

   PARSE_OPT, if nonnull, is passed all unknown command-line arguments.
   It should return true if it recognizes the argument or false if a
   generic error should be reported.  */

bool
md_reader::read_md_files (int argc, const char **argv,
			  bool (*parse_opt) (const char *))
{
  int i;
  bool no_more_options;
  bool already_read_stdin;
  int num_files;

  /* First we loop over all the options.  */
  for (i = 1; i < argc; i++)
    if (argv[i][0] == '-')
      {
	/* An argument consisting of exactly one dash is a request to
	   read stdin.  This will be handled in the second loop.  */
	if (argv[i][1] == '\0')
	  continue;

	/* An argument consisting of just two dashes causes option
	   parsing to cease.  */
	if (argv[i][1] == '-' && argv[i][2] == '\0')
	  break;

	if (argv[i][1] == 'I')
	  {
	    if (argv[i][2] != '\0')
	      add_include_path (argv[i] + 2);
	    else if (++i < argc)
	      add_include_path (argv[i]);
	    else
	      fatal ("directory name missing after -I option");
	    continue;
	  }

	/* The program may have provided a callback so it can
	   accept its own options.  */
	if (parse_opt && parse_opt (argv[i]))
	  continue;

	fatal ("invalid option `%s'", argv[i]);
      }

  /* Now loop over all input files.  */
  num_files = 0;
  no_more_options = false;
  already_read_stdin = false;
  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-')
	{
	  if (argv[i][1] == '\0')
	    {
	      /* Read stdin.  */
	      if (already_read_stdin)
		fatal ("cannot read standard input twice");

	      m_read_md_file = stdin;
	      m_read_md_filename = "<stdin>";
	      handle_toplevel_file ();
	      already_read_stdin = true;
	      continue;
	    }
	  else if (argv[i][1] == '-' && argv[i][2] == '\0')
	    {
	      /* No further arguments are to be treated as options.  */
	      no_more_options = true;
	      continue;
	    }
	  else if (!no_more_options)
	    continue;
	}

      /* If we get here we are looking at a non-option argument, i.e.
	 a file to be processed.  */
      m_read_md_filename = argv[i];
      m_read_md_file = fopen (m_read_md_filename, "r");
      if (m_read_md_file == 0)
	{
	  perror (m_read_md_filename);
	  return false;
	}
      handle_toplevel_file ();
      num_files++;
    }

  /* If we get to this point without having seen any files to process,
     read the standard input now.  */
  if (num_files == 0 && !already_read_stdin)
    {
      m_read_md_file = stdin;
      m_read_md_filename = "<stdin>";
      handle_toplevel_file ();
    }

  return !have_error;
}

#endif /* #ifdef GENERATOR_FILE */

/* Read FILENAME.  */

bool
md_reader::read_file (const char *filename)
{
  m_read_md_filename = filename;
  m_read_md_file = fopen (m_read_md_filename, "r");
  if (m_read_md_file == 0)
    {
      perror (m_read_md_filename);
      return false;
    }
  handle_toplevel_file ();
  return !have_error;
}

/* Read FILENAME, filtering to just the given lines.  */

bool
md_reader::read_file_fragment (const char *filename,
			       int first_line,
			       int last_line)
{
  m_read_md_filename = filename;
  m_read_md_file = fopen (m_read_md_filename, "r");
  if (m_read_md_file == 0)
    {
      perror (m_read_md_filename);
      return false;
    }
  m_first_line = first_line;
  m_last_line = last_line;
  handle_toplevel_file ();
  return !have_error;
}

/* class noop_reader : public md_reader */

/* A dummy implementation which skips unknown directives.  */
void
noop_reader::handle_unknown_directive (file_location loc, const char *)
{
  read_skip_construct (1, loc);
}
