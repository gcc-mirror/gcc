/* MD reader for GCC.
   Copyright (C) 1987, 1988, 1991, 1994, 1997, 1998, 1999, 2000, 2001, 2002,
   2003, 2004, 2005, 2006, 2007, 2008, 2010
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

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "hashtab.h"
#include "errors.h"
#include "read-md.h"

/* Associates PTR (which can be a string, etc.) with the file location
   specified by FILENAME and LINENO.  */
struct ptr_loc {
  const void *ptr;
  const char *filename;
  int lineno;
};

/* Obstack used for allocating MD strings.  */
struct obstack string_obstack;

/* A table of ptr_locs, hashed on the PTR field.  */
static htab_t ptr_locs;

/* An obstack for the above.  Plain xmalloc is a bit heavyweight for a
   small structure like ptr_loc.  */
static struct obstack ptr_loc_obstack;

/* A hash table of triples (A, B, C), where each of A, B and C is a condition
   and A is equivalent to "B && C".  This is used to keep track of the source
   of conditions that are made up of separate MD strings (such as the split
   condition of a define_insn_and_split).  */
static htab_t joined_conditions;

/* An obstack for allocating joined_conditions entries.  */
static struct obstack joined_conditions_obstack;

/* The file we are reading.  */
FILE *read_md_file;

/* The filename of READ_MD_FILE.  */
const char *read_md_filename;

/* The current line number in READ_MD_FILE.  */
int read_md_lineno;

/* A table of md_constant structures, hashed by name.  Null if no
   constant expansion should occur.  */
static htab_t md_constants;

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

/* Associate PTR with the file position given by FILENAME and LINENO.  */

static void
set_md_ptr_loc (const void *ptr, const char *filename, int lineno)
{
  struct ptr_loc *loc;

  loc = (struct ptr_loc *) obstack_alloc (&ptr_loc_obstack,
					  sizeof (struct ptr_loc));
  loc->ptr = ptr;
  loc->filename = filename;
  loc->lineno = lineno;
  *htab_find_slot (ptr_locs, loc, INSERT) = loc;
}

/* Return the position associated with pointer PTR.  Return null if no
   position was set.  */

static const struct ptr_loc *
get_md_ptr_loc (const void *ptr)
{
  return (const struct ptr_loc *) htab_find (ptr_locs, &ptr);
}

/* Associate NEW_PTR with the same file position as OLD_PTR.  */

void
copy_md_ptr_loc (const void *new_ptr, const void *old_ptr)
{
  const struct ptr_loc *loc = get_md_ptr_loc (old_ptr);
  if (loc != 0)
    set_md_ptr_loc (new_ptr, loc->filename, loc->lineno);
}

/* If PTR is associated with a known file position, print a #line
   directive for it.  */

void
print_md_ptr_loc (const void *ptr)
{
  const struct ptr_loc *loc = get_md_ptr_loc (ptr);
  if (loc != 0)
    printf ("#line %d \"%s\"\n", loc->lineno, loc->filename);
}

/* Return a condition that satisfies both COND1 and COND2.  Either string
   may be null or empty.  */

const char *
join_c_conditions (const char *cond1, const char *cond2)
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
  obstack_ptr_grow (&joined_conditions_obstack, result);
  obstack_ptr_grow (&joined_conditions_obstack, cond1);
  obstack_ptr_grow (&joined_conditions_obstack, cond2);
  entry = XOBFINISH (&joined_conditions_obstack, const void **);
  *htab_find_slot (joined_conditions, entry, INSERT) = entry;
  return result;
}

/* Print condition COND, wrapped in brackets.  If COND was created by
   join_c_conditions, recursively invoke this function for the original
   conditions and join the result with "&&".  Otherwise print a #line
   directive for COND if its original file position is known.  */

void
print_c_condition (const char *cond)
{
  const char **halves = (const char **) htab_find (joined_conditions, &cond);
  if (halves != 0)
    {
      printf ("(");
      print_c_condition (halves[1]);
      printf (" && ");
      print_c_condition (halves[2]);
      printf (")");
    }
  else
    {
      putc ('\n', stdout);
      print_md_ptr_loc (cond);
      printf ("(%s)", cond);
    }
}

/* A vfprintf-like function for reporting an error against line LINENO
   of the current MD file.  */

static void ATTRIBUTE_PRINTF(2,0)
message_with_line_1 (int lineno, const char *msg, va_list ap)
{
  fprintf (stderr, "%s:%d: ", read_md_filename, lineno);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);
}

/* A printf-like function for reporting an error against line LINENO
   in the current MD file.  */

void
message_with_line (int lineno, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  message_with_line_1 (lineno, msg, ap);
  va_end (ap);
}

/* Like message_with_line, but treat the condition as an error.  */

void
error_with_line (int lineno, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  message_with_line_1 (lineno, msg, ap);
  va_end (ap);
  have_error = 1;
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

  fprintf (stderr, "%s:%d: ", read_md_filename, read_md_lineno);
  vfprintf (stderr, msg, ap);
  putc ('\n', stderr);

  /* Gather some following context.  */
  for (i = 0; i < sizeof (context)-1; ++i)
    {
      c = read_char ();
      if (c == EOF)
	break;
      if (c == '\r' || c == '\n')
	break;
      context[i] = c;
    }
  context[i] = '\0';

  fprintf (stderr, "%s:%d: following context is `%s'\n",
	   read_md_filename, read_md_lineno, context);

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
	case '\n':
	  read_md_lineno++;
	  break;

	case ' ': case '\t': case '\f': case '\r':
	  break;

	case ';':
	  do
	    c = read_char ();
	  while (c != '\n' && c != EOF);
	  read_md_lineno++;
	  break;

	case '/':
	  {
	    int prevc;
	    c = read_char ();
	    if (c != '*')
	      fatal_expected_char ('*', c);

	    prevc = 0;
	    while ((c = read_char ()) && c != EOF)
	      {
		if (c == '\n')
		   read_md_lineno++;
	        else if (prevc == '*' && c == '/')
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

/* Read an rtx code name into NAME.  It is terminated by any of the
   punctuation chars of rtx printed syntax.  */

void
read_name (struct md_name *name)
{
  int c;
  size_t i;

  c = read_skip_spaces ();

  i = 0;
  while (1)
    {
      if (c == ' ' || c == '\n' || c == '\t' || c == '\f' || c == '\r'
	  || c == EOF)
	break;
      if (c == ':' || c == ')' || c == ']' || c == '"' || c == '/'
	  || c == '(' || c == '[')
	{
	  unread_char (c);
	  break;
	}

      if (i == sizeof (name->buffer) - 1)
	fatal_with_file_and_line ("name too long");
      name->buffer[i++] = c;

      c = read_char ();
    }

  if (i == 0)
    fatal_with_file_and_line ("missing name or number");
  if (c == '\n')
    read_md_lineno++;

  name->buffer[i] = 0;
  name->string = name->buffer;

  if (md_constants)
    {
      /* Do constant expansion.  */
      struct md_constant *def;

      do
	{
	  struct md_constant tmp_def;

	  tmp_def.name = name->string;
	  def = (struct md_constant *) htab_find (md_constants, &tmp_def);
	  if (def)
	    name->string = def->value;
	}
      while (def);
    }
}

/* Subroutine of the string readers.  Handles backslash escapes.
   Caller has read the backslash, but not placed it into the obstack.  */

static void
read_escape (void)
{
  int c = read_char ();

  switch (c)
    {
      /* Backslash-newline is replaced by nothing, as in C.  */
    case '\n':
      read_md_lineno++;
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
      obstack_1grow (&string_obstack, '\\');
      break;

      /* \; makes stuff for a C string constant containing
	 newline and tab.  */
    case ';':
      obstack_grow (&string_obstack, "\\n\\t", 4);
      return;

      /* pass anything else through, but issue a warning.  */
    default:
      fprintf (stderr, "%s:%d: warning: unrecognized escape \\%c\n",
	       read_md_filename, read_md_lineno, c);
      obstack_1grow (&string_obstack, '\\');
      break;
    }

  obstack_1grow (&string_obstack, c);
}

/* Read a double-quoted string onto the obstack.  Caller has scanned
   the leading quote.  */

char *
read_quoted_string (void)
{
  int c;

  while (1)
    {
      c = read_char (); /* Read the string  */
      if (c == '\n')
	read_md_lineno++;
      else if (c == '\\')
	{
	  read_escape ();
	  continue;
	}
      else if (c == '"' || c == EOF)
	break;

      obstack_1grow (&string_obstack, c);
    }

  obstack_1grow (&string_obstack, 0);
  return XOBFINISH (&string_obstack, char *);
}

/* Read a braced string (a la Tcl) onto the string obstack.  Caller
   has scanned the leading brace.  Note that unlike quoted strings,
   the outermost braces _are_ included in the string constant.  */

static char *
read_braced_string (void)
{
  int c;
  int brace_depth = 1;  /* caller-processed */
  unsigned long starting_read_md_lineno = read_md_lineno;

  obstack_1grow (&string_obstack, '{');
  while (brace_depth)
    {
      c = read_char (); /* Read the string  */

      if (c == '\n')
	read_md_lineno++;
      else if (c == '{')
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

      obstack_1grow (&string_obstack, c);
    }

  obstack_1grow (&string_obstack, 0);
  return XOBFINISH (&string_obstack, char *);
}

/* Read some kind of string constant.  This is the high-level routine
   used by read_rtx.  It handles surrounding parentheses, leading star,
   and dispatch to the appropriate string constant reader.  */

char *
read_string (int star_if_braced)
{
  char *stringbuf;
  int saw_paren = 0;
  int c, old_lineno;

  c = read_skip_spaces ();
  if (c == '(')
    {
      saw_paren = 1;
      c = read_skip_spaces ();
    }

  old_lineno = read_md_lineno;
  if (c == '"')
    stringbuf = read_quoted_string ();
  else if (c == '{')
    {
      if (star_if_braced)
	obstack_1grow (&string_obstack, '*');
      stringbuf = read_braced_string ();
    }
  else
    fatal_with_file_and_line ("expected `\"' or `{', found `%c'", c);

  if (saw_paren)
    {
      c = read_skip_spaces ();
      if (c != ')')
	fatal_expected_char (')', c);
    }

  set_md_ptr_loc (stringbuf, read_md_filename, old_lineno);
  return stringbuf;
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
  while (ISSPACE(*p))
    p++;

  if (*p == '\0')
    return NULL;

  start = p;

  while (*p != ',' && *p != '\0')
    p++;

  *pstr = p;
  return start;
}

/* Process a define_constants directive, starting with the optional space
   after the "define_constants".  */

void
read_constants (void)
{
  int c;
  htab_t defs;

  defs = md_constants;
  if (! defs)
    defs = htab_create (32, leading_string_hash,
			leading_string_eq_p, (htab_del) 0);

  c = read_skip_spaces ();
  if (c != '[')
    fatal_expected_char ('[', c);

  /* Disable constant expansion during definition processing.  */
  md_constants = 0;
  while ( (c = read_skip_spaces ()) != ']')
    {
      struct md_name name, value;
      struct md_constant *def, tmp_def;
      void **entry_ptr;

      if (c != '(')
	fatal_expected_char ('(', c);

      read_name (&name);
      read_name (&value);

      tmp_def.name = name.string;
      entry_ptr = htab_find_slot (defs, &tmp_def, INSERT);
      if (*entry_ptr)
	{
	  def = (struct md_constant *) *entry_ptr;
	  if (strcmp (def->value, value.string) != 0)
	    fatal_with_file_and_line ("redefinition of %s, was %s, now %s",
				      def->name, def->value, value.string);
	}
      else
	{
	  def = XNEW (struct md_constant);
	  def->name = xstrdup (name.string);
	  def->value = xstrdup (value.string);
	  *entry_ptr = def;
	}

      c = read_skip_spaces ();
      if (c != ')')
	fatal_expected_char (')', c);
    }
  md_constants = defs;
  c = read_skip_spaces ();
  if (c != ')')
    fatal_expected_char (')', c);
}

/* For every constant definition, call CALLBACK with two arguments:
   a pointer a pointer to the constant definition and INFO.
   Stop when CALLBACK returns zero.  */

void
traverse_md_constants (htab_trav callback, void *info)
{
  if (md_constants)
    htab_traverse (md_constants, callback, info);
}

/* Initialize this file's static data.  */

void
init_md_reader (void)
{
  obstack_init (&string_obstack);
  ptr_locs = htab_create (161, leading_ptr_hash, leading_ptr_eq_p, 0);
  obstack_init (&ptr_loc_obstack);
  joined_conditions = htab_create (161, leading_ptr_hash, leading_ptr_eq_p, 0);
  obstack_init (&joined_conditions_obstack);
}
