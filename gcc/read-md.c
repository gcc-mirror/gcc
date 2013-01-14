/* MD reader for GCC.
   Copyright (C) 1987-2013 Free Software Foundation, Inc.

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

/* A singly-linked list of filenames.  */
struct file_name_list {
  struct file_name_list *next;
  const char *fname;
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

/* The name of the toplevel file that indirectly included READ_MD_FILE.  */
const char *in_fname;

/* The directory part of IN_FNAME.  NULL if IN_FNAME is a bare filename.  */
static char *base_dir;

/* The first directory to search.  */
static struct file_name_list *first_dir_md_include;

/* A pointer to the null terminator of the md include chain.  */
static struct file_name_list **last_dir_md_include_ptr = &first_dir_md_include;

/* This callback will be invoked whenever an md include directive is
   processed.  To be used for creation of the dependency file.  */
void (*include_callback) (const char *);

/* The current maximum length of directory names in the search path
   for include files.  (Altered as we get more of them.)  */
static size_t max_include_len;

/* A table of md_constant structures, hashed by name.  Null if no
   constant expansion should occur.  */
static htab_t md_constants;

/* A table of enum_type structures, hashed by name.  */
static htab_t enum_types;

static void handle_file (directive_handler_t);

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
   directive for it to OUTF.  */

void
fprint_md_ptr_loc (FILE *outf, const void *ptr)
{
  const struct ptr_loc *loc = get_md_ptr_loc (ptr);
  if (loc != 0)
    fprintf (outf, "#line %d \"%s\"\n", loc->lineno, loc->filename);
}

/* Special fprint_md_ptr_loc for writing to STDOUT.  */
void
print_md_ptr_loc (const void *ptr)
{
  fprint_md_ptr_loc (stdout, ptr);
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

/* Print condition COND to OUTF, wrapped in brackets.  If COND was created
   by join_c_conditions, recursively invoke this function for the original
   conditions and join the result with "&&".  Otherwise print a #line
   directive for COND if its original file position is known.  */

void
fprint_c_condition (FILE *outf, const char *cond)
{
  const char **halves = (const char **) htab_find (joined_conditions, &cond);
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
print_c_condition (const char *cond)
{
  fprint_c_condition (stdout, cond);
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
	{
	  unread_char (c);
	  break;
	}
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
      if (c == '\\')
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

/* Skip the rest of a construct that started at line LINENO and that
   is currently nested by DEPTH levels of parentheses.  */

void
read_skip_construct (int depth, int lineno)
{
  struct md_name name;
  int c;

  do
    {
      c = read_skip_spaces ();
      if (c == EOF)
	{
	  error_with_line (lineno, "unterminated construct");
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

static void
handle_constants (void)
{
  int c;
  htab_t defs;

  c = read_skip_spaces ();
  if (c != '[')
    fatal_expected_char ('[', c);

  /* Disable constant expansion during definition processing.  */
  defs = md_constants;
  md_constants = 0;
  while ( (c = read_skip_spaces ()) != ']')
    {
      struct md_name name, value;

      if (c != '(')
	fatal_expected_char ('(', c);

      read_name (&name);
      read_name (&value);
      add_constant (defs, xstrdup (name.string), xstrdup (value.string), 0);

      c = read_skip_spaces ();
      if (c != ')')
	fatal_expected_char (')', c);
    }
  md_constants = defs;
}

/* For every constant definition, call CALLBACK with two arguments:
   a pointer a pointer to the constant definition and INFO.
   Stop when CALLBACK returns zero.  */

void
traverse_md_constants (htab_trav callback, void *info)
{
  htab_traverse (md_constants, callback, info);
}

/* Return a malloc()ed decimal string that represents number NUMBER.  */

static char *
decimal_string (int number)
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

static void
handle_enum (int lineno, bool md_p)
{
  char *enum_name, *value_name;
  struct md_name name;
  struct enum_type *def;
  struct enum_value *ev;
  void **slot;
  int c;

  enum_name = read_string (false);
  slot = htab_find_slot (enum_types, &enum_name, INSERT);
  if (*slot)
    {
      def = (struct enum_type *) *slot;
      if (def->md_p != md_p)
	error_with_line (lineno, "redefining `%s' as a different type of enum",
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

  c = read_skip_spaces ();
  if (c != '[')
    fatal_expected_char ('[', c);

  while ((c = read_skip_spaces ()) != ']')
    {
      if (c == EOF)
	{
	  error_with_line (lineno, "unterminated construct");
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
      ev->def = add_constant (md_constants, value_name,
			      decimal_string (def->num_values), def);

      *def->tail_ptr = ev;
      def->tail_ptr = &ev->next;
      def->num_values++;
    }
}

/* Try to find the definition of the given enum.  Return null on failure.  */

struct enum_type *
lookup_enum_type (const char *name)
{
  return (struct enum_type *) htab_find (enum_types, &name);
}

/* For every enum definition, call CALLBACK with two arguments:
   a pointer to the constant definition and INFO.  Stop when CALLBACK
   returns zero.  */

void
traverse_enum_types (htab_trav callback, void *info)
{
  htab_traverse (enum_types, callback, info);
}

/* Process an "include" directive, starting with the optional space
   after the "include".  Read in the file and use HANDLE_DIRECTIVE
   to process each unknown directive.  LINENO is the line number on
   which the "include" occured.  */

static void
handle_include (int lineno, directive_handler_t handle_directive)
{
  const char *filename;
  const char *old_filename;
  int old_lineno;
  char *pathname;
  FILE *input_file, *old_file;

  filename = read_string (false);
  input_file = NULL;

  /* If the specified file name is absolute, skip the include stack.  */
  if (!IS_ABSOLUTE_PATH (filename))
    {
      struct file_name_list *stackp;

      /* Search the directory path, trying to open the file.  */
      for (stackp = first_dir_md_include; stackp; stackp = stackp->next)
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
      if (base_dir)
	pathname = concat (base_dir, filename, NULL);
      else
	pathname = xstrdup (filename);
      input_file = fopen (pathname, "r");
    }

  if (input_file == NULL)
    {
      free (pathname);
      error_with_line (lineno, "include file `%s' not found", filename);
      return;
    }

  /* Save the old cursor.  Note that the LINENO argument to this
     function is the beginning of the include statement, while
     read_md_lineno has already been advanced.  */
  old_file = read_md_file;
  old_filename = read_md_filename;
  old_lineno = read_md_lineno;

  if (include_callback)
    include_callback (pathname);

  read_md_file = input_file;
  read_md_filename = pathname;
  handle_file (handle_directive);

  /* Restore the old cursor.  */
  read_md_file = old_file;
  read_md_filename = old_filename;
  read_md_lineno = old_lineno;

  /* Do not free the pathname.  It is attached to the various rtx
     queue elements.  */
}

/* Process the current file, assuming that read_md_file and
   read_md_filename are valid.  Use HANDLE_DIRECTIVE to handle
   unknown directives.  */

static void
handle_file (directive_handler_t handle_directive)
{
  struct md_name directive;
  int c, lineno;

  read_md_lineno = 1;
  while ((c = read_skip_spaces ()) != EOF)
    {
      lineno = read_md_lineno;
      if (c != '(')
	fatal_expected_char ('(', c);

      read_name (&directive);
      if (strcmp (directive.string, "define_constants") == 0)
	handle_constants ();
      else if (strcmp (directive.string, "define_enum") == 0)
	handle_enum (lineno, true);
      else if (strcmp (directive.string, "define_c_enum") == 0)
	handle_enum (lineno, false);
      else if (strcmp (directive.string, "include") == 0)
	handle_include (lineno, handle_directive);
      else if (handle_directive)
	handle_directive (lineno, directive.string);
      else
	read_skip_construct (1, lineno);

      c = read_skip_spaces ();
      if (c != ')')
	fatal_expected_char (')', c);
    }
  fclose (read_md_file);
}

/* Like handle_file, but for top-level files.  Set up in_fname and
   base_dir accordingly.  */

static void
handle_toplevel_file (directive_handler_t handle_directive)
{
  const char *base;

  in_fname = read_md_filename;
  base = lbasename (in_fname);
  if (base == in_fname)
    base_dir = NULL;
  else
    base_dir = xstrndup (in_fname, base - in_fname);

  handle_file (handle_directive);
}

/* Parse a -I option with argument ARG.  */

static void
parse_include (const char *arg)
{
  struct file_name_list *dirtmp;

  dirtmp = XNEW (struct file_name_list);
  dirtmp->next = 0;
  dirtmp->fname = arg;
  *last_dir_md_include_ptr = dirtmp;
  last_dir_md_include_ptr = &dirtmp->next;
  if (strlen (dirtmp->fname) > max_include_len)
    max_include_len = strlen (dirtmp->fname);
}

/* The main routine for reading .md files.  Try to process all the .md
   files specified on the command line and return true if no error occured.

   ARGC and ARGV are the arguments to main.

   PARSE_OPT, if nonnull, is passed all unknown command-line arguments.
   It should return true if it recognizes the argument or false if a
   generic error should be reported.

   If HANDLE_DIRECTIVE is nonnull, the parser calls it for each
   unknown directive, otherwise it just skips such directives.
   See the comment above the directive_handler_t definition for
   details about the callback's interface.  */

bool
read_md_files (int argc, char **argv, bool (*parse_opt) (const char *),
	       directive_handler_t handle_directive)
{
  int i;
  bool no_more_options;
  bool already_read_stdin;
  int num_files;

  /* Initialize global data.  */
  obstack_init (&string_obstack);
  ptr_locs = htab_create (161, leading_ptr_hash, leading_ptr_eq_p, 0);
  obstack_init (&ptr_loc_obstack);
  joined_conditions = htab_create (161, leading_ptr_hash, leading_ptr_eq_p, 0);
  obstack_init (&joined_conditions_obstack);
  md_constants = htab_create (31, leading_string_hash,
			      leading_string_eq_p, (htab_del) 0);
  enum_types = htab_create (31, leading_string_hash,
			    leading_string_eq_p, (htab_del) 0);

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

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
	      parse_include (argv[i] + 2);
	    else if (++i < argc)
	      parse_include (argv[i]);
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

	      read_md_file = stdin;
	      read_md_filename = "<stdin>";
	      handle_toplevel_file (handle_directive);
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
      read_md_filename = argv[i];
      read_md_file = fopen (read_md_filename, "r");
      if (read_md_file == 0)
	{
	  perror (read_md_filename);
	  return false;
	}
      handle_toplevel_file (handle_directive);
      num_files++;
    }

  /* If we get to this point without having seen any files to process,
     read the standard input now.  */
  if (num_files == 0 && !already_read_stdin)
    {
      read_md_file = stdin;
      read_md_filename = "<stdin>";
      handle_toplevel_file (handle_directive);
    }

  return !have_error;
}
