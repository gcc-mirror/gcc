/* RTL reader for GCC.
   Copyright (C) 1987, 1988, 1991, 1994, 1997, 1998, 1999, 2000, 2001, 2002,
   2003
   Free Software Foundation, Inc.

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

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "obstack.h"
#include "hashtab.h"

static htab_t md_constants;

static void fatal_with_file_and_line (FILE *, const char *, ...)
  ATTRIBUTE_PRINTF_2 ATTRIBUTE_NORETURN;
static void fatal_expected_char (FILE *, int, int) ATTRIBUTE_NORETURN;
static void read_name (char *, FILE *);
static char *read_string (struct obstack *, FILE *, int);
static char *read_quoted_string (struct obstack *, FILE *);
static char *read_braced_string (struct obstack *, FILE *);
static void read_escape (struct obstack *, FILE *);
static hashval_t def_hash (const void *);
static int def_name_eq_p (const void *, const void *);
static void read_constants (FILE *infile, char *tmp_char);
static void validate_const_int (FILE *, const char *);

/* Subroutines of read_rtx.  */

/* The current line number for the file.  */
int read_rtx_lineno = 1;

/* The filename for aborting with file and line.  */
const char *read_rtx_filename = "<unknown>";

static void
fatal_with_file_and_line (FILE *infile, const char *msg, ...)
{
  char context[64];
  size_t i;
  int c;
  va_list ap;

  va_start (ap, msg);

  fprintf (stderr, "%s:%d: ", read_rtx_filename, read_rtx_lineno);
  vfprintf (stderr, msg, ap);
  putc ('\n', stderr);

  /* Gather some following context.  */
  for (i = 0; i < sizeof (context)-1; ++i)
    {
      c = getc (infile);
      if (c == EOF)
	break;
      if (c == '\r' || c == '\n')
	break;
      context[i] = c;
    }
  context[i] = '\0';

  fprintf (stderr, "%s:%d: following context is `%s'\n",
	   read_rtx_filename, read_rtx_lineno, context);

  va_end (ap);
  exit (1);
}

/* Dump code after printing a message.  Used when read_rtx finds
   invalid data.  */

static void
fatal_expected_char (FILE *infile, int expected_c, int actual_c)
{
  fatal_with_file_and_line (infile, "expected character `%c', found `%c'",
			    expected_c, actual_c);
}

/* Read chars from INFILE until a non-whitespace char
   and return that.  Comments, both Lisp style and C style,
   are treated as whitespace.
   Tools such as genflags use this function.  */

int
read_skip_spaces (FILE *infile)
{
  int c;

  while (1)
    {
      c = getc (infile);
      switch (c)
	{
	case '\n':
	  read_rtx_lineno++;
	  break;

	case ' ': case '\t': case '\f': case '\r':
	  break;

	case ';':
	  do
	    c = getc (infile);
	  while (c != '\n' && c != EOF);
	  read_rtx_lineno++;
	  break;

	case '/':
	  {
	    int prevc;
	    c = getc (infile);
	    if (c != '*')
	      fatal_expected_char (infile, '*', c);

	    prevc = 0;
	    while ((c = getc (infile)) && c != EOF)
	      {
		if (c == '\n')
		   read_rtx_lineno++;
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

/* Read an rtx code name into the buffer STR[].
   It is terminated by any of the punctuation chars of rtx printed syntax.  */

static void
read_name (char *str, FILE *infile)
{
  char *p;
  int c;

  c = read_skip_spaces (infile);

  p = str;
  while (1)
    {
      if (c == ' ' || c == '\n' || c == '\t' || c == '\f' || c == '\r')
	break;
      if (c == ':' || c == ')' || c == ']' || c == '"' || c == '/'
	  || c == '(' || c == '[')
	{
	  ungetc (c, infile);
	  break;
	}
      *p++ = c;
      c = getc (infile);
    }
  if (p == str)
    fatal_with_file_and_line (infile, "missing name or number");
  if (c == '\n')
    read_rtx_lineno++;

  *p = 0;

  if (md_constants)
    {
      /* Do constant expansion.  */
      struct md_constant *def;

      p = str;
      do
	{
	  struct md_constant tmp_def;

	  tmp_def.name = p;
	  def = htab_find (md_constants, &tmp_def);
	  if (def)
	    p = def->value;
	} while (def);
      if (p != str)
	strcpy (str, p);
    }
}

/* Subroutine of the string readers.  Handles backslash escapes.
   Caller has read the backslash, but not placed it into the obstack.  */
static void
read_escape (struct obstack *ob, FILE *infile)
{
  int c = getc (infile);

  switch (c)
    {
      /* Backslash-newline is replaced by nothing, as in C.  */
    case '\n':
      read_rtx_lineno++;
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
      obstack_1grow (ob, '\\');
      break;

      /* \; makes stuff for a C string constant containing
	 newline and tab.  */
    case ';':
      obstack_grow (ob, "\\n\\t", 4);
      return;

      /* pass anything else through, but issue a warning.  */
    default:
      fprintf (stderr, "%s:%d: warning: unrecognized escape \\%c\n",
	       read_rtx_filename, read_rtx_lineno, c);
      obstack_1grow (ob, '\\');
      break;
    }

  obstack_1grow (ob, c);
}


/* Read a double-quoted string onto the obstack.  Caller has scanned
   the leading quote.  */
static char *
read_quoted_string (struct obstack *ob, FILE *infile)
{
  int c;

  while (1)
    {
      c = getc (infile); /* Read the string  */
      if (c == '\n')
	read_rtx_lineno++;
      else if (c == '\\')
	{
	  read_escape (ob, infile);
	  continue;
	}
      else if (c == '"')
	break;

      obstack_1grow (ob, c);
    }

  obstack_1grow (ob, 0);
  return obstack_finish (ob);
}

/* Read a braced string (a la Tcl) onto the obstack.  Caller has
   scanned the leading brace.  Note that unlike quoted strings,
   the outermost braces _are_ included in the string constant.  */
static char *
read_braced_string (struct obstack *ob, FILE *infile)
{
  int c;
  int brace_depth = 1;  /* caller-processed */
  unsigned long starting_read_rtx_lineno = read_rtx_lineno;

  obstack_1grow (ob, '{');
  while (brace_depth)
    {
      c = getc (infile); /* Read the string  */

      if (c == '\n')
	read_rtx_lineno++;
      else if (c == '{')
	brace_depth++;
      else if (c == '}')
	brace_depth--;
      else if (c == '\\')
	{
	  read_escape (ob, infile);
	  continue;
	}
      else if (c == EOF)
	fatal_with_file_and_line
	  (infile, "missing closing } for opening brace on line %lu",
	   starting_read_rtx_lineno);

      obstack_1grow (ob, c);
    }

  obstack_1grow (ob, 0);
  return obstack_finish (ob);
}

/* Read some kind of string constant.  This is the high-level routine
   used by read_rtx.  It handles surrounding parentheses, leading star,
   and dispatch to the appropriate string constant reader.  */

static char *
read_string (struct obstack *ob, FILE *infile, int star_if_braced)
{
  char *stringbuf;
  int saw_paren = 0;
  int c;

  c = read_skip_spaces (infile);
  if (c == '(')
    {
      saw_paren = 1;
      c = read_skip_spaces (infile);
    }

  if (c == '"')
    stringbuf = read_quoted_string (ob, infile);
  else if (c == '{')
    {
      if (star_if_braced)
	obstack_1grow (ob, '*');
      stringbuf = read_braced_string (ob, infile);
    }
  else
    fatal_with_file_and_line (infile, "expected `\"' or `{', found `%c'", c);

  if (saw_paren)
    {
      c = read_skip_spaces (infile);
      if (c != ')')
	fatal_expected_char (infile, ')', c);
    }

  return stringbuf;
}

/* Provide a version of a function to read a long long if the system does
   not provide one.  */
#if HOST_BITS_PER_WIDE_INT > HOST_BITS_PER_LONG && !defined(HAVE_ATOLL) && !defined(HAVE_ATOQ)
HOST_WIDE_INT atoll (const char *);

HOST_WIDE_INT
atoll (const char *p)
{
  int neg = 0;
  HOST_WIDE_INT tmp_wide;

  while (ISSPACE (*p))
    p++;
  if (*p == '-')
    neg = 1, p++;
  else if (*p == '+')
    p++;

  tmp_wide = 0;
  while (ISDIGIT (*p))
    {
      HOST_WIDE_INT new_wide = tmp_wide*10 + (*p - '0');
      if (new_wide < tmp_wide)
	{
	  /* Return INT_MAX equiv on overflow.  */
	  tmp_wide = (~(unsigned HOST_WIDE_INT) 0) >> 1;
	  break;
	}
      tmp_wide = new_wide;
      p++;
    }

  if (neg)
    tmp_wide = -tmp_wide;
  return tmp_wide;
}
#endif

/* Given a constant definition, return a hash code for its name.  */
static hashval_t
def_hash (const void *def)
{
  unsigned result, i;
  const char *string = ((const struct md_constant *) def)->name;

  for (result = i = 0;*string++ != '\0'; i++)
    result += ((unsigned char) *string << (i % CHAR_BIT));
  return result;
}

/* Given two constant definitions, return true if they have the same name.  */
static int
def_name_eq_p (const void *def1, const void *def2)
{
  return ! strcmp (((const struct md_constant *) def1)->name,
		   ((const struct md_constant *) def2)->name);
}

/* INFILE is a FILE pointer to read text from.  TMP_CHAR is a buffer suitable
   to read a name or number into.  Process a define_constants directive,
   starting with the optional space after the "define_constants".  */
static void
read_constants (FILE *infile, char *tmp_char)
{
  int c;
  htab_t defs;

  c = read_skip_spaces (infile);
  if (c != '[')
    fatal_expected_char (infile, '[', c);
  defs = md_constants;
  if (! defs)
    defs = htab_create (32, def_hash, def_name_eq_p, (htab_del) 0);
  /* Disable constant expansion during definition processing.  */
  md_constants = 0;
  while ( (c = read_skip_spaces (infile)) != ']')
    {
      struct md_constant *def;
      void **entry_ptr;

      if (c != '(')
	fatal_expected_char (infile, '(', c);
      def = xmalloc (sizeof (struct md_constant));
      def->name = tmp_char;
      read_name (tmp_char, infile);
      entry_ptr = htab_find_slot (defs, def, TRUE);
      if (! *entry_ptr)
	def->name = xstrdup (tmp_char);
      c = read_skip_spaces (infile);
      ungetc (c, infile);
      read_name (tmp_char, infile);
      if (! *entry_ptr)
	{
	  def->value = xstrdup (tmp_char);
	  *entry_ptr = def;
	}
      else
	{
	  def = *entry_ptr;
	  if (strcmp (def->value, tmp_char))
	    fatal_with_file_and_line (infile,
				      "redefinition of %s, was %s, now %s",
				      def->name, def->value, tmp_char);
	}
      c = read_skip_spaces (infile);
      if (c != ')')
	fatal_expected_char (infile, ')', c);
    }
  md_constants = defs;
  c = read_skip_spaces (infile);
  if (c != ')')
    fatal_expected_char (infile, ')', c);
}

/* For every constant definition, call CALLBACK with two arguments:
   a pointer a pointer to the constant definition and INFO.
   Stops when CALLBACK returns zero.  */
void
traverse_md_constants (htab_trav callback, void *info)
{
  if (md_constants)
    htab_traverse (md_constants, callback, info);
}

static void
validate_const_int (FILE *infile, const char *string)
{
  const char *cp;
  int valid = 1;

  cp = string;
  while (*cp && ISSPACE (*cp))
    cp++;
  if (*cp == '-' || *cp == '+')
    cp++;
  if (*cp == 0)
    valid = 0;
  for (; *cp; cp++)
    if (! ISDIGIT (*cp))
      valid = 0;
  if (!valid)
    fatal_with_file_and_line (infile, "invalid decimal constant \"%s\"\n", string);
}

/* Read an rtx in printed representation from INFILE
   and return an actual rtx in core constructed accordingly.
   read_rtx is not used in the compiler proper, but rather in
   the utilities gen*.c that construct C code from machine descriptions.  */

rtx
read_rtx (FILE *infile)
{
  int i, j;
  RTX_CODE tmp_code;
  const char *format_ptr;
  /* tmp_char is a buffer used for reading decimal integers
     and names of rtx types and machine modes.
     Therefore, 256 must be enough.  */
  char tmp_char[256];
  rtx return_rtx;
  int c;
  int tmp_int;
  HOST_WIDE_INT tmp_wide;

  /* Obstack used for allocating RTL objects.  */
  static struct obstack rtl_obstack;
  static int initialized;

  /* Linked list structure for making RTXs: */
  struct rtx_list
    {
      struct rtx_list *next;
      rtx value;		/* Value of this node.  */
    };

  if (!initialized) {
    obstack_init (&rtl_obstack);
    initialized = 1;
  }

again:
  c = read_skip_spaces (infile); /* Should be open paren.  */
  if (c != '(')
    fatal_expected_char (infile, '(', c);

  read_name (tmp_char, infile);

  tmp_code = UNKNOWN;

  if (! strcmp (tmp_char, "define_constants"))
    {
      read_constants (infile, tmp_char);
      goto again;
    }
  for (i = 0; i < NUM_RTX_CODE; i++)
    if (! strcmp (tmp_char, GET_RTX_NAME (i)))
      {
	tmp_code = (RTX_CODE) i;	/* get value for name */
	break;
      }

  if (tmp_code == UNKNOWN)
    fatal_with_file_and_line (infile, "unknown rtx code `%s'", tmp_char);

  /* (NIL) stands for an expression that isn't there.  */
  if (tmp_code == NIL)
    {
      /* Discard the closeparen.  */
      while ((c = getc (infile)) && c != ')')
	;

      return 0;
    }

  /* If we end up with an insn expression then we free this space below.  */
  return_rtx = rtx_alloc (tmp_code);
  format_ptr = GET_RTX_FORMAT (GET_CODE (return_rtx));

  /* If what follows is `: mode ', read it and
     store the mode in the rtx.  */

  i = read_skip_spaces (infile);
  if (i == ':')
    {
      read_name (tmp_char, infile);
      for (j = 0; j < NUM_MACHINE_MODES; j++)
	if (! strcmp (GET_MODE_NAME (j), tmp_char))
	  break;

      if (j == MAX_MACHINE_MODE)
	fatal_with_file_and_line (infile, "unknown mode `%s'", tmp_char);

      PUT_MODE (return_rtx, (enum machine_mode) j);
    }
  else
    ungetc (i, infile);

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (return_rtx)); i++)
    switch (*format_ptr++)
      {
	/* 0 means a field for internal use only.
	   Don't expect it to be present in the input.  */
      case '0':
	break;

      case 'e':
      case 'u':
	XEXP (return_rtx, i) = read_rtx (infile);
	break;

      case 'V':
	/* 'V' is an optional vector: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces (infile);
	ungetc (c, infile);
	if (c == ')')
	  {
	    XVEC (return_rtx, i) = 0;
	    break;
	  }
	/* Now process the vector.  */

      case 'E':
	{
	  /* Obstack to store scratch vector in.  */
	  struct obstack vector_stack;
	  int list_counter = 0;
	  rtvec return_vec = NULL_RTVEC;

	  c = read_skip_spaces (infile);
	  if (c != '[')
	    fatal_expected_char (infile, '[', c);

	  /* add expressions to a list, while keeping a count */
	  obstack_init (&vector_stack);
	  while ((c = read_skip_spaces (infile)) && c != ']')
	    {
	      ungetc (c, infile);
	      list_counter++;
	      obstack_ptr_grow (&vector_stack, read_rtx (infile));
	    }
	  if (list_counter > 0)
	    {
	      return_vec = rtvec_alloc (list_counter);
	      memcpy (&return_vec->elem[0], obstack_finish (&vector_stack),
		      list_counter * sizeof (rtx));
	    }
	  XVEC (return_rtx, i) = return_vec;
	  obstack_free (&vector_stack, NULL);
	  /* close bracket gotten */
	}
	break;

      case 'S':
	/* 'S' is an optional string: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces (infile);
	ungetc (c, infile);
	if (c == ')')
	  {
	    XSTR (return_rtx, i) = 0;
	    break;
	  }

      case 'T':
      case 's':
	{
	  char *stringbuf;

	  /* The output template slot of a DEFINE_INSN,
	     DEFINE_INSN_AND_SPLIT, or DEFINE_PEEPHOLE automatically
	     gets a star inserted as its first character, if it is
	     written with a brace block instead of a string constant.  */
	  int star_if_braced = (format_ptr[-1] == 'T');

	  stringbuf = read_string (&rtl_obstack, infile, star_if_braced);

	  /* For insn patterns, we want to provide a default name
	     based on the file and line, like "*foo.md:12", if the
	     given name is blank.  These are only for define_insn and
	     define_insn_and_split, to aid debugging.  */
	  if (*stringbuf == '\0'
	      && i == 0
	      && (GET_CODE (return_rtx) == DEFINE_INSN
		  || GET_CODE (return_rtx) == DEFINE_INSN_AND_SPLIT))
	    {
	      char line_name[20];
	      const char *fn = (read_rtx_filename ? read_rtx_filename : "rtx");
	      const char *slash;
	      for (slash = fn; *slash; slash ++)
		if (*slash == '/' || *slash == '\\' || *slash == ':')
		  fn = slash + 1;
	      obstack_1grow (&rtl_obstack, '*');
	      obstack_grow (&rtl_obstack, fn, strlen (fn));
	      sprintf (line_name, ":%d", read_rtx_lineno);
	      obstack_grow (&rtl_obstack, line_name, strlen (line_name)+1);
	      stringbuf = (char *) obstack_finish (&rtl_obstack);
	    }

	  if (star_if_braced)
	    XTMPL (return_rtx, i) = stringbuf;
	  else
	    XSTR (return_rtx, i) = stringbuf;
	}
	break;

      case 'w':
	read_name (tmp_char, infile);
	validate_const_int (infile, tmp_char);
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
	tmp_wide = atoi (tmp_char);
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
	tmp_wide = atol (tmp_char);
#else
	/* Prefer atoll over atoq, since the former is in the ISO C99 standard.
	   But prefer not to use our hand-rolled function above either.  */
#if defined(HAVE_ATOLL) || !defined(HAVE_ATOQ)
	tmp_wide = atoll (tmp_char);
#else
	tmp_wide = atoq (tmp_char);
#endif
#endif
#endif
	XWINT (return_rtx, i) = tmp_wide;
	break;

      case 'i':
      case 'n':
	read_name (tmp_char, infile);
	validate_const_int (infile, tmp_char);
	tmp_int = atoi (tmp_char);
	XINT (return_rtx, i) = tmp_int;
	break;

      default:
	fprintf (stderr,
		 "switch format wrong in rtl.read_rtx(). format was: %c.\n",
		 format_ptr[-1]);
	fprintf (stderr, "\tfile position: %ld\n", ftell (infile));
	abort ();
      }

  c = read_skip_spaces (infile);
  if (c != ')')
    fatal_expected_char (infile, ')', c);

  return return_rtx;
}
