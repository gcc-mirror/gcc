/* Context-dependent ALGOL 68 tokeniser.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Context-dependent ALGOL 68 tokeniser.  */


#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "options.h"
#include "vec.h"

#include "a68.h"

/* A few forward references of static functions defined in this file.  */

static void include_files (LINE_T *top);

/* Standard prelude and postlude for source files.

   These are used for particular programs only.  Not for prelude packets.
   We need several versions for the several supported stropping regimes.  */

static const char *
upper_prelude_start[] = {
  "BEGIN",
  "      BEGIN",
  NO_TEXT
};

static const char *
upper_postlude[] = {
  "      END;",
  "      stop: SKIP",
  "END",
  NO_TEXT
};

static const char *
supper_prelude_start[] = {
  "begin",
  "     begin",
  NO_TEXT
};

static const char *
supper_postlude[] = {
  "      end;",
  "      stop: skip",
  "end",
  NO_TEXT
};

/* Macros.  */

#define NULL_CHAR '\0'
#define STOP_CHAR 127
#define FORMFEED_CHAR '\f'
#define CR_CHAR '\r'
#define QUOTE_CHAR '"'
#define APOSTROPHE_CHAR '\''
#define BACKSLASH_CHAR '\\'
#define NEWLINE_CHAR '\n'
#define EXPONENT_CHAR 'e'
#define RADIX_CHAR 'r'
#define POINT_CHAR '.'
#define TAB_CHAR '\t'

#define MAX_RESTART 256

#define EOL(c) ((c) == NEWLINE_CHAR || (c) == NULL_CHAR)
#define SCAN_ERROR(c, u, v, txt) if (c)		\
    do						\
      {						\
	a68_scan_error (u, v, txt);		\
      }						\
    while (0)


#define SCAN_DIGITS(c)				\
  while (ISDIGIT (c))				\
    {						\
      (sym++)[0] = (c);				\
      (c) = next_char (ref_l, ref_s, true);	\
    }

#define SCAN_EXPONENT_PART(c)						\
  do									\
    {									\
      (sym++)[0] = EXPONENT_CHAR;					\
      (c) = next_char (ref_l, ref_s, true);				\
      if ((c) == '+' || (c) == '-') {					\
	(sym++)[0] = (c);						\
	(c) = next_char (ref_l, ref_s, true);				\
      }									\
      SCAN_ERROR (!ISDIGIT (c), *start_l, *start_c,			\
		  "invalid exponent digit");				\
      SCAN_DIGITS (c);							\
    }									\
  while (0)

/* Get the size of a file given a file descriptor FD.  In case the size of
   the file cannot be determined then this function returns -1.  */

ssize_t
a68_file_size (int fd)
{
  ssize_t fsize;
  off_t off, save;

  save = lseek (fd, 0, SEEK_CUR);
  if (save == (off_t) -1)
    return -1;

  off = lseek (fd, 0, SEEK_END);
  if (off == (off_t) -1)
    return -1;
  fsize = (ssize_t) off;

  off = lseek (fd, save, SEEK_SET);
  if (off == (off_t) -1)
    return -1;

  return fsize;
}

/* Read bytes from file into buffer given a file descriptor.  */

ssize_t
a68_file_read (int fd, void *buf, size_t n)
{
  size_t to_do = n;
  int restarts = 0;
  char *z = (char *) buf;
  while (to_do > 0)
    {
      ssize_t bytes_read;

      errno = 0;
      bytes_read = read (fd, z, to_do);
      if (bytes_read < 0)
	{
	  if (errno == EINTR)
	    {
	      /* interrupt, retry.  */
	      bytes_read = 0;
	      if (restarts++ > MAX_RESTART)
		{
		  return -1;
		}
	    }
	  else
	    {
	      /* read error.  */
	      return -1;
	    }
	}
      else if (bytes_read == 0)
	{
	  /* EOF_CHAR */
	  break;
	}
      to_do -= (size_t) bytes_read;
      z += bytes_read;
    }

  /* return >= 0  */
  return (ssize_t) n - (ssize_t) to_do;
}

/* Save scanner state, for character look-ahead.  */

static void
save_state (LINE_T *ref_l, char *ref_s, char ch)
{
  SCAN_STATE_L (&A68_JOB) = ref_l;
  SCAN_STATE_S (&A68_JOB) = ref_s;
  SCAN_STATE_C (&A68_JOB) = ch;
}

/* Restore scanner state, for character look-ahead.  */

static void
restore_state (LINE_T **ref_l, char **ref_s, char *ch)
{
  *ref_l = SCAN_STATE_L (&A68_JOB);
  *ref_s = SCAN_STATE_S (&A68_JOB);
  *ch = SCAN_STATE_C (&A68_JOB);
}

/* New_source_line.  */

static LINE_T *
new_source_line (void)
{
  LINE_T *z = ggc_cleared_alloc<LINE_T> ();

  MARKER (z)[0] = '\0';
  STRING (z) = NO_TEXT;
  FILENAME (z) = NO_TEXT;
  NUMBER (z) = 0;
  NEXT (z) = NO_LINE;
  PREVIOUS (z) = NO_LINE;
  return z;
}

/* Append a source line to the internal source file.  */

static void
append_source_line (const char *str, LINE_T **ref_l, int *line_num,
		    const char *filename)
{
  LINE_T *z = new_source_line ();

  /* Link line into the chain.  */
  STRING (z) = xstrdup (str);
  FILENAME (z) = ggc_strdup (filename);
  NUMBER (z) = (*line_num)++;
  NEXT (z) = NO_LINE;
  PREVIOUS (z) = *ref_l;
  if (TOP_LINE (&A68_JOB) == NO_LINE)
    TOP_LINE (&A68_JOB) = z;
  if (*ref_l != NO_LINE)
    NEXT (*ref_l) = z;
  *ref_l = z;
}

/* Append environment source lines.  */

static void
append_environ (const char *str[], LINE_T **ref_l, int *line_num, const char *name)
{
  for (int k = 0; str[k] != NO_TEXT; k++)
    {
      int zero_line_num = 0;
      (*line_num)++;
      append_source_line (str[k], ref_l, &zero_line_num, name);
    }
}

/*
 * Scanner, tokenises the source code.
 */

/* Emit a diagnostic if CH is an unworthy character.  */

static void
unworthy (LINE_T *u, char *v, char ch)
{
  if (ISPRINT (ch))
    {
      if (snprintf (A68 (edit_line), SNPRINTF_SIZE, "*%s",
		    "unworthy character") < 0)
	gcc_unreachable ();
    }
  else
    {
      if (snprintf (A68 (edit_line), SNPRINTF_SIZE, "*%s %c",
		    "unworthy character", ch) < 0)
	gcc_unreachable ();
    }

  a68_scan_error (u, v, A68 (edit_line));
}

/* Concatenate lines that terminate in '\' with next line.  */

static void
concatenate_lines (LINE_T * top)
{
  LINE_T *q;
  /* Work from bottom backwards.  */
  for (q = top; q != NO_LINE && NEXT (q) != NO_LINE; FORWARD (q))
    ;

  for (; q != NO_LINE; BACKWARD (q))
    {
      char *z = STRING (q);
      size_t len = strlen (z);

      if (len >= 2
	  && z[len - 2] == BACKSLASH_CHAR
	  && z[len - 1] == NEWLINE_CHAR
	  && NEXT (q) != NO_LINE
	  && STRING (NEXT (q)) != NO_TEXT)
	{
	  z[len - 2] = '\0';
	  len += (int) strlen (STRING (NEXT (q)));
	  z = (char *) xmalloc (len + 1);
	  a68_bufcpy (z, STRING (q), len + 1);
	  a68_bufcat (z, STRING (NEXT (q)), len + 1);
	  STRING (NEXT (q))[0] = '\0';
	  STRING (q) = z;
	}
    }
}

/* Read source file FILENAME and make internal copy.  */

static bool
read_source_file (const char *filename)
{
  struct stat statbuf;
  LINE_T *ref_l = NO_LINE;
  int line_num = 0;
  size_t k;
  size_t bytes_read;
  ssize_t l;
  size_t source_file_size;
  char *buffer;
  FILE *f;
  bool ret = true;

  /* First open the given file.  */
  if (!(FILE_SOURCE_FD (&A68_JOB) = fopen (filename, "r")))
    fatal_error (UNKNOWN_LOCATION, "could not open source file %s",
		 filename);
  FILE_SOURCE_NAME (&A68_JOB) = ggc_strdup (filename);
  f = FILE_SOURCE_FD (&A68_JOB);

  if (fstat (fileno (f), &statbuf)
      || !(S_ISREG (statbuf.st_mode) || S_ISCHR (statbuf.st_mode)))
    fatal_error (UNKNOWN_LOCATION, "specified file %s is a directory",
		 filename);

  l = a68_file_size (fileno (f));
  if (l < 0)
    error ("could not get size of source file");
  source_file_size = l;

  /* Allocate A68_PARSER (scan_buf), which is an auxiliary buffer used by the
     scanner known to be big enough to hold any string contained in the source
     file.  */
  A68_PARSER (max_scan_buf_length) = source_file_size + 1;
  A68_PARSER (max_scan_buf_length) += 1024; /* For the environment.  */
  A68_PARSER (scan_buf) = (char *) xmalloc (A68_PARSER (max_scan_buf_length));

  /* Prelude.  */
  append_environ (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING
		  ? upper_prelude_start : supper_prelude_start,
		  &ref_l, &line_num, "prelude");

  /* Read the file into a single buffer, so we save on system calls.  */
  line_num = 1;
  buffer = (char *) xmalloc (8 + source_file_size);
  bytes_read = a68_file_read (fileno (f), buffer, source_file_size);
  gcc_assert (bytes_read == source_file_size);

  /* Link all lines into the list.  */
  k = 0;
  while (k < source_file_size)
    {
      l = 0;
      A68_PARSER (scan_buf)[0] = '\0';
      while (k < source_file_size  && buffer[k] != NEWLINE_CHAR)
	{
	  if (k < source_file_size - 1
	      && buffer[k] == CR_CHAR && buffer[k + 1] == NEWLINE_CHAR)
	    k++;
	  else
	    {
	      A68_PARSER (scan_buf)[l++] = buffer[k++];
	      A68_PARSER (scan_buf)[l] = '\0';
	    }
	}
      A68_PARSER (scan_buf)[l++] = NEWLINE_CHAR;
      A68_PARSER (scan_buf)[l] = '\0';
      if (k < source_file_size)
	k++;
      append_source_line (A68_PARSER (scan_buf), &ref_l, &line_num,
			  FILE_SOURCE_NAME (&A68_JOB));
      SCAN_ERROR (l != (ssize_t) strlen (A68_PARSER (scan_buf)),
		  NO_LINE, NO_TEXT, "invalid characters in source file");
    }

  /* Postlude.  */
  append_environ (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING
		  ? upper_postlude : supper_postlude,
		  &ref_l, &line_num, "postlude");

  /* Concatenate lines that end with \.  */
  concatenate_lines (TOP_LINE (&A68_JOB));

  /* Include files.  */
  include_files (TOP_LINE (&A68_JOB));

  if (fclose (FILE_SOURCE_FD (&A68_JOB)) != 0)
    gcc_unreachable ();
  return ret;
}

/* Get next character from internal copy of source file.

   If ALLOW_TYPO is true then typographical display features are skipped.

   If ALLOW_ONE_UNDER is true then a single underscore character is
   skipped.  */

static char
next_char (LINE_T **ref_l, char **ref_s, bool allow_typo,
	   bool allow_one_under = false, bool *found_under = NULL)
{
  char ch;

  /* Empty source.  */
  if (*ref_l == NO_LINE)
    return STOP_CHAR;

  if ((*ref_s)[0] == NEWLINE_CHAR || (*ref_s)[0] == '\0')
    {
      /* Go to new line.  */
      *ref_l = NEXT (*ref_l);
      if (*ref_l == NO_LINE)
        return STOP_CHAR;
      *ref_s = STRING (*ref_l);
    }
  else
    (*ref_s)++;

  /* Deliver next char.  */
  ch = (*ref_s)[0];
  if ((allow_typo && (ISSPACE (ch) || ch == FORMFEED_CHAR))
      || (allow_one_under && ch == '_'))
    {
      if (ch == '_' && found_under != NULL)
	*found_under = true;
      ch = next_char (ref_l, ref_s, allow_typo);
    }
  return ch;
}

/* Find first character that can start a valid symbol.  */

static void
get_good_char (char *ref_c, LINE_T **ref_l, char **ref_s)
{
  while (*ref_c != STOP_CHAR && (ISSPACE (*ref_c) || (*ref_c == '\0')))
    *ref_c = next_char (ref_l, ref_s, false);
}

/* Case insensitive strncmp for at most the number of chars in V.  */

static int
streq (const char *u, const char *v)
{
  int diff;
  for (diff = 0; diff == 0 && u[0] != NULL_CHAR && v[0] != NULL_CHAR; u++, v++)
    diff = ((int) TOLOWER (u[0])) - ((int) TOLOWER (v[0]));
  return diff;
}

/* Case insensitive strncmp for at most N chars.  */

static int
strneq (const char *u, const char *v, size_t n)
{
  int diff;
  size_t pos = 0;
  for (diff = 0;
       diff == 0 && u[0] != NULL_CHAR && v[0] != NULL_CHAR && pos < n;
       u++, v++, pos++)
    diff = ((int) TOLOWER (u[0])) - ((int) TOLOWER (v[0]));
  return diff;
}


/* Determine whether u is bold tag v, independent of stropping regime.  */

static bool
is_bold (char *u, const char *v)
{
  size_t len = strlen (v);

  if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING)
    /* UPPER stropping.  */
    return strncmp (u, v, len) == 0 && !ISUPPER (u[len]);
  else
    /* SUPPER stropping.  */
    return (strlen (u) >= len
	    && ISLOWER (u[0])
	    && strneq (u, v, len) == 0
	    && !ISALPHA (u[len])
	    && !ISDIGIT (u[len]));
}

/* Skip a string denotation.

   This function returns true if the end of the string denotation is found.
   Returns false otherwise.  */

static bool
skip_string (LINE_T **top, char **ch)
{
  LINE_T *u = *top;
  char *v = *ch;
  v++;
  while (u != NO_LINE)
    {
      while (v[0] != NULL_CHAR)
	{
	  if (v[0] == QUOTE_CHAR && v[1] != QUOTE_CHAR)
	    {
	      *top = u;
	      *ch = &v[1];
	      return true;
	    }
	  else if (v[0] == QUOTE_CHAR && v[1] == QUOTE_CHAR)
	    {
	      v += 2;
	    }
	  else
	    {
	      v++;
	    }
	}
      FORWARD (u);
      if (u != NO_LINE) {
	v = &(STRING (u)[0]);
      } else {
	v = NO_TEXT;
      }
    }
  return false;
}

/* Skip a comment.

   This function returns true if the end of the comment is found.  Returns
   false otherwise.  */

static bool
skip_comment (LINE_T **top, char **ch, int delim)
{
  LINE_T *u = *top;
  char *v = *ch;
  int nesting_level = 1;
  v++;
  while (u != NO_LINE)
    {
      while (v[0] != NULL_CHAR)
	{
	  LINE_T *l = u;
	  char *c = v;

	  if (v[0] == QUOTE_CHAR && skip_string (&l, &c)
	      && (delim == BOLD_COMMENT_BEGIN_SYMBOL || delim == BRIEF_COMMENT_BEGIN_SYMBOL))
	    {
	      u = l;
	      v = c;
	    }
	  else if (is_bold (v, "COMMENT") && delim == BOLD_COMMENT_SYMBOL)
	    {
	      *top = u;
	      *ch = &v[1];
	      return true;
	    }
	  else if (is_bold (v, "CO") && delim == STYLE_I_COMMENT_SYMBOL)
	    {
	      *top = u;
	      *ch = &v[1];
	      return true;
	    }
	  else if (v[0] == '#' && delim == STYLE_II_COMMENT_SYMBOL)
	    {
	      *top = u;
	      *ch = &v[1];
	      return true;
	    }
	  else if (is_bold (v, "ETON") && delim == BOLD_COMMENT_BEGIN_SYMBOL)
	    {
	      gcc_assert (nesting_level > 0);
	      nesting_level -= 1;
	      if (nesting_level == 0)
		{
		  *top = u;
		  *ch = &v[1];
		  return true;
		}
	    }
	  else if (v[0] == '}' && delim == BRIEF_COMMENT_BEGIN_SYMBOL)
	    {
	      gcc_assert (nesting_level > 0);
	      nesting_level -= 1;
	      if (nesting_level == 0)
		{
		  *top = u;
		  *ch = &v[1];
		  return true;
		}
	    }
	  else
	    {
	      if ((is_bold (v, "NOTE") && delim == BOLD_COMMENT_BEGIN_SYMBOL)
		  || (v[0] == '{' && delim == BRIEF_COMMENT_BEGIN_SYMBOL))
		{
		  nesting_level += 1;
		}

	      v++;
	    }
	}
      FORWARD (u);
      if (u != NO_LINE)
	v = &(STRING (u)[0]);
      else
	v = NO_TEXT;
    }

  return false;
}

/* Skip rest of pragmat.

   This function returns true if the end of the pragmat is found, false
   otherwise.  */

static bool
skip_pragmat (LINE_T **top, char **ch, int delim, bool whitespace)
{
  LINE_T *u = *top;
  char *v = *ch;
  while (u != NO_LINE)
    {
      while (v[0] != NULL_CHAR)
	{
	  if (is_bold (v, "PRAGMAT") && delim == BOLD_PRAGMAT_SYMBOL)
	    {
	      *top = u;
	      *ch = &v[1];
	      return true;
	    }
	  else if (is_bold (v, "PR") && delim == STYLE_I_PRAGMAT_SYMBOL)
	    {
	      *top = u;
	      *ch = &v[1];
	      return true;
	    }
	  else
	    {
	      if (whitespace && !ISSPACE (v[0]) && v[0] != NEWLINE_CHAR)
		{
		  SCAN_ERROR (true, u, v, "error in pragment");
		}
	      else if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING && ISUPPER (v[0]))
		{
		  /* Skip a bold word as you may trigger on REPR, for
		     instance.  */
		  while (ISUPPER (v[0]))
		    v++;
		}
	      else if (OPTION_STROPPING (&A68_JOB) == SUPPER_STROPPING && ISLOWER (v[0]))
		{
		  /* Skip a tag as you may trigger on expr, for instance.  */
		  while (ISLOWER (v[0]) || ISDIGIT (v[0]) || v[0] == '_')
		    v++;
		}
	      else
		{
		  v++;
		}
	    }
	}

      FORWARD (u);
      if (u != NO_LINE)
	v = &(STRING (u)[0]);
      else
	v = NO_TEXT;
    }

  return false;
}

/* Return pointer to next token within pragmat.  */

static char *
get_pragmat_item (LINE_T **top, char **ch)
{
  LINE_T *u = *top;
  char *v = *ch;
  while (u != NO_LINE)
    {
      while (v[0] != NULL_CHAR)
	{
	  if (!ISSPACE (v[0]) && v[0] != NEWLINE_CHAR)
	    {
	      *top = u;
	      *ch = v;
	      return v;
	    }
	  else
	    {
	      v++;
	    }
	}
      FORWARD (u);
      if (u != NO_LINE)
	v = &(STRING (u)[0]);
      else
	v = NO_TEXT;
  }

  return NO_TEXT;
}

/* Scan for the next pragmat and yield the first item within it.  */

static char *
next_preprocessor_item (LINE_T **top, char **ch, int *delim)
{
  LINE_T *u = *top;
  char *v = *ch;
  *delim = 0;
  while (u != NO_LINE)
    {
      while (v[0] != NULL_CHAR)
	{
	  LINE_T *start_l = u;
	  char *start_c = v;

	  if (v[0] == QUOTE_CHAR)
	    {
	      /* Skip string denotation.  */
	      SCAN_ERROR (!skip_string (&u, &v), start_l, start_c,
			  "unterminated string");
	    }
	  else if (a68_find_keyword (A68 (top_keyword), "COMMENT") != NO_KEYWORD
		   && is_bold (v, "COMMENT"))
	    {
	      /* Skip comment.  */
	      SCAN_ERROR (!skip_comment (&u, &v, BOLD_COMMENT_SYMBOL), start_l, start_c,
			  "unterminated comment");
	    }
	  else if (a68_find_keyword (A68 (top_keyword), "CO") != NO_KEYWORD
		   && is_bold (v, "CO"))
	    {
	      /* skip comment.  */
	      SCAN_ERROR (!skip_comment (&u, &v, STYLE_I_COMMENT_SYMBOL), start_l, start_c,
			  "unterminated comment");
	    }
	  else if (a68_find_keyword (A68 (top_keyword), "#") != NO_KEYWORD
		   && v[0] == '#')
	    {
	      SCAN_ERROR (!skip_comment (&u, &v, STYLE_II_COMMENT_SYMBOL), start_l, start_c,
			  "unterminated comment");
	    }
	  else if (a68_find_keyword (A68 (top_keyword), "NOTE") != NO_KEYWORD
		   && is_bold (v, "NOTE"))
	    {
	      SCAN_ERROR (!skip_comment (&u, &v, BOLD_COMMENT_BEGIN_SYMBOL), start_l, start_c,
			  "unterminated comment");
	    }
	  else if (a68_find_keyword (A68 (top_keyword), "{") != NO_KEYWORD
		   && v[0] == '{')
	    {
	      SCAN_ERROR (!skip_comment (&u, &v, BRIEF_COMMENT_BEGIN_SYMBOL), start_l, start_c,
			  "unterminated comment");
	    }
	  else if (is_bold (v, "PRAGMAT") || is_bold (v, "PR"))
	    {
	      /* We caught a PRAGMAT.  */
	      char *item;
	      if (is_bold (v, "PRAGMAT"))
		{
		  *delim = BOLD_PRAGMAT_SYMBOL;
		  v = &v[strlen ("PRAGMAT")];
		}
	      else if (is_bold (v, "PR"))
		{
		  *delim = STYLE_I_PRAGMAT_SYMBOL;
		  v = &v[strlen ("PR")];
		}
	      item = get_pragmat_item (&u, &v);
	      SCAN_ERROR (item == NO_TEXT, start_l, start_c,
			  "unterminated pragmat");

	      if (streq (item, "INCLUDE") == 0)
		{
		  /* Item "INCLUDE" includes a file.  */
		  *top = u;
		  *ch = v;
		  return item;
		}
	      else
		{
		  /* Unrecognised item - probably options handled later by the
		     tokeniser.  */
		  SCAN_ERROR (!skip_pragmat (&u, &v, *delim, false), start_l, start_c,
			      "unterminated pragmat");
		}
	    }
	  else if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING && ISUPPER (v[0]))
	    {
	      /* Skip a bold word as you may trigger on REPR, for instance.  */
	      while (ISUPPER (v[0]))
		v++;
	    }
	  else if (OPTION_STROPPING (&A68_JOB) == SUPPER_STROPPING && ISLOWER (v[0]))
	    {
	      /* Skip a tag as you may trigger on expr, for instance.  */
	      while (ISLOWER (v[0]) || ISDIGIT (v[0]) || v[0] == '_')
		v++;
	    }
	  else
	    {
	      v++;
	    }
	}

      FORWARD (u);
      if (u != NO_LINE)
	v = &(STRING (u)[0]);
      else
	v = NO_TEXT;
    }

  *top = u;
  *ch = v;
  return NO_TEXT;
}

/* Concatenate the two paths P1 and P2.  */

static char *
a68_relpath (const char *p1, const char *p2, const char *fn)
{
#if defined(__GNU__)
  /* The Hurd doesn't define PATH_MAX.  */
# define PATH_MAX 4096
#endif

  char q[PATH_MAX + 1];
  a68_bufcpy (q, p1, PATH_MAX);
  a68_bufcat (q, "/", PATH_MAX);
  a68_bufcat (q, p2, PATH_MAX);
  a68_bufcat (q, "/", PATH_MAX);
  a68_bufcat (q, fn, PATH_MAX);
  /* Home directory shortcut ~ is a shell extension.  */
  if (strchr (q, '~') != NO_TEXT) {
    return NO_TEXT;
  }
  char *r = (char *) xmalloc (PATH_MAX + 1);
  gcc_assert (r != NULL);
  /*  Error handling in the caller!  */
  errno = 0;
  r = lrealpath (q);
  return r;
}

/* Return true if we can open the file for reading.  False otherwise.  */

static bool
file_read_p (const char *filename)
{
  return access (filename, R_OK) == 0 ? true : false;
}

/* Find a file to include into the current source being parsed.  Search the file
   system for FILENAME and return a string with the file path.  If the file is
   not found, return NULL.

   When FILENAME is not an absolute path we first try to find it relative to the
   current file being parsed (CURFILE). Failing to do that we use the search
   paths provided by the -I option.  */

static char *
find_include_file (const char *curfile, const char *filename)
{
  char *filepath = NO_TEXT;
  char *tmpfpath = NO_TEXT;
  char *fnbdir = ldirname (filename);
  const char *incfile = lbasename (filename);

  if (fnbdir == NULL || incfile == NULL)
    gcc_unreachable ();

  if (!IS_ABSOLUTE_PATH (filename))
    {
      char *sourcedir = ldirname (curfile);

      if (sourcedir == NULL || fnbdir == NULL)
	gcc_unreachable ();

      if (strlen (sourcedir) == 0 && strlen (fnbdir) == 0)
	{
	  free (sourcedir);
	  sourcedir = (char *) xmalloc (2);
	  a68_bufcpy (sourcedir, ".", 2);
	}

      tmpfpath = a68_relpath (sourcedir, fnbdir, incfile);
      if (file_read_p (tmpfpath))
	{
	  filepath = tmpfpath;
	  goto cleanup;
	}

      for (unsigned ix = 0; ix != vec_safe_length (A68_INCLUDE_PATHS); ix++)
	{
	  const char *include_dir = (*(A68_INCLUDE_PATHS))[ix];
	  tmpfpath = a68_relpath (include_dir, fnbdir, incfile);
	  if (!IS_ABSOLUTE_PATH (tmpfpath))
	    tmpfpath = a68_relpath (sourcedir, fnbdir, incfile);
	  if (file_read_p (tmpfpath))
	    {
	      filepath = tmpfpath;
	      goto cleanup;
	    }
	}

    cleanup:
      free (sourcedir);
      goto end;
    }
  else
    {
      size_t fnwid = (int) strlen (filename) + 1;
      tmpfpath = (char *) xmalloc ((size_t) fnwid);
      a68_bufcpy (tmpfpath, filename, fnwid);

      if (file_read_p (tmpfpath))
	{
	  filepath = tmpfpath;
	  goto end;
	}
    }

end:
  free (fnbdir);
  return filepath;
}

/* Include files.
   This function handles the INCLUDE pragmat in the source file.  */

static void
include_files (LINE_T *top)
{
  /* syntax: PR include "filename" PR

     The file gets inserted before the line containing the pragmat. In this way
     correct line numbers are preserved which helps diagnostics. A file that
     has been included will not be included a second time - it will be ignored.
     A rigorous fail-safe, but there is no mechanism to prevent recursive
     includes in A68 source code. User reports do not indicate sophisticated
     use of INCLUDE, so this is fine for now.
  */

  bool make_pass = true;
  while (make_pass)
    {
      LINE_T *s, *t, *u = top;
      char *v = &(STRING (u)[0]);
      make_pass = false;
      errno = 0;
      while (u != NO_LINE)
	{
	  int pr_lim;
	  char *item = next_preprocessor_item (&u, &v, &pr_lim);
	  LINE_T *start_l = u;
	  char *start_c = v;
	  /* Search for PR include "filename" PR.  */
	  if (item != NO_TEXT && streq (item, "INCLUDE") == 0)
	    {
	      FILE *fp;
	      size_t fsize, k;
	      ssize_t ssize;
	      int n, linum, bytes_read;
	      char *fbuf, delim;
	      BUFFER fnb;
	      char *fn = NO_TEXT;
	      /* Skip to filename.  */
	      while (ISALPHA (v[0]))
		v++;
	      while (ISSPACE (v[0]))
		v++;
	      /* Scan quoted filename.  */
	      SCAN_ERROR ((v[0] != QUOTE_CHAR && v[0] != '\''), start_l, start_c,
			  "incorrect filename");
	      delim = (v++)[0];
	      n = 0;
	      fnb[0] = NULL_CHAR;
	      /* Scan Algol 68 string (note: "" denotes a ", while in C it
		 concatenates).  */
	      do
		{
		  SCAN_ERROR (EOL (v[0]), start_l, start_c,
			      "incorrect filename");
		  SCAN_ERROR (n == BUFFER_SIZE - 1, start_l, start_c,
			      "incorrect filename");
		  if (v[0] == delim)
		    {
		      while (v[0] == delim && v[1] == delim)
			{
			  SCAN_ERROR (n == BUFFER_SIZE - 1, start_l, start_c,
				      "incorrect filename");
			  fnb[n++] = delim;
			  fnb[n] = NULL_CHAR;
			  v += 2;
			}
		    }
		  else if (ISPRINT (v[0]))
		    {
		      fnb[n++] = *(v++);
		      fnb[n] = NULL_CHAR;
		    }
		  else
		    {
		      SCAN_ERROR (true, start_l, start_c,
				  "incorrect filename");
		    }
		}
	      while (v[0] != delim);

	      /* Insist that the pragmat is closed properly.  */
	      v = &v[1];
	      SCAN_ERROR (!skip_pragmat (&u, &v, pr_lim, true), start_l, start_c,
			  "unterminated pragmat");
	      SCAN_ERROR (n == 0, start_l, start_c,
			  "incorrect filename");

	      char *sourcefile = NO_TEXT;
	      if (FILENAME (u) != NO_TEXT)
		{
		  sourcefile = xstrdup (FILENAME (u));
		}
	      else
		{
		  sourcefile = (char *) xmalloc (2);
		  a68_bufcpy (sourcefile, ".", 1);
		}
	      fn = find_include_file (sourcefile, fnb);
	      free (sourcefile);

	      /* Do not check errno, since errno may be undefined here
		 after a successful call.  */
	      if (fn != NO_TEXT)
		a68_bufcpy (fnb, fn, BUFFER_SIZE);
	      else
		{
		  SCAN_ERROR (true, start_l, start_c,
			      "included file not found");
		}
	      size_t fnwid = (int) strlen (fnb) + 1;
	      fn = (char *) xmalloc ((size_t) fnwid);
	      a68_bufcpy (fn, fnb, fnwid);

	      /* Ignore the file when included more than once.  */
	      for (t = top; t != NO_LINE; t = NEXT (t))
		{
		  if (strcmp (FILENAME (t), fn) == 0)
		    goto search_next_pragmat;
		}
	      t = NO_LINE;

	      /* Access the file.  */
	      fp = fopen (fn, "r");
	      SCAN_ERROR (fp == NULL, start_l, start_c,
			  "error opening included file");
	      ssize = a68_file_size (fileno (fp));
	      SCAN_ERROR (ssize < 0, start_l, start_c,
			  "error getting included file size");
	      fsize = ssize;
	      fbuf = (char *) xmalloc (8 + fsize);
	      bytes_read = (int) a68_file_read (fileno (fp), fbuf, (size_t) fsize);
	      SCAN_ERROR ((size_t) bytes_read != fsize, start_l, start_c,
			  "error while reading file");

	      /* Buffer still usable?.  */
	      if (fsize > A68_PARSER (max_scan_buf_length))
		{
		  A68_PARSER (max_scan_buf_length) = fsize;
		  A68_PARSER (scan_buf) = (char *) xmalloc (8 + A68_PARSER (max_scan_buf_length));
		}

	      /* Link all lines into the list.  */
	      linum = 1;
	      s = u;
	      t = PREVIOUS (u);
	      k = 0;
	      if (fsize == 0)
		{
		  /* If file is empty, insert single empty line.  */
		  A68_PARSER (scan_buf)[0] = NEWLINE_CHAR;
		  A68_PARSER (scan_buf)[1] = NULL_CHAR;
		  append_source_line (A68_PARSER (scan_buf), &t, &linum, fn);
		}
	      else
		{
		  while (k < fsize)
		    {
		      n = 0;
		      A68_PARSER (scan_buf)[0] = NULL_CHAR;
		      while (k < fsize && fbuf[k] != NEWLINE_CHAR)
			{
			  SCAN_ERROR ((ISCNTRL (fbuf[k]) && !ISSPACE (fbuf[k]))
				      || fbuf[k] == STOP_CHAR,
				      start_l, start_c,
				      "invalid characters in included file");
			  A68_PARSER (scan_buf)[n++] = fbuf[k++];
			  A68_PARSER (scan_buf)[n] = NULL_CHAR;
			}
		      A68_PARSER (scan_buf)[n++] = NEWLINE_CHAR;
		      A68_PARSER (scan_buf)[n] = NULL_CHAR;
		      if (k < fsize)
			k++;
		      append_source_line (A68_PARSER (scan_buf), &t, &linum, fn);
		    }
		}

	      /* Conclude and go find another include directive, if any.  */
	      NEXT (t) = s;
	      PREVIOUS (s) = t;
	      concatenate_lines (top);
	      if (fclose (fp) != 0)
		gcc_unreachable ();
	      make_pass = true;
	    }
	search_next_pragmat:
	  { (void) 0; };
	}
    }
}

/* Handle a pragment (pragmat or comment).  */

static char *
pragment (int type, LINE_T **ref_l, char **ref_c)
{
#define INIT_BUFFER					\
  do							\
    {							\
      chars_in_buf = 0;					\
      A68_PARSER (scan_buf)[chars_in_buf] = '\0';	\
    }							\
  while (0)

#define ADD_ONE_CHAR(CH)						\
  do									\
    {									\
      A68_PARSER (scan_buf)[chars_in_buf ++] = (CH);			\
      A68_PARSER (scan_buf)[chars_in_buf] = '\0';			\
    }									\
  while (0)

  const char *term_s = NO_TEXT;
  const char *beg_s = NO_TEXT;
  char c = **ref_c, *start_c = *ref_c;
  char *z = NO_TEXT;
  LINE_T *start_l = *ref_l;
  int beg_s_length, term_s_length, chars_in_buf;
  bool stop, pragmat = false;

  /* Set terminator to look for.  */
  if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING)
    {
      if (type == STYLE_I_COMMENT_SYMBOL)
	term_s = "CO";
      else if (type == STYLE_II_COMMENT_SYMBOL)
	term_s = "#";
      else if (type == BOLD_COMMENT_SYMBOL)
	term_s = "COMMENT";
      else if (type == BOLD_COMMENT_BEGIN_SYMBOL)
	{
	  beg_s = "NOTE";
	  term_s = "ETON";
	}
      else if (type == BRIEF_COMMENT_BEGIN_SYMBOL)
	{
	  beg_s = "{";
	  term_s = "}";
	}
      else if (type == STYLE_I_PRAGMAT_SYMBOL)
	{
	  term_s = "PR";
	  pragmat = true;
	}
      else if (type == BOLD_PRAGMAT_SYMBOL)
	{
	  term_s = "PRAGMAT";
	  pragmat = true;
	}
    }
  else
    {
      /* SUPPER stropping.  */
      if (type == STYLE_I_COMMENT_SYMBOL)
	term_s = "co";
      else if (type == STYLE_II_COMMENT_SYMBOL)
	term_s = "#";
      else if (type == BOLD_COMMENT_SYMBOL)
	term_s = "comment";
      else if (type == BOLD_COMMENT_BEGIN_SYMBOL)
	{
	  beg_s = "note";
	  term_s = "eton";
	}
      else if (type == BRIEF_COMMENT_BEGIN_SYMBOL)
	{
	  beg_s = "{";
	  term_s = "}";
	}
      else if (type == STYLE_I_PRAGMAT_SYMBOL)
	{
	  term_s = "pr";
	  pragmat = true;
	}
      else if (type == BOLD_PRAGMAT_SYMBOL)
	{
	  term_s = "pragmat";
	  pragmat = true;
	}
    }

  beg_s_length = (beg_s != NO_TEXT ? (int) strlen (beg_s) : 0);
  term_s_length = (int) strlen (term_s);

  /* Scan for terminator.  */
  bool nestable_comment = (beg_s != NO_TEXT);
  int nesting_level = 1;
  INIT_BUFFER;
  stop = false;
  while (stop == false)
    {
      SCAN_ERROR (c == STOP_CHAR, start_l, start_c,
		  "unterminated pragment");

      /* A ".." or '..' delimited string in a PRAGMAT, or
	 a ".." in a nestable comment.  */
      if ((pragmat && (c == QUOTE_CHAR || c == '\''))
	  || (nestable_comment && c == QUOTE_CHAR))
	{
	  char delim = c;
	  bool eos = false;
	  ADD_ONE_CHAR (c);
	  c = next_char (ref_l, ref_c, false);
	  while (!eos)
	    {
	      SCAN_ERROR (EOL (c), start_l, start_c,
			  "string within pragment exceeds end of line");

	      if (c == delim)
		{
		  ADD_ONE_CHAR (delim);
		  save_state (*ref_l, *ref_c, c);
		  c = next_char (ref_l, ref_c, false);
		  if (c == delim)
		    c = next_char (ref_l, ref_c, false);
		  else
		    {
		      restore_state (ref_l, ref_c, &c);
		      eos = true;
		    }
		}
	      else if (ISPRINT (c))
		{
		  ADD_ONE_CHAR (c);
		  c = next_char (ref_l, ref_c, false);
		}
	      else
		unworthy (start_l, start_c, c);
	    }
	}
      else if (EOL (c))
	ADD_ONE_CHAR (NEWLINE_CHAR);
      else if (ISPRINT (c) || ISSPACE (c))
	ADD_ONE_CHAR (c);

      if (nestable_comment && chars_in_buf >= beg_s_length)
	{
	  /* If we find another instance of the nestable begin mark, bump the
	     nesting level and continue scanning.  */
	  if (strcmp (beg_s,
		      &(A68_PARSER (scan_buf)[chars_in_buf - beg_s_length])) == 0)
	    {
	      nesting_level += 1;
	      goto nextchar;
	    }
	}

      if (chars_in_buf >= term_s_length)
	{
	  /* Check whether we encountered the terminator.  Mind nesting if
	     necessary.  */
	  if (strcmp (term_s,
		      &(A68_PARSER (scan_buf)[chars_in_buf - term_s_length])) == 0)
	    {
	      if (nestable_comment)
		{
		  gcc_assert (nesting_level > 0);
		  nesting_level -= 1;
		  stop = (nesting_level == 0);
		}
	      else
		stop = true;
	    }
	}

    nextchar:
      c = next_char (ref_l, ref_c, false);
    }

  A68_PARSER (scan_buf)[chars_in_buf - term_s_length] = '\0';
  z = a68_new_string (term_s, A68_PARSER (scan_buf), term_s, NO_TEXT);
  return z;
#undef ADD_ONE_CHAR
#undef INIT_BUFFER
}

/* Whether input shows exponent character.  */

static bool
is_exp_char (LINE_T **ref_l, char **ref_s, char *ch)
{
  bool ret = false;

  char exp_syms[3];

  /* Note that this works for both UPPER and SUPPER stropping regimes.  */
  exp_syms[0] = EXPONENT_CHAR;
  exp_syms[1] = TOUPPER (EXPONENT_CHAR);
  exp_syms[2] = '\0';

  save_state (*ref_l, *ref_s, *ch);
  if (strchr (exp_syms, *ch) != NO_TEXT)
    {
      *ch = next_char (ref_l, ref_s, true);
      ret = (strchr ("+-0123456789", *ch) != NO_TEXT);
    }
  restore_state (ref_l, ref_s, ch);
  return ret;
}

/* Whether input shows radix character.  */

static bool
is_radix_char (LINE_T **ref_l, char **ref_s, char *ch)
{
  bool ret = false;

  save_state (*ref_l, *ref_s, *ch);
  /* Note that this works for both UPPER and SUPPER stropping regimes.  */
  if (*ch == RADIX_CHAR)
    {
      *ch = next_char (ref_l, ref_s, true);
      ret = (strchr ("0123456789abcdef", *ch) != NO_TEXT);
    }
  restore_state (ref_l, ref_s, ch);
  return ret;
}

/* Whether input shows decimal point.  */

static bool
is_decimal_point (LINE_T **ref_l, char **ref_s, char *ch)
{
  bool ret = false;

  save_state (*ref_l, *ref_s, *ch);
  if (*ch == POINT_CHAR)
    {
      char exp_syms[3];

      /* Note that this works for both UPPER and SUPPER stropping regimes.  */
      exp_syms[0] = EXPONENT_CHAR;
      exp_syms[1] = TOUPPER (EXPONENT_CHAR);
      exp_syms[2] = '\0';

      *ch = next_char (ref_l, ref_s, true);
      if (strchr (exp_syms, *ch) != NO_TEXT)
	{
	  *ch = next_char (ref_l, ref_s, true);
	  ret = (strchr ("+-0123456789", *ch) != NO_TEXT);
	}
      else
	ret = (strchr ("0123456789", *ch) != NO_TEXT);
    }
  restore_state (ref_l, ref_s, ch);
  return ret;
}

/* Attribute for format item.  */

static enum a68_attribute
get_format_item (char ch)
{
  switch (TOLOWER (ch))
    {
    case 'a':
      return FORMAT_ITEM_A;
    case 'b':
      return FORMAT_ITEM_B;
    case 'c':
      return FORMAT_ITEM_C;
    case 'd':
      return FORMAT_ITEM_D;
    case 'e':
      return FORMAT_ITEM_E;
    case 'f':
      return FORMAT_ITEM_F;
    case 'g':
      return FORMAT_ITEM_G;
    case 'h':
      return FORMAT_ITEM_H;
    case 'i':
      return FORMAT_ITEM_I;
    case 'j':
      return FORMAT_ITEM_J;
    case 'k':
      return FORMAT_ITEM_K;
    case 'l':
    case '/':
      return FORMAT_ITEM_L;
    case 'm':
      return FORMAT_ITEM_M;
    case 'n':
      return FORMAT_ITEM_N;
    case 'o':
      return FORMAT_ITEM_O;
    case 'p':
      return FORMAT_ITEM_P;
    case 'q':
      return FORMAT_ITEM_Q;
    case 'r':
      return FORMAT_ITEM_R;
    case 's':
      return FORMAT_ITEM_S;
    case 't':
      return FORMAT_ITEM_T;
    case 'u':
      return FORMAT_ITEM_U;
    case 'v':
      return FORMAT_ITEM_V;
    case 'w':
      return FORMAT_ITEM_W;
    case 'x':
      return FORMAT_ITEM_X;
    case 'y':
      return FORMAT_ITEM_Y;
    case 'z':
      return FORMAT_ITEM_Z;
    case '+':
      return FORMAT_ITEM_PLUS;
    case '-':
      return FORMAT_ITEM_MINUS;
    case POINT_CHAR:
      return FORMAT_ITEM_POINT;
    case '%':
      return FORMAT_ITEM_ESCAPE;
    default:
      return STOP;
    }
}

/* Get next token from internal copy of source file.

   The kind of token is set via the passed pointer ATTR.
   The contents of token is set in the scan_buf via SYM.

   The recognized tokens are, by reported ATTR:

   <unset>
     End of file.
   FORMAT_ITEM_*
     Item in a format.
   STATIC_REPLICATOR
     INT denotation for a static replicator in a format.
   BOLD_TAG
     Bold tag.
   IDENTIFIER
     A "lower case" identifier.
   IDENTIFIER_WITH_UNDERSCORES
     A "lower case" identifier whose's at least one taggle
     was found adjacent to an underscore.
   REAL_DENOTATION
     A REAL denotation.
   POINT_SYMBOL
     .
   BITS_DENOTATION
     A BITS denotation like 16rffff
   INT_DENOTATION
     An INT denotation.
   ROW_CHAR_DENOTATION
     A STRING denotation.
   LITERAL
     A literal denotation in a format.
   STOP
     Single-character symbols #$()[]{},;@|:
     := /= :=: :/=:
     The character is placed in SYM.
   EQUALS_SYMBOL
     The equality symbol.
   OPERATOR
     A predefined operator.
*/

static void
get_next_token (bool in_format,
		LINE_T **ref_l, char **ref_s,
		LINE_T **start_l, char **start_c, enum a68_attribute *att)
{
  char c = **ref_s;
  char *sym = A68_PARSER (scan_buf);

  sym[0] = '\0';
  get_good_char (&c, ref_l, ref_s);
  *start_l = *ref_l;
  *start_c = *ref_s;
  if (c == STOP_CHAR)
    {
      /* We are at EOF.  */
      (sym++)[0] = STOP_CHAR;
      sym[0] = '\0';
      return;
    }

  if (in_format)
    {
      /* In a format.  */
      const char *format_items = "/%\\+-.abcdefghijklmnopqrstuvwxyz";
      if (strchr (format_items, c) != NO_TEXT)
	{
	  /* General format items.  */
	  (sym++)[0] = c;
	  sym[0] = NULL_CHAR;
	  *att = get_format_item (c);
	  (void) next_char (ref_l, ref_s, false);
	  return;
	}
      if (ISDIGIT (c))
	{
	  /* INT denotation for static replicator.  */
	  SCAN_DIGITS (c);
	  sym[0] = NULL_CHAR;
	  *att = STATIC_REPLICATOR;
	  return;
	}
    }

  if (ISUPPER (c))
    {
      /* Bold taggles are enabled only in gnu68.  */
      bool allow_one_under = !OPTION_STRICT (&A68_JOB);

      if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING)
	{
	  /* In UPPER stropping a bold tag is an upper case word.  */
	  while (ISUPPER (c))
	    {
	      (sym++)[0] = c;
	      c = next_char (ref_l, ref_s, false, allow_one_under);
	    }
	  sym[0] = '\0';
	  *att = BOLD_TAG;
	}
      else
	{
	  /* In SUPPER stropping a bold tag is a capitalized word that may
	     contain letters and digits.  */
	  while (ISALPHA (c) || ISDIGIT (c))
	    {
	      (sym++)[0] = c;
	      c = next_char (ref_l, ref_s, false, allow_one_under);
	    }
	  sym[0] = '\0';
	  *att = BOLD_TAG;
	}
    }
  else if (ISLOWER (c))
    {
      /* In both UPPER and SUPPER stropping regimes a tag is a lower case word
	 which may contain letters and digits.

	 In SUPPER stropping, however, it is not allowed to have blanks
	 separating the taggles within tags.  */

      bool allow_one_under = true;
      bool found_under = false;
      bool allow_typo = OPTION_STROPPING (&A68_JOB) != SUPPER_STROPPING;

      /* Lower case word - identifier.  */
      while (ISLOWER (c) || ISDIGIT (c))
	{
	  (sym++)[0] = c;
	  c = next_char (ref_l, ref_s, allow_typo, allow_one_under,
			 &found_under);
	}

      sym[0] = '\0';
      *att = found_under ? IDENTIFIER_WITH_UNDERSCORES : IDENTIFIER;
    }
  else if (c == POINT_CHAR)
    {
      /* Begins with a point symbol - point, L REAL denotation.  */
      if (is_decimal_point (ref_l, ref_s, &c))
	{
	  (sym++)[0] = '0';
	  (sym++)[0] = POINT_CHAR;
	  c = next_char (ref_l, ref_s, true);
	  SCAN_DIGITS (c);
	  if (is_exp_char (ref_l, ref_s, &c))
	    SCAN_EXPONENT_PART (c);
	  sym[0] = '\0';
	  *att = REAL_DENOTATION;
	}
      else
	{
	  c = next_char (ref_l, ref_s, true);
	  (sym++)[0] = POINT_CHAR;
	  sym[0] = '\0';
	  *att = POINT_SYMBOL;
	}
    }
  else if (ISDIGIT (c))
    {
      /* Something that begins with a digit:
	 L INT denotation, L REAL denotation.  */
      SCAN_DIGITS (c);

      if (is_decimal_point (ref_l, ref_s, &c))
	{
	  c = next_char (ref_l, ref_s, true);
	  if (is_exp_char (ref_l, ref_s, &c))
	    {
	      (sym++)[0] = POINT_CHAR;
	      (sym++)[0] = '0';
	      SCAN_EXPONENT_PART (c);
	      *att = REAL_DENOTATION;
	    }
	  else
	    {
	      (sym++)[0] = POINT_CHAR;
	      SCAN_DIGITS (c);
	      if (is_exp_char (ref_l, ref_s, &c))
		SCAN_EXPONENT_PART (c);
	      *att = REAL_DENOTATION;
	    }
	}
      else if (is_exp_char (ref_l, ref_s, &c))
	{
	  SCAN_EXPONENT_PART (c);
	  *att = REAL_DENOTATION;
	}
      else if (is_radix_char (ref_l, ref_s, &c))
	{
	  /* Parse the radix, which is expressed in base 10.  */
	  (sym++)[0] = c;
	  char *end;
	  int64_t radix = strtol (A68_PARSER (scan_buf), &end, 10);
	  gcc_assert (end != A68_PARSER (scan_buf) && *end == 'r');

	  /* Get the rest of the bits literal.  Typographical display features
	     are allowed in the reference language between the digit symbols
	     composing the denotation.  However, in SUPPER stropping this could
	     lead to confusing situations like:

	       while bitmask /= 16r0 do ~ od

	     Where the scanner would recognize a bits denotation 16r0d and then
	     the parser would complain about a missing 'do'.  This is not a
	     problem in UPPER stropping since D is not a valid hexadecimal
	     digit.

	     To avoid confusing errors, in SUPPER stropping we do not allow
	     typographical display features in bits denotations when the radix
	     is 16.  */
	  c = next_char (ref_l, ref_s, true);
	  while (((radix == 2 && (c == '0' || c == '1'))
		  || (radix == 4 && (c >= '0' && c <= '3'))
		  || (radix == 8 && (c >= '0' && c <= '7'))
		  || (radix == 16 && (ISDIGIT (c) || strchr ("abcdef", c) != NO_TEXT))))
	    {
	      (sym++)[0] = c;
	      c = next_char (ref_l, ref_s,
			     OPTION_STROPPING (&A68_JOB) != SUPPER_STROPPING
			     || radix != 16);
	    }
	  *att = BITS_DENOTATION;
	}
      else
	{
	  *att = INT_DENOTATION;
	}
      sym[0] = '\0';
    }
  else if (c == QUOTE_CHAR)
    {
      /* STRING denotation.  */
      bool stop = false;

      while (!stop)
	{
	  c = next_char (ref_l, ref_s, false);
	  while (c != QUOTE_CHAR && c != STOP_CHAR)
	    {
	      if (c == APOSTROPHE_CHAR)
		{
		  (sym++)[0] = c;
		  c = next_char (ref_l, ref_s, false);
		  switch (c)
		    {
		    case APOSTROPHE_CHAR:
		    case 'n':
		    case 'f':
		    case 'r':
		    case 't':
		      (sym++)[0] = c;
		      break;
		    case '(':
		      {
			unsigned int num_code_points = 0;

			(sym++)[0] = c;
			/* Process code points.  */
			while (1)
			  {
			    /* Skip white spaces.  */
			    while (1)
			      {
				c = next_char (ref_l, ref_s, false);
				if (!ISSPACE (c))
				  break;
			      }

			    /* See if we are done.  */
			    if (c == ')')
			      {
				SCAN_ERROR (num_code_points == 0, *start_l, *ref_s,
					    "expected at least one character point in string break");
				(sym++)[0] = c;
				break;
			      }
			    else if (c == 'u' || c == 'U')
			      {
				(sym++)[0] = c;
				/* Process a code point.  */
				char u = c;
				int numdigits = (u == 'u' ? 4 : 8);
				char *startpos = *ref_s;
				int i = 0;
				do
				  {
				    c = next_char (ref_l, ref_s, false);
				    if (!(ISDIGIT (c)
					  || ((c >= 'a') && (c <= 'f'))
					  || ((c >= 'A') && (c <= 'F'))))
				      {
					SCAN_ERROR (true, *start_l, startpos,
						    (u == 'u'
						     ? "expected four hex digits in \
string break character point"
						     : "expected eight hex digits in \
string break character point"));
				      }
				    (sym++)[0] = c;
				    i += 1;
				  }
				while (i < numdigits);

				/* Skip white spaces.  */
				while (1)
				  {
				    c = next_char (ref_l, ref_s, false);
				    if (!ISSPACE (c))
				      break;
				  }

				/* Comma or end of list.  */
				if (c == ')')
				  {
				    (sym++)[0] = c;
				    break;
				  }

				SCAN_ERROR (c != ',', *start_l, *ref_s,
					    "expected , or ) in string break");
			      }
			    else
			      {
				SCAN_ERROR (true, *start_l, *ref_s,
					    "unterminated list of character codes");
			      }
			  }
			break;
		      }
		    default:
		      SCAN_ERROR (true, *start_l, *ref_s, "invalid string break sequence");
		    }
		}
	      else
		{
		  SCAN_ERROR (EOL (c), *start_l, *start_c, "string exceeds end of line");
		  (sym++)[0] = c;
		}
	      c = next_char (ref_l, ref_s, false);
	    }
	  SCAN_ERROR (*ref_l == NO_LINE, *start_l, *start_c, "unterminated string");
	  c = next_char (ref_l, ref_s, false);
	  if (c == QUOTE_CHAR)
	    (sym++)[0] = QUOTE_CHAR;
	  else
	    stop = true;
	}
      sym[0] = '\0';
      *att = (in_format ? LITERAL : ROW_CHAR_DENOTATION);
    }
  else if (strchr ("#$()[]{},;@", c) != NO_TEXT)
    {
      /* Single character symbols.  */
      (sym++)[0] = c;
      (void) next_char (ref_l, ref_s, false);
      sym[0] = '\0';
      *att = STOP;
    }
  else if (c == '|')
    {
      /* Bar.  */
      (sym++)[0] = c;
      c = next_char (ref_l, ref_s, false);
      if (c == ':')
	{
	  (sym++)[0] = c;
	  (void) next_char (ref_l, ref_s, false);
	}
      sym[0] = '\0';
      *att = STOP;
    }
  else if (c == ':')
    {
      /* Colon, semicolon, IS, ISNT.  */
      (sym++)[0] = c;
      c = next_char (ref_l, ref_s, false);
      if (c == '=')
	{
	  (sym++)[0] = c;
	  if ((c = next_char (ref_l, ref_s, false)) == ':')
	    {
	      (sym++)[0] = c;
	      c = next_char (ref_l, ref_s, false);
	    }
	}
      else if (c == '/')
	{
	  (sym++)[0] = c;
	  if ((c = next_char (ref_l, ref_s, false)) == '=')
	    {
	      (sym++)[0] = c;
	      if ((c = next_char (ref_l, ref_s, false)) == ':')
		{
		  (sym++)[0] = c;
		  c = next_char (ref_l, ref_s, false);
		}
	    }
	}
      else if (c == ':')
	{
	  (sym++)[0] = c;
	  if ((c = next_char (ref_l, ref_s, false)) == '=')
	    (sym++)[0] = c;
	}

      sym[0] = '\0';
      *att = STOP;

    }
  else if (c == '=')
    {
      /* Operator starting with "=".  */
      char *scanned = sym;
      (sym++)[0] = c;
      c = next_char (ref_l, ref_s, false);
      if (strchr (NOMADS, c) != NO_TEXT)
	{
	  (sym++)[0] = c;
	  c = next_char (ref_l, ref_s, false);
	}
      if (c == '=')
	{
	  (sym++)[0] = c;
	  if (next_char (ref_l, ref_s, false) == ':')
	    {
	      (sym++)[0] = ':';
	      c = next_char (ref_l, ref_s, false);
	      if (strlen (sym) < 4 && c == '=')
		{
		  (sym++)[0] = '=';
		  (void) next_char (ref_l, ref_s, false);
		}
	    }
	}
      else if (c == ':')
	{
	  (sym++)[0] = c;
	  sym[0] = '\0';
	  if (next_char (ref_l, ref_s, false) == '=')
	    {
	      (sym++)[0] = '=';
	      (void) next_char (ref_l, ref_s, false);
	    }
	  else
	    {
	      SCAN_ERROR (!(strcmp (scanned, "=:") == 0 || strcmp (scanned, "==:") == 0),
			  *start_l, *start_c, "invalid operator tag");
	    }
	}
      sym[0] = '\0';
      if (strcmp (scanned, "=") == 0)
	*att = EQUALS_SYMBOL;
      else
	*att = OPERATOR;
    }
  else if (strchr (MONADS, c) != NO_TEXT || strchr (NOMADS, c) != NO_TEXT)
    {
      /* Operator.  */
      char *scanned = sym;
      (sym++)[0] = c;
      c = next_char (ref_l, ref_s, false);
      if (strchr (NOMADS, c) != NO_TEXT)
	{
	  (sym++)[0] = c;
	  c = next_char (ref_l, ref_s, false);
	}
      if (c == '=')
	{
	  (sym++)[0] = c;
	  if (next_char (ref_l, ref_s, false) == ':')
	    {
	      (sym++)[0] = ':';
	      c = next_char (ref_l, ref_s, false);
	      if (strlen (scanned) < 4 && c == '=')
		{
		  (sym++)[0] = '=';
		  (void) next_char (ref_l, ref_s, false);
		}
	    }
	}
      else if (c == ':')
	{
	  (sym++)[0] = c;
	  sym[0] = '\0';
	  if (next_char (ref_l, ref_s, false) == '=')
	    {
	      (sym++)[0] = '=';
	      sym[0] = '\0';
	      (void) next_char (ref_l, ref_s, false);
	    }
	  else
	    {
	      SCAN_ERROR (strcmp (&(scanned[1]), "=:") != 0,
			  *start_l, *start_c, "invalid operator tag");
	    }
	}
      sym[0] = '\0';
      *att = OPERATOR;
    }
  else
    {
      /* Afuuus ... strange characters!.  */
      unworthy (*start_l, *start_c, (int) c);
    }
}

/* Whether att opens an embedded clause.  */

static bool
open_nested_clause (int att)
{
  switch (att)
    {
    case OPEN_SYMBOL:
    case BEGIN_SYMBOL:
    case PAR_SYMBOL:
    case IF_SYMBOL:
    case CASE_SYMBOL:
    case FOR_SYMBOL:
    case FROM_SYMBOL:
    case BY_SYMBOL:
    case TO_SYMBOL:
    case WHILE_SYMBOL:
    case DO_SYMBOL:
    case SUB_SYMBOL:
      return true;
    }
  return false;
}

/* Whether att closes an embedded clause.  */

static bool
close_nested_clause (int att)
{
  switch (att)
    {
    case CLOSE_SYMBOL:
    case END_SYMBOL:
    case FI_SYMBOL:
    case ESAC_SYMBOL:
    case OD_SYMBOL:
    case BUS_SYMBOL:
      return true;
    }
  return false;
}

/* Cast a string to lower case.  */

static void
make_lower_case (char *p)
{
  for (; p != NO_TEXT && p[0] != '\0'; p++)
    p[0] = TOLOWER (p[0]);
}

/* Cast a string to upper case.  */

static void
make_upper_case (char *p)
{
  for (; p != NO_TEXT && p[0] != '\0'; p++)
    p[0] = TOUPPER (p[0]);
}

/* Construct a linear list of tokens.  */

static void
tokenise_source (NODE_T **root, int level, bool in_format,
		 LINE_T **l, char **s, LINE_T **start_l,
		 char **start_c)
{
  char *pragmat_lpr = NO_TEXT;
  int pragmat_lprt = 0;
  LINE_T *pragmat_lprl = NO_LINE;
  char *pragmat_lprc = NULL;

  char *comment_lpr = NO_TEXT;
  int comment_lprt = 0;
  LINE_T *comment_lprl = NO_LINE;
  char *comment_lprc = NULL;

  while (l != NO_VAR && !A68_PARSER (stop_scanner))
    {
      enum a68_attribute att = STOP;
      get_next_token (in_format, l, s, start_l, start_c, &att);

      if (A68_PARSER (scan_buf)[0] == STOP_CHAR)
	A68_PARSER (stop_scanner) = true;
      else if (strlen (A68_PARSER (scan_buf)) > 0 || att == ROW_CHAR_DENOTATION || att == LITERAL)
	{
	  KEYWORD_T *kw;
	  const char *c = NO_TEXT;
	  bool make_node = true;
	  const char *trailing = NO_TEXT;

	  if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING)
	    {
	      /* In UPPER stropping all symbols in R9.4.1 are expressed as bold
		 tags like "BEGIN", or symbols like "@".  */

	      /* In this stropping regime there is no need to handle
		 identifiers for which taggles were adjacent to underscores
		 specially.  */
	      if (att != IDENTIFIER && att != IDENTIFIER_WITH_UNDERSCORES)
		kw = a68_find_keyword (A68 (top_keyword), A68_PARSER (scan_buf));
	      else
		kw = NO_KEYWORD;
	    }
	  else
	    {
	      /* In SUPPER stropping all symbols in R9.4.1 are expressed as
		 tags like "begin", or symbols like "@".  */

	      /* Normalize bold tags to all upper-case letters.  */
	      if (att == BOLD_TAG)
		make_upper_case (A68_PARSER (scan_buf));

	      /* If any of the taggles of the scanned identifier were adjacent
		 to an underscore, that inhibits interpreting it as a
		 keyword.  */
	      if (att != BOLD_TAG && att != IDENTIFIER_WITH_UNDERSCORES)
		kw = a68_find_keyword (A68 (top_keyword), A68_PARSER (scan_buf));
	      else
		kw = NO_KEYWORD;
	    }

	  /* Beyond this point it is irrelevant whether an identifier had
	     taggles adjacent to an underscore.  */
	  if (att == IDENTIFIER_WITH_UNDERSCORES)
	    att = IDENTIFIER;

	  if (kw == NO_KEYWORD || att == ROW_CHAR_DENOTATION)
	    {
	      if (att == IDENTIFIER)
		make_lower_case (A68_PARSER (scan_buf));
	      if (att != ROW_CHAR_DENOTATION && att != LITERAL)
		{
		  size_t len = strlen (A68_PARSER (scan_buf));
		  while (len >= 1 && A68_PARSER (scan_buf)[len - 1] == '_')
		    {
		      trailing = "_";
		      A68_PARSER (scan_buf)[len - 1] = NULL_CHAR;
		      len--;
		    }
		}
	      c = TEXT (a68_add_token (&A68 (top_token), A68_PARSER (scan_buf)));
	    }
	  else
	    {
	      if (IS (kw, TO_SYMBOL))
		{
		  /* Merge GO and TO to GOTO.  */
		  if (*root != NO_NODE && IS (*root, GO_SYMBOL))
		    {
		      ATTRIBUTE (*root) = GOTO_SYMBOL;
		      NSYMBOL (*root) = TEXT (a68_find_keyword (A68 (top_keyword), "GOTO"));
		      make_node = false;
		    }
		  else
		    {
		      att = ATTRIBUTE (kw);
		      c = TEXT (kw);
		    }
		}
	      else
		{
		  if (OPTION_STROPPING (&A68_JOB) == UPPER_STROPPING)
		    {
		      if (att == 0 || att == BOLD_TAG)
			att = ATTRIBUTE (kw);
		    }
		  else
		    {
		      if (att == 0 || att == IDENTIFIER)
			att = ATTRIBUTE (kw);
		    }

		  c = TEXT (kw);
		  /* Handle pragments.  */
		  if (att == STYLE_II_COMMENT_SYMBOL
		      || att == STYLE_I_COMMENT_SYMBOL
		      || att == BOLD_COMMENT_SYMBOL
		      || att == BOLD_COMMENT_BEGIN_SYMBOL
		      || att == BRIEF_COMMENT_BEGIN_SYMBOL)
		    {
		      char *nlpr = pragment (ATTRIBUTE (kw), l, s);

		      if (comment_lpr == NO_TEXT
			  || (int) strlen (comment_lpr) == 0)
			comment_lpr = nlpr;
		      else
			{
			  char *stale = comment_lpr;
			  comment_lpr
			    = a68_new_string (comment_lpr, "n\n", nlpr, NO_TEXT);
			  free (stale);
			}
		      comment_lprt = att;
		      comment_lprl = *start_l;
		      comment_lprc = *start_c;
		      make_node = false;
		    }
		  else if (att == STYLE_I_PRAGMAT_SYMBOL
			   || att == BOLD_PRAGMAT_SYMBOL)
		    {
		      char *nlpr = pragment (ATTRIBUTE (kw), l, s);
		      if (pragmat_lpr == NO_TEXT
			  || (int) strlen (pragmat_lpr) == 0)
			pragmat_lpr = nlpr;
		      else
			{
			  char *stale = pragmat_lpr;
			  pragmat_lpr
			    = a68_new_string (pragmat_lpr, " ", nlpr, NO_TEXT);
			  free (stale);
			}
		      pragmat_lprt = att;
		      pragmat_lprl = *start_l;
		      pragmat_lprc = *start_c;
		      if (!A68_PARSER (stop_scanner))
			make_node = false;
		    }
		}
	    }
	  /* Add token to the tree.  */
	  if (make_node)
	    {
	      NODE_T *q = a68_new_node ();
	      INFO (q) = a68_new_node_info ();

	      switch (att)
		{
		case ASSIGN_SYMBOL:
		case END_SYMBOL:
		case ESAC_SYMBOL:
		case OD_SYMBOL:
		case OF_SYMBOL:
		case FI_SYMBOL:
		case CLOSE_SYMBOL:
		case BUS_SYMBOL:
		case COLON_SYMBOL:
		case COMMA_SYMBOL:
		case SEMI_SYMBOL:
		  GINFO (q) = NO_GINFO;
		  break;
		default:
		  GINFO (q) = a68_new_genie_info ();
		  break;
		}

	      STATUS (q) = (STATUS_MASK_T) 0;
	      LINE (INFO (q)) = *start_l;
	      CHAR_IN_LINE (INFO (q)) = *start_c;
	      PRIO (INFO (q)) = 0;
	      PROCEDURE_LEVEL (INFO (q)) = 0;
	      ATTRIBUTE (q) = att;
	      NSYMBOL (q) = c;
	      PREVIOUS (q) = *root;
	      SUB (q) = NEXT (q) = NO_NODE;
	      TABLE (q) = NO_TABLE;
	      MOID (q) = NO_MOID;
	      TAX (q) = NO_TAG;
	      if (pragmat_lpr != NO_TEXT)
		{
		  NPRAGMAT (q) = pragmat_lpr;
		  NPRAGMAT_TYPE (q) = pragmat_lprt;
		  NPRAGMAT_LINE (q) = pragmat_lprl;
		  NPRAGMAT_CHAR_IN_LINE (q) = pragmat_lprc;
		  pragmat_lpr = NO_TEXT;
		  pragmat_lprt = 0;
		}
	      if (comment_lpr != NO_TEXT)
		{
		  NCOMMENT (q) = comment_lpr;
		  NCOMMENT_TYPE (q) = comment_lprt;
		  NCOMMENT_LINE (q) = comment_lprl;
		  NCOMMENT_CHAR_IN_LINE (q) = comment_lprc;
		  comment_lpr = NO_TEXT;
		  comment_lprt = 0;
		}
	      if (*root != NO_NODE)
		NEXT (*root) = q;
	      if (TOP_NODE (&A68_JOB) == NO_NODE)
		TOP_NODE (&A68_JOB) = q;
	      *root = q;
	      if (trailing != NO_TEXT)
		a68_warning (q, 0,
			     "ignoring trailing character H in A",
			     trailing, att);
	    }
	  /* Redirection in tokenising formats. The scanner is a recursive-descent type as
	     to know when it scans a format text and when not.  */
	  if (in_format && att == FORMAT_DELIMITER_SYMBOL)
	    return;
	  else if (!in_format && att == FORMAT_DELIMITER_SYMBOL)
	    tokenise_source (root, level + 1, true, l, s, start_l, start_c);
	  else if (in_format && open_nested_clause (att))
	    {
	      NODE_T *z = PREVIOUS (*root);

	      if (z != NO_NODE && a68_is_one_of (z, FORMAT_ITEM_N, FORMAT_ITEM_G, FORMAT_ITEM_H,
						 FORMAT_ITEM_F, STOP))
		{
		  tokenise_source (root, level, false, l, s, start_l, start_c);
		}
	      else if (att == OPEN_SYMBOL)
		ATTRIBUTE (*root) = FORMAT_OPEN_SYMBOL;
	      else if (OPTION_BRACKETS (&A68_JOB) && att == SUB_SYMBOL)
		ATTRIBUTE (*root) = FORMAT_OPEN_SYMBOL;
	    }
	  else if (!in_format && level > 0 && open_nested_clause (att))
	    tokenise_source (root, level + 1, false, l, s, start_l, start_c);
	  else if (!in_format && level > 0 && close_nested_clause (att))
	    return;
	  else if (in_format && att == CLOSE_SYMBOL)
	    ATTRIBUTE (*root) = FORMAT_CLOSE_SYMBOL;
	  else if (OPTION_BRACKETS (&A68_JOB) && in_format && att == BUS_SYMBOL)
	    ATTRIBUTE (*root) = FORMAT_CLOSE_SYMBOL;
	}
    }
}

/* Tokenise source file, build initial syntax tree.  */

bool
a68_lexical_analyser (const char *filename, bool *empty_program)
{
  LINE_T *l = NO_LINE, *start_l = NO_LINE;
  char *s = NO_TEXT, *start_c = NO_TEXT;
  NODE_T *root = NO_NODE;

  /* Read the source file into lines.  */
  if (!read_source_file (filename))
    return false;

  /* Start tokenising.  */
  A68_PARSER (stop_scanner) = false;
  if ((l = TOP_LINE (&A68_JOB)) != NO_LINE)
    s = STRING (l);
  tokenise_source (&root, 0, false, &l, &s, &start_l, &start_c);

  /* Detemine whether the actual file contents resulted in some token.  This is
     used in order to provide better diagnostics for empty source files or
     files containing only comments or pragmats.  These are not valid Algol 68
     packets.  */

  *empty_program = true;
  for (NODE_T *p = TOP_NODE (&A68_JOB); p != NO_NODE; FORWARD (p))
    {
      LINE_T *l = LINE (INFO (p));
      if (strcmp (FILENAME (l), "prelude") != 0
	  && strcmp (FILENAME (l), "postlude") != 0)
	{
	  *empty_program = false;
	  break;
	}
    }

  /* If the source is a prelude packet then we should remove the prelude and
     postlude nodes from the token stream.  We distinguish these nodes by
     location.

     Yes this is crude and creepy but it works and it is less annoying than not
     adding the prelude/postlude in read_source_file and I got other fish to
     fry at this moment.  Somebody please fix this in a decent way, thanks -
     jemarch.  */

  NODE_T *p = TOP_NODE (&A68_JOB);
  for (; p != NO_NODE; FORWARD (p))
    {
      LINE_T *l = LINE (INFO (p));
      if (strcmp (FILENAME (l), "prelude") != 0)
	break;
    }

  if (p != NO_NODE && IS (p, MODULE_SYMBOL))
    {
      p = TOP_NODE (&A68_JOB);
      while (p != NO_NODE)
	{
	  LINE_T *l = LINE (INFO (p));
	  if (strcmp (FILENAME (l), "prelude") == 0
	      || strcmp (FILENAME (l), "postlude") == 0)
	    {
	      if (PREVIOUS (p) != NO_NODE)
		NEXT (PREVIOUS (p)) = NEXT (p);
	      else
		TOP_NODE (&A68_JOB) = NEXT (p);

	      if (NEXT (p) != NO_NODE)
		PREVIOUS (NEXT (p)) = PREVIOUS (p);

	      NODE_T *next = NEXT (p);
	      p = next;
	    }
	  else
	    p = FORWARD (p);
	}
    }

  /* Note that A68_PARSER (scan_buf) and A68_PARSER (max_scan_buf_length) are
     allocated by read_source_line.  */
  free (A68_PARSER (scan_buf));
  A68_PARSER (scan_buf) = NULL;
  A68_PARSER (max_scan_buf_length) = 0;
  return true;
}
