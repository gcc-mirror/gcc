/* Character scanner.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Set of subroutines to (ultimately) return the next character to the
   various matching subroutines.  This file's job is to read files and
   build up lines that are parsed by the parser.  This means that we
   handle continuation lines and "include" lines.

   The first thing the scanner does is to load an entire file into
   memory.  We load the entire file into memory for a couple reasons.
   The first is that we want to be able to deal with nonseekable input
   (pipes, stdin) and there is a lot of backing up involved during
   parsing.

   The second is that we want to be able to print the locus of errors,
   and an error on line 999999 could conflict with something on line
   one.  Given nonseekable input, we've got to store the whole thing.

   One thing that helps are the column truncation limits that give us
   an upper bound on the size of individual lines.  We don't store the
   truncated stuff.

   From the scanner's viewpoint, the higher level subroutines ask for
   new characters and do a lot of jumping backwards.  */

#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "toplev.h"

/* Structure for holding module and include file search path.  */
typedef struct gfc_directorylist
{
  char *path;
  struct gfc_directorylist *next;
}
gfc_directorylist;

/* List of include file search directories.  */
static gfc_directorylist *include_dirs;

static gfc_file *file_head, *current_file;

static int continue_flag, end_flag, openmp_flag;
static int continue_count, continue_line;
static locus openmp_locus;

gfc_source_form gfc_current_form;
static gfc_linebuf *line_head, *line_tail;
       
locus gfc_current_locus;
const char *gfc_source_file;
static FILE *gfc_src_file;
static char *gfc_src_preprocessor_lines[2];

extern int pedantic;

/* Main scanner initialization.  */

void
gfc_scanner_init_1 (void)
{
  file_head = NULL;
  line_head = NULL;
  line_tail = NULL;

  continue_count = 0;
  continue_line = 0;

  end_flag = 0;
}


/* Main scanner destructor.  */

void
gfc_scanner_done_1 (void)
{
  gfc_linebuf *lb;
  gfc_file *f;

  while(line_head != NULL) 
    {
      lb = line_head->next;
      gfc_free(line_head);
      line_head = lb;
    }
     
  while(file_head != NULL) 
    {
      f = file_head->next;
      gfc_free(file_head->filename);
      gfc_free(file_head);
      file_head = f;    
    }

}


/* Adds path to the list pointed to by list.  */

void
gfc_add_include_path (const char *path)
{
  gfc_directorylist *dir;
  const char *p;

  p = path;
  while (*p == ' ' || *p == '\t')  /* someone might do 'gfortran "-I include"' */
    if (*p++ == '\0')
      return;

  dir = include_dirs;
  if (!dir)
    {
      dir = include_dirs = gfc_getmem (sizeof (gfc_directorylist));
    }
  else
    {
      while (dir->next)
	dir = dir->next;

      dir->next = gfc_getmem (sizeof (gfc_directorylist));
      dir = dir->next;
    }

  dir->next = NULL;
  dir->path = gfc_getmem (strlen (p) + 2);
  strcpy (dir->path, p);
  strcat (dir->path, "/");	/* make '/' last character */
}


/* Release resources allocated for options.  */

void
gfc_release_include_path (void)
{
  gfc_directorylist *p;

  gfc_free (gfc_option.module_dir);
  while (include_dirs != NULL)
    {
      p = include_dirs;
      include_dirs = include_dirs->next;
      gfc_free (p->path);
      gfc_free (p);
    }
}

/* Opens file for reading, searching through the include directories
   given if necessary.  If the include_cwd argument is true, we try
   to open the file in the current directory first.  */

FILE *
gfc_open_included_file (const char *name, const bool include_cwd)
{
  char *fullname;
  gfc_directorylist *p;
  FILE *f;

  if (IS_ABSOLUTE_PATH (name))
    return gfc_open_file (name);

  if (include_cwd)
    {
      f = gfc_open_file (name);
      if (f != NULL)
	return f;
    }

  for (p = include_dirs; p; p = p->next)
    {
      fullname = (char *) alloca(strlen (p->path) + strlen (name) + 1);
      strcpy (fullname, p->path);
      strcat (fullname, name);

      f = gfc_open_file (fullname);
      if (f != NULL)
	return f;
    }

  return NULL;
}

/* Test to see if we're at the end of the main source file.  */

int
gfc_at_end (void)
{

  return end_flag;
}


/* Test to see if we're at the end of the current file.  */

int
gfc_at_eof (void)
{

  if (gfc_at_end ())
    return 1;

  if (line_head == NULL)
    return 1;			/* Null file */

  if (gfc_current_locus.lb == NULL)
    return 1;

  return 0;
}


/* Test to see if we're at the beginning of a new line.  */

int
gfc_at_bol (void)
{
  if (gfc_at_eof ())
    return 1;

  return (gfc_current_locus.nextc == gfc_current_locus.lb->line);
}


/* Test to see if we're at the end of a line.  */

int
gfc_at_eol (void)
{

  if (gfc_at_eof ())
    return 1;

  return (*gfc_current_locus.nextc == '\0');
}


/* Advance the current line pointer to the next line.  */

void
gfc_advance_line (void)
{
  if (gfc_at_end ())
    return;

  if (gfc_current_locus.lb == NULL) 
    {
      end_flag = 1;
      return;
    } 

  gfc_current_locus.lb = gfc_current_locus.lb->next;

  if (gfc_current_locus.lb != NULL)         
    gfc_current_locus.nextc = gfc_current_locus.lb->line;
  else 
    {
      gfc_current_locus.nextc = NULL;
      end_flag = 1;
    }       
}


/* Get the next character from the input, advancing gfc_current_file's
   locus.  When we hit the end of the line or the end of the file, we
   start returning a '\n' in order to complete the current statement.
   No Fortran line conventions are implemented here.

   Requiring explicit advances to the next line prevents the parse
   pointer from being on the wrong line if the current statement ends
   prematurely.  */

static int
next_char (void)
{
  int c;
  
  if (gfc_current_locus.nextc == NULL)
    return '\n';

  c = (unsigned char) *gfc_current_locus.nextc++;
  if (c == '\0')
    {
      gfc_current_locus.nextc--; /* Remain on this line.  */
      c = '\n';
    }

  return c;
}

/* Skip a comment.  When we come here the parse pointer is positioned
   immediately after the comment character.  If we ever implement
   compiler directives withing comments, here is where we parse the
   directive.  */

static void
skip_comment_line (void)
{
  char c;

  do
    {
      c = next_char ();
    }
  while (c != '\n');

  gfc_advance_line ();
}


/* Comment lines are null lines, lines containing only blanks or lines
   on which the first nonblank line is a '!'.
   Return true if !$ openmp conditional compilation sentinel was
   seen.  */

static bool
skip_free_comments (void)
{
  locus start;
  char c;
  int at_bol;

  for (;;)
    {
      at_bol = gfc_at_bol ();
      start = gfc_current_locus;
      if (gfc_at_eof ())
	break;

      do
	c = next_char ();
      while (gfc_is_whitespace (c));

      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (c == '!')
	{
	  /* If -fopenmp, we need to handle here 2 things:
	     1) don't treat !$omp as comments, but directives
	     2) handle OpenMP conditional compilation, where
		!$ should be treated as 2 spaces (for initial lines
		only if followed by space).  */
	  if (gfc_option.flag_openmp && at_bol)
	    {
	      locus old_loc = gfc_current_locus;
	      if (next_char () == '$')
		{
		  c = next_char ();
		  if (c == 'o' || c == 'O')
		    {
		      if (((c = next_char ()) == 'm' || c == 'M')
			  && ((c = next_char ()) == 'p' || c == 'P')
			  && ((c = next_char ()) == ' ' || continue_flag))
			{
			  while (gfc_is_whitespace (c))
			    c = next_char ();
			  if (c != '\n' && c != '!')
			    {
			      openmp_flag = 1;
			      openmp_locus = old_loc;
			      gfc_current_locus = start;
			      return false;
			    }
			}
		      gfc_current_locus = old_loc;
		      next_char ();
		      c = next_char ();
		    }
		  if (continue_flag || c == ' ')
		    {
		      gfc_current_locus = old_loc;
		      next_char ();
		      openmp_flag = 0;
		      return true;
		    }
		}
	      gfc_current_locus = old_loc;
	    }
	  skip_comment_line ();
	  continue;
	}

      break;
    }

  if (openmp_flag && at_bol)
    openmp_flag = 0;
  gfc_current_locus = start;
  return false;
}


/* Skip comment lines in fixed source mode.  We have the same rules as
   in skip_free_comment(), except that we can have a 'c', 'C' or '*'
   in column 1, and a '!' cannot be in column 6.  Also, we deal with
   lines with 'd' or 'D' in column 1, if the user requested this.  */

static void
skip_fixed_comments (void)
{
  locus start;
  int col;
  char c;

  if (! gfc_at_bol ())
    {
      start = gfc_current_locus;
      if (! gfc_at_eof ())
	{
	  do
	    c = next_char ();
	  while (gfc_is_whitespace (c));

	  if (c == '\n')
	    gfc_advance_line ();
	  else if (c == '!')
	    skip_comment_line ();
	}

      if (! gfc_at_bol ())
	{
	  gfc_current_locus = start;
	  return;
	}
    }

  for (;;)
    {
      start = gfc_current_locus;
      if (gfc_at_eof ())
	break;

      c = next_char ();
      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (c == '!' || c == 'c' || c == 'C' || c == '*')
	{
	  /* If -fopenmp, we need to handle here 2 things:
	     1) don't treat !$omp|c$omp|*$omp as comments, but directives
	     2) handle OpenMP conditional compilation, where
		!$|c$|*$ should be treated as 2 spaces if the characters
		in columns 3 to 6 are valid fixed form label columns
		characters.  */
	  if (gfc_option.flag_openmp)
	    {
	      if (next_char () == '$')
		{
		  c = next_char ();
		  if (c == 'o' || c == 'O')
		    {
		      if (((c = next_char ()) == 'm' || c == 'M')
			  && ((c = next_char ()) == 'p' || c == 'P'))
			{
			  c = next_char ();
			  if (c != '\n'
			      && ((openmp_flag && continue_flag)
				  || c == ' ' || c == '0'))
			    {
			      c = next_char ();
			      while (gfc_is_whitespace (c))
				c = next_char ();
			      if (c != '\n' && c != '!')
				{
				  /* Canonicalize to *$omp.  */
				  *start.nextc = '*';
				  openmp_flag = 1;
				  gfc_current_locus = start;
				  return;
				}
			    }
			}
		    }
		  else
		    {
		      int digit_seen = 0;

		      for (col = 3; col < 6; col++, c = next_char ())
			if (c == ' ')
			  continue;
			else if (c < '0' || c > '9')
			  break;
			else
			  digit_seen = 1;

		      if (col == 6 && c != '\n'
			  && ((continue_flag && !digit_seen)
			      || c == ' ' || c == '0'))
			{
			  gfc_current_locus = start;
			  start.nextc[0] = ' ';
			  start.nextc[1] = ' ';
			  continue;
			}
		    }
		}
	      gfc_current_locus = start;
	    }
	  skip_comment_line ();
	  continue;
	}

      if (gfc_option.flag_d_lines != -1 && (c == 'd' || c == 'D'))
	{
	  if (gfc_option.flag_d_lines == 0)
	    {
	      skip_comment_line ();
	      continue;
	    }
	  else
	    *start.nextc = c = ' ';
	}

      col = 1;

      while (gfc_is_whitespace (c))
	{
	  c = next_char ();
	  col++;
	}

      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (col != 6 && c == '!')
	{
	  skip_comment_line ();
	  continue;
	}

      break;
    }

  openmp_flag = 0;
  gfc_current_locus = start;
}


/* Skips the current line if it is a comment.  */

void
gfc_skip_comments (void)
{
  if (gfc_current_form == FORM_FREE)
    skip_free_comments ();
  else
    skip_fixed_comments ();
}


/* Get the next character from the input, taking continuation lines
   and end-of-line comments into account.  This implies that comment
   lines between continued lines must be eaten here.  For higher-level
   subroutines, this flattens continued lines into a single logical
   line.  The in_string flag denotes whether we're inside a character
   context or not.  */

int
gfc_next_char_literal (int in_string)
{
  locus old_loc;
  int i, c, prev_openmp_flag;

  continue_flag = 0;

restart:
  c = next_char ();
  if (gfc_at_end ())
    {
      continue_count = 0;
      return c;
    }

  if (gfc_current_form == FORM_FREE)
    {
      bool openmp_cond_flag;

      if (!in_string && c == '!')
	{
	  if (openmp_flag
	      && memcmp (&gfc_current_locus, &openmp_locus,
		 sizeof (gfc_current_locus)) == 0)
	    goto done;

	  /* This line can't be continued */
	  do
	    {
	      c = next_char ();
	    }
	  while (c != '\n');

	  /* Avoid truncation warnings for comment ending lines.  */
	  gfc_current_locus.lb->truncated = 0;

	  goto done;
	}

      if (c != '&')
	goto done;

      /* If the next nonblank character is a ! or \n, we've got a
	 continuation line.  */
      old_loc = gfc_current_locus;

      c = next_char ();
      while (gfc_is_whitespace (c))
	c = next_char ();

      /* Character constants to be continued cannot have commentary
	 after the '&'.  */

      if (in_string && c != '\n')
	{
	  gfc_current_locus = old_loc;
	  c = '&';
	  goto done;
	}

      if (c != '!' && c != '\n')
	{
	  gfc_current_locus = old_loc;
	  c = '&';
	  goto done;
	}

      prev_openmp_flag = openmp_flag;
      continue_flag = 1;
      if (c == '!')
	skip_comment_line ();
      else
	gfc_advance_line ();
      
      if (gfc_at_eof())
	goto not_continuation;

      /* We've got a continuation line.  If we are on the very next line after
	 the last continuation, increment the continuation line count and
	 check whether the limit has been exceeded.  */
      if (gfc_current_locus.lb->linenum == continue_line + 1)
	{
	  if (++continue_count == gfc_option.max_continue_free)
	    {
	      if (gfc_notification_std (GFC_STD_GNU)
		  || pedantic)
		gfc_warning ("Limit of %d continuations exceeded in statement at %C",
			      gfc_option.max_continue_free);
	    }
	}
      continue_line = gfc_current_locus.lb->linenum;

      /* Now find where it continues. First eat any comment lines.  */
      openmp_cond_flag = skip_free_comments ();

      if (prev_openmp_flag != openmp_flag)
	{
	  gfc_current_locus = old_loc;
	  openmp_flag = prev_openmp_flag;
	  c = '&';
	  goto done;
	}

      /* Now that we have a non-comment line, probe ahead for the
	 first non-whitespace character.  If it is another '&', then
	 reading starts at the next character, otherwise we must back
	 up to where the whitespace started and resume from there.  */

      old_loc = gfc_current_locus;

      c = next_char ();
      while (gfc_is_whitespace (c))
	c = next_char ();

      if (openmp_flag)
	{
	  for (i = 0; i < 5; i++, c = next_char ())
	    {
	      gcc_assert (TOLOWER (c) == "!$omp"[i]);
	      if (i == 4)
		old_loc = gfc_current_locus;
	    }
	  while (gfc_is_whitespace (c))
	    c = next_char ();
	}

      if (c != '&')
	{
	  if (in_string)
	    {
	      if (gfc_option.warn_ampersand)
		gfc_warning_now ("Missing '&' in continued character constant at %C");
	      gfc_current_locus.nextc--;
	    }
	  /* Both !$omp and !$ -fopenmp continuation lines have & on the
	     continuation line only optionally.  */
	  else if (openmp_flag || openmp_cond_flag)
	    gfc_current_locus.nextc--;
	  else
	    {
	      c = ' ';
	      gfc_current_locus = old_loc;
	      goto done;
	    }
	}
    }
  else
    {
      /* Fixed form continuation.  */
      if (!in_string && c == '!')
	{
	  /* Skip comment at end of line.  */
	  do
	    {
	      c = next_char ();
	    }
	  while (c != '\n');

	  /* Avoid truncation warnings for comment ending lines.  */
	  gfc_current_locus.lb->truncated = 0;
	}

      if (c != '\n')
	goto done;

      prev_openmp_flag = openmp_flag;
      continue_flag = 1;
      old_loc = gfc_current_locus;

      gfc_advance_line ();
      skip_fixed_comments ();

      /* See if this line is a continuation line.  */
      if (openmp_flag != prev_openmp_flag)
	{
	  openmp_flag = prev_openmp_flag;
	  goto not_continuation;
	}

      if (!openmp_flag)
	for (i = 0; i < 5; i++)
	  {
	    c = next_char ();
	    if (c != ' ')
	      goto not_continuation;
	  }
      else
	for (i = 0; i < 5; i++)
	  {
	    c = next_char ();
	    if (TOLOWER (c) != "*$omp"[i])
	      goto not_continuation;
	  }

      c = next_char ();
      if (c == '0' || c == ' ' || c == '\n')
	goto not_continuation;

      /* We've got a continuation line.  If we are on the very next line after
	 the last continuation, increment the continuation line count and
	 check whether the limit has been exceeded.  */
      if (gfc_current_locus.lb->linenum == continue_line + 1)
	{
	  if (++continue_count == gfc_option.max_continue_fixed)
	    {
	      if (gfc_notification_std (GFC_STD_GNU)
		  || pedantic)
		gfc_warning ("Limit of %d continuations exceeded in statement at %C",
			      gfc_option.max_continue_fixed);
	    }
	}

      if (continue_line < gfc_current_locus.lb->linenum)
	continue_line = gfc_current_locus.lb->linenum;
    }

  /* Ready to read first character of continuation line, which might
     be another continuation line!  */
  goto restart;

not_continuation:
  c = '\n';
  gfc_current_locus = old_loc;

done:
  if (c == '\n')
    continue_count = 0;
  continue_flag = 0;
  return c;
}


/* Get the next character of input, folded to lowercase.  In fixed
   form mode, we also ignore spaces.  When matcher subroutines are
   parsing character literals, they have to call
   gfc_next_char_literal().  */

int
gfc_next_char (void)
{
  int c;

  do
    {
      c = gfc_next_char_literal (0);
    }
  while (gfc_current_form == FORM_FIXED && gfc_is_whitespace (c));

  return TOLOWER (c);
}


int
gfc_peek_char (void)
{
  locus old_loc;
  int c;

  old_loc = gfc_current_locus;
  c = gfc_next_char ();
  gfc_current_locus = old_loc;

  return c;
}


/* Recover from an error.  We try to get past the current statement
   and get lined up for the next.  The next statement follows a '\n'
   or a ';'.  We also assume that we are not within a character
   constant, and deal with finding a '\'' or '"'.  */

void
gfc_error_recovery (void)
{
  char c, delim;

  if (gfc_at_eof ())
    return;

  for (;;)
    {
      c = gfc_next_char ();
      if (c == '\n' || c == ';')
	break;

      if (c != '\'' && c != '"')
	{
	  if (gfc_at_eof ())
	    break;
	  continue;
	}
      delim = c;

      for (;;)
	{
	  c = next_char ();

	  if (c == delim)
	    break;
	  if (c == '\n')
	    return;
	  if (c == '\\')
	    {
	      c = next_char ();
	      if (c == '\n')
		return;
	    }
	}
      if (gfc_at_eof ())
	break;
    }
}


/* Read ahead until the next character to be read is not whitespace.  */

void
gfc_gobble_whitespace (void)
{
  static int linenum = 0;
  locus old_loc;
  int c;

  do
    {
      old_loc = gfc_current_locus;
      c = gfc_next_char_literal (0);
      /* Issue a warning for nonconforming tabs.  We keep track of the line
	 number because the Fortran matchers will often back up and the same
	 line will be scanned multiple times.  */
      if (!gfc_option.warn_tabs && c == '\t')
	{
#ifdef USE_MAPPED_LOCATION
	  int cur_linenum = LOCATION_LINE (gfc_current_locus.lb->location);
#else
	  int cur_linenum = gfc_current_locus.lb->linenum;
#endif
	  if (cur_linenum != linenum)
	    {
	      linenum = cur_linenum;
	      gfc_warning_now ("Nonconforming tab character at %C");
	    }
	}
    }
  while (gfc_is_whitespace (c));

  gfc_current_locus = old_loc;
}


/* Load a single line into pbuf.

   If pbuf points to a NULL pointer, it is allocated.
   We truncate lines that are too long, unless we're dealing with
   preprocessor lines or if the option -ffixed-line-length-none is set,
   in which case we reallocate the buffer to fit the entire line, if
   need be.
   In fixed mode, we expand a tab that occurs within the statement
   label region to expand to spaces that leave the next character in
   the source region.
   load_line returns whether the line was truncated.

   NOTE: The error machinery isn't available at this point, so we can't
	 easily report line and column numbers consistent with other 
	 parts of gfortran.  */

static int
load_line (FILE * input, char **pbuf, int *pbuflen)
{
  static int linenum = 0, current_line = 1;
  int c, maxlen, i, preprocessor_flag, buflen = *pbuflen;
  int trunc_flag = 0, seen_comment = 0;
  int seen_printable = 0, seen_ampersand = 0;
  char *buffer;

  /* Determine the maximum allowed line length.
     The default for free-form is GFC_MAX_LINE, for fixed-form or for
     unknown form it is 72. Refer to the documentation in gfc_option_t.  */
  if (gfc_current_form == FORM_FREE)
    {
      if (gfc_option.free_line_length == -1)
	maxlen = GFC_MAX_LINE;
      else
	maxlen = gfc_option.free_line_length;
    }
  else if (gfc_current_form == FORM_FIXED)
    {
      if (gfc_option.fixed_line_length == -1)
	maxlen = 72;
      else
	maxlen = gfc_option.fixed_line_length;
    }
  else
    maxlen = 72;

  if (*pbuf == NULL)
    {
      /* Allocate the line buffer, storing its length into buflen.  */
      if (maxlen > 0)
	buflen = maxlen;
      else
	buflen = GFC_MAX_LINE;

      *pbuf = gfc_getmem (buflen + 1);
    }

  i = 0;
  buffer = *pbuf;

  preprocessor_flag = 0;
  c = fgetc (input);
  if (c == '#')
    /* In order to not truncate preprocessor lines, we have to
       remember that this is one.  */
    preprocessor_flag = 1;
  ungetc (c, input);

  for (;;)
    {
      c = fgetc (input);

      if (c == EOF)
	break;
      if (c == '\n')
	{
	  /* Check for illegal use of ampersand. See F95 Standard 3.3.1.3.  */
	  if (gfc_current_form == FORM_FREE 
		&& !seen_printable && seen_ampersand)
	    {
	      if (pedantic)
		gfc_error_now
		  ("'&' not allowed by itself in line %d", current_line);
	      else
		gfc_warning_now
		  ("'&' not allowed by itself in line %d", current_line);
	    }
	  break;
	}

      if (c == '\r')
	continue;		/* Gobble characters.  */
      if (c == '\0')
	continue;

      /* Check for illegal use of ampersand. See F95 Standard 3.3.1.3.  */
      if (c == '&')
	seen_ampersand = 1;

      if ((c != ' ' && c != '&' && c != '!') || (c == '!' && !seen_ampersand))
	seen_printable = 1;
      
      if (gfc_current_form == FORM_FREE 
	    && c == '!' && !seen_printable && seen_ampersand)
	{
	  if (pedantic)
	    gfc_error_now (
	      "'&' not allowed by itself with comment in line %d", current_line);
	  else
	    gfc_warning_now (
	      "'&' not allowed by itself with comment in line %d", current_line);
	  seen_printable = 1;
	}

      /* Is this a fixed-form comment?  */
      if (gfc_current_form == FORM_FIXED && i == 0
	  && (c == '*' || c == 'c' || c == 'd'))
	seen_comment = 1;

      if (gfc_current_form == FORM_FIXED && c == '\t' && i <= 6)
	{
	  if (!gfc_option.warn_tabs && seen_comment == 0
	      && current_line != linenum)
	    {
	      linenum = current_line;
	      gfc_warning_now (
		"Nonconforming tab character in column 1 of line %d", linenum);
	    }

	  while (i <= 6)
	    {
	      *buffer++ = ' ';
	      i++;
	    }

	  continue;
	}

      *buffer++ = c;
      i++;

      if (maxlen == 0 || preprocessor_flag)
	{
	  if (i >= buflen)
	    {
	      /* Reallocate line buffer to double size to hold the
		overlong line.  */
	      buflen = buflen * 2;
	      *pbuf = xrealloc (*pbuf, buflen + 1);
	      buffer = (*pbuf)+i;
	    }
	}
      else if (i >= maxlen)
	{
	  /* Truncate the rest of the line.  */
	  for (;;)
	    {
	      c = fgetc (input);
	      if (c == '\n' || c == EOF)
		break;

	      trunc_flag = 1;
	    }

	  ungetc ('\n', input);
	}
    }

  /* Pad lines to the selected line length in fixed form.  */
  if (gfc_current_form == FORM_FIXED
      && gfc_option.fixed_line_length != 0
      && !preprocessor_flag
      && c != EOF)
    {
      while (i++ < maxlen)
	*buffer++ = ' ';
    }

  *buffer = '\0';
  *pbuflen = buflen;
  current_line++;

  return trunc_flag;
}


/* Get a gfc_file structure, initialize it and add it to
   the file stack.  */

static gfc_file *
get_file (const char *name, enum lc_reason reason ATTRIBUTE_UNUSED)
{
  gfc_file *f;

  f = gfc_getmem (sizeof (gfc_file));

  f->filename = gfc_getmem (strlen (name) + 1);
  strcpy (f->filename, name);

  f->next = file_head;
  file_head = f;

  f->included_by = current_file;
  if (current_file != NULL)
    f->inclusion_line = current_file->line;

#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, reason, false, f->filename, 1);
#endif

  return f;
}

/* Deal with a line from the C preprocessor. The
   initial octothorp has already been seen.  */

static void
preprocessor_line (char *c)
{
  bool flag[5];
  int i, line;
  char *filename;
  gfc_file *f;
  int escaped, unescape;

  c++;
  while (*c == ' ' || *c == '\t')
    c++;

  if (*c < '0' || *c > '9')
    goto bad_cpp_line;

  line = atoi (c);

  c = strchr (c, ' ');
  if (c == NULL)
    {
      /* No file name given.  Set new line number.  */
      current_file->line = line;
      return;
    }

  /* Skip spaces.  */
  while (*c == ' ' || *c == '\t')
    c++;

  /* Skip quote.  */
  if (*c != '"')
    goto bad_cpp_line;
  ++c;

  filename = c;

  /* Make filename end at quote.  */
  unescape = 0;
  escaped = false;
  while (*c && ! (! escaped && *c == '"'))
    {
      if (escaped)
        escaped = false;
      else if (*c == '\\')
	{
	  escaped = true;
	  unescape++;
	}
      ++c;
    }

  if (! *c)
    /* Preprocessor line has no closing quote.  */
    goto bad_cpp_line;

  *c++ = '\0';

  /* Undo effects of cpp_quote_string.  */
  if (unescape)
    {
      char *s = filename;
      char *d = gfc_getmem (c - filename - unescape);

      filename = d;
      while (*s)
	{
	  if (*s == '\\')
	    *d++ = *++s;
	  else
	    *d++ = *s;
	  s++;
	}
      *d = '\0';
    }

  /* Get flags.  */

  flag[1] = flag[2] = flag[3] = flag[4] = false;

  for (;;)
    {
      c = strchr (c, ' ');
      if (c == NULL)
	break;

      c++;
      i = atoi (c);

      if (1 <= i && i <= 4)
	flag[i] = true;
    }

  /* Interpret flags.  */

  if (flag[1]) /* Starting new file.  */
    {
      f = get_file (filename, LC_RENAME);
      f->up = current_file;
      current_file = f;
    }

  if (flag[2]) /* Ending current file.  */
    {
      if (!current_file->up
	  || strcmp (current_file->up->filename, filename) != 0)
	{
	  gfc_warning_now ("%s:%d: file %s left but not entered",
			   current_file->filename, current_file->line,
			   filename);
	  if (unescape)
	    gfc_free (filename);
	  return;
	}
      current_file = current_file->up;
    }

  /* The name of the file can be a temporary file produced by
     cpp. Replace the name if it is different.  */

  if (strcmp (current_file->filename, filename) != 0)
    {
      gfc_free (current_file->filename);
      current_file->filename = gfc_getmem (strlen (filename) + 1);
      strcpy (current_file->filename, filename);
    }

  /* Set new line number.  */
  current_file->line = line;
  if (unescape)
    gfc_free (filename);
  return;

 bad_cpp_line:
  gfc_warning_now ("%s:%d: Illegal preprocessor directive",
		   current_file->filename, current_file->line);
  current_file->line++;
}


static try load_file (const char *, bool);

/* include_line()-- Checks a line buffer to see if it is an include
   line.  If so, we call load_file() recursively to load the included
   file.  We never return a syntax error because a statement like
   "include = 5" is perfectly legal.  We return false if no include was
   processed or true if we matched an include.  */

static bool
include_line (char *line)
{
  char quote, *c, *begin, *stop;

  c = line;

  if (gfc_option.flag_openmp)
    {
      if (gfc_current_form == FORM_FREE)
	{
	  while (*c == ' ' || *c == '\t')
	    c++;
	  if (*c == '!' && c[1] == '$' && (c[2] == ' ' || c[2] == '\t'))
	    c += 3;
	}
      else
	{
	  if ((*c == '!' || *c == 'c' || *c == 'C' || *c == '*')
	      && c[1] == '$' && (c[2] == ' ' || c[2] == '\t'))
	    c += 3;
	}
    }

  while (*c == ' ' || *c == '\t')
    c++;

  if (strncasecmp (c, "include", 7))
      return false;

  c += 7;
  while (*c == ' ' || *c == '\t')
    c++;

  /* Find filename between quotes.  */
  
  quote = *c++;
  if (quote != '"' && quote != '\'')
    return false;

  begin = c;

  while (*c != quote && *c != '\0')
    c++;

  if (*c == '\0')
    return false;

  stop = c++;
  
  while (*c == ' ' || *c == '\t')
    c++;

  if (*c != '\0' && *c != '!')
    return false;

  /* We have an include line at this point.  */

  *stop = '\0'; /* It's ok to trash the buffer, as this line won't be
		   read by anything else.  */

  load_file (begin, false);
  return true;
}

/* Load a file into memory by calling load_line until the file ends.  */

static try
load_file (const char *filename, bool initial)
{
  char *line;
  gfc_linebuf *b;
  gfc_file *f;
  FILE *input;
  int len, line_len;

  for (f = current_file; f; f = f->up)
    if (strcmp (filename, f->filename) == 0)
      {
	gfc_error_now ("File '%s' is being included recursively", filename);
	return FAILURE;
      }

  if (initial)
    {
      if (gfc_src_file)
	{
	  input = gfc_src_file;
	  gfc_src_file = NULL;
	}
      else
	input = gfc_open_file (filename);
      if (input == NULL)
	{
	  gfc_error_now ("Can't open file '%s'", filename);
	  return FAILURE;
	}
    }
  else
    {
      input = gfc_open_included_file (filename, false);
      if (input == NULL)
	{
	  gfc_error_now ("Can't open included file '%s'", filename);
	  return FAILURE;
	}
    }

  /* Load the file.  */

  f = get_file (filename, initial ? LC_RENAME : LC_ENTER);
  f->up = current_file;
  current_file = f;
  current_file->line = 1;
  line = NULL;
  line_len = 0;

  if (initial && gfc_src_preprocessor_lines[0])
    {
      preprocessor_line (gfc_src_preprocessor_lines[0]);
      gfc_free (gfc_src_preprocessor_lines[0]);
      gfc_src_preprocessor_lines[0] = NULL;
      if (gfc_src_preprocessor_lines[1])
	{
	  preprocessor_line (gfc_src_preprocessor_lines[1]);
	  gfc_free (gfc_src_preprocessor_lines[1]);
	  gfc_src_preprocessor_lines[1] = NULL;
	}
    }

  for (;;)
    {
      int trunc = load_line (input, &line, &line_len);

      len = strlen (line);
      if (feof (input) && len == 0)
	break;

      /* There are three things this line can be: a line of Fortran
	 source, an include line or a C preprocessor directive.  */

      if (line[0] == '#')
	{
	  preprocessor_line (line);
	  continue;
	}

      if (include_line (line))
	{
	  current_file->line++;
	  continue;
	}

      /* Add line.  */

      b = gfc_getmem (gfc_linebuf_header_size + len + 1);

#ifdef USE_MAPPED_LOCATION
      b->location
	= linemap_line_start (&line_table, current_file->line++, 120);
#else
      b->linenum = current_file->line++;
#endif
      b->file = current_file;
      b->truncated = trunc;
      strcpy (b->line, line);

      if (line_head == NULL)
	line_head = b;
      else
	line_tail->next = b;

      line_tail = b;
    }

  /* Release the line buffer allocated in load_line.  */
  gfc_free (line);

  fclose (input);

  current_file = current_file->up;
#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, LC_LEAVE, 0, NULL, 0);
#endif
  return SUCCESS;
}


/* Open a new file and start scanning from that file. Returns SUCCESS
   if everything went OK, FAILURE otherwise.  If form == FORM_UKNOWN
   it tries to determine the source form from the filename, defaulting
   to free form.  */

try
gfc_new_file (void)
{
  try result;

  result = load_file (gfc_source_file, true);

  gfc_current_locus.lb = line_head;
  gfc_current_locus.nextc = (line_head == NULL) ? NULL : line_head->line;

#if 0 /* Debugging aid.  */
  for (; line_head; line_head = line_head->next)
    gfc_status ("%s:%3d %s\n", line_head->file->filename, 
#ifdef USE_MAPPED_LOCATION
		LOCATION_LINE (line_head->location),
#else
		line_head->linenum,
#endif
		line_head->line);

  exit (0);
#endif

  return result;
}

static char *
unescape_filename (const char *ptr)
{
  const char *p = ptr, *s;
  char *d, *ret;
  int escaped, unescape = 0;

  /* Make filename end at quote.  */
  escaped = false;
  while (*p && ! (! escaped && *p == '"'))
    {
      if (escaped)
	escaped = false;
      else if (*p == '\\')
	{
	  escaped = true;
	  unescape++;
	}
      ++p;
    }

  if (! *p || p[1])
    return NULL;

  /* Undo effects of cpp_quote_string.  */
  s = ptr;
  d = gfc_getmem (p + 1 - ptr - unescape);
  ret = d;

  while (s != p)
    {
      if (*s == '\\')
	*d++ = *++s;
      else
	*d++ = *s;
      s++;
    }
  *d = '\0';
  return ret;
}

/* For preprocessed files, if the first tokens are of the form # NUM.
   handle the directives so we know the original file name.  */

const char *
gfc_read_orig_filename (const char *filename, const char **canon_source_file)
{
  int c, len;
  char *dirname;

  gfc_src_file = gfc_open_file (filename);
  if (gfc_src_file == NULL)
    return NULL;

  c = fgetc (gfc_src_file);
  ungetc (c, gfc_src_file);

  if (c != '#')
    return NULL;

  len = 0;
  load_line (gfc_src_file, &gfc_src_preprocessor_lines[0], &len);

  if (strncmp (gfc_src_preprocessor_lines[0], "# 1 \"", 5) != 0)
    return NULL;

  filename = unescape_filename (gfc_src_preprocessor_lines[0] + 5);
  if (filename == NULL)
    return NULL;

  c = fgetc (gfc_src_file);
  ungetc (c, gfc_src_file);

  if (c != '#')
    return filename;

  len = 0;
  load_line (gfc_src_file, &gfc_src_preprocessor_lines[1], &len);

  if (strncmp (gfc_src_preprocessor_lines[1], "# 1 \"", 5) != 0)
    return filename;

  dirname = unescape_filename (gfc_src_preprocessor_lines[1] + 5);
  if (dirname == NULL)
    return filename;

  len = strlen (dirname);
  if (len < 3 || dirname[len - 1] != '/' || dirname[len - 2] != '/')
    {
      gfc_free (dirname);
      return filename;
    }
  dirname[len - 2] = '\0';
  set_src_pwd (dirname);

  if (! IS_ABSOLUTE_PATH (filename))
    {
      char *p = gfc_getmem (len + strlen (filename));

      memcpy (p, dirname, len - 2);
      p[len - 2] = '/';
      strcpy (p + len - 1, filename);
      *canon_source_file = p;
    }

  gfc_free (dirname);
  return filename;
}
