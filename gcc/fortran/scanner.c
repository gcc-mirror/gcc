/* Character scanner.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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

static int continue_flag, end_flag;

gfc_source_form gfc_current_form;
static gfc_linebuf *line_head, *line_tail;
       
locus gfc_current_locus;
char *gfc_source_file;
      

/* Main scanner initialization.  */

void
gfc_scanner_init_1 (void)
{
  file_head = NULL;
  line_head = NULL;
  line_tail = NULL;

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
   given if necessary.  */

FILE *
gfc_open_included_file (const char *name)
{
  char fullname[PATH_MAX];
  gfc_directorylist *p;
  FILE *f;

  f = gfc_open_file (name);
  if (f != NULL)
    return f;

  for (p = include_dirs; p; p = p->next)
    {
      if (strlen (p->path) + strlen (name) + 1 > PATH_MAX)
	continue;

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

  c = *gfc_current_locus.nextc++;
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
   on which the first nonblank line is a '!'.  */

static void
skip_free_comments (void)
{
  locus start;
  char c;

  for (;;)
    {
      start = gfc_current_locus;
      if (gfc_at_eof ())
	break;

      do
	{
	  c = next_char ();
	}
      while (gfc_is_whitespace (c));

      if (c == '\n')
	{
	  gfc_advance_line ();
	  continue;
	}

      if (c == '!')
	{
	  skip_comment_line ();
	  continue;
	}

      break;
    }

  gfc_current_locus = start;
}


/* Skip comment lines in fixed source mode.  We have the same rules as
   in skip_free_comment(), except that we can have a 'c', 'C' or '*'
   in column 1, and a '!' cannot be in column 6.  */

static void
skip_fixed_comments (void)
{
  locus start;
  int col;
  char c;

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
	  skip_comment_line ();
	  continue;
	}

      col = 1;
      do
	{
	  c = next_char ();
	  col++;
	}
      while (gfc_is_whitespace (c));

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

  gfc_current_locus = start;
}


/* Skips the current line if it is a comment.  Assumes that we are at
   the start of the current line.  */

void
gfc_skip_comments (void)
{

  if (!gfc_at_bol () || gfc_current_form == FORM_FREE)
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
  int i, c;

  continue_flag = 0;

restart:
  c = next_char ();
  if (gfc_at_end ())
    return c;

  if (gfc_current_form == FORM_FREE)
    {

      if (!in_string && c == '!')
	{
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

      continue_flag = 1;
      if (c == '!')
	skip_comment_line ();
      else
	gfc_advance_line ();

      /* We've got a continuation line and need to find where it continues.
         First eat any comment lines.  */
      gfc_skip_comments ();

      /* Now that we have a non-comment line, probe ahead for the
         first non-whitespace character.  If it is another '&', then
         reading starts at the next character, otherwise we must back
         up to where the whitespace started and resume from there.  */

      old_loc = gfc_current_locus;

      c = next_char ();
      while (gfc_is_whitespace (c))
	c = next_char ();

      if (c != '&')
	gfc_current_locus = old_loc;

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

      continue_flag = 1;
      old_loc = gfc_current_locus;

      gfc_advance_line ();
      gfc_skip_comments ();

      /* See if this line is a continuation line.  */
      for (i = 0; i < 5; i++)
	{
	  c = next_char ();
	  if (c != ' ')
	    goto not_continuation;
	}

      c = next_char ();
      if (c == '0' || c == ' ')
	goto not_continuation;
    }

  /* Ready to read first character of continuation line, which might
     be another continuation line!  */
  goto restart;

not_continuation:
  c = '\n';
  gfc_current_locus = old_loc;

done:
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
  locus old_loc;
  int c;

  do
    {
      old_loc = gfc_current_locus;
      c = gfc_next_char_literal (0);
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
   load_line returns wether the line was truncated.  */

static int
load_line (FILE * input, char **pbuf)
{
  int c, maxlen, i, preprocessor_flag;
  int trunc_flag = 0;
  static int buflen = 0;
  char *buffer;

  /* Determine the maximum allowed line length.  */
  if (gfc_current_form == FORM_FREE)
    maxlen = GFC_MAX_LINE;
  else
    maxlen = gfc_option.fixed_line_length;

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
	break;

      if (c == '\r')
	continue;		/* Gobble characters.  */
      if (c == '\0')
	continue;

      if (c == '\032')
	{
	  /* Ctrl-Z ends the file.  */
	  while (fgetc (input) != EOF);
	  break;
	}

      if (gfc_current_form == FORM_FIXED && c == '\t' && i <= 6)
	{			/* Tab expansion.  */
	  while (i <= 6)
	    {
	      *buffer++ = ' ';
	      i++;
	    }

	  continue;
	}

      *buffer++ = c;
      i++;

      if (i >= buflen && (maxlen == 0 || preprocessor_flag))
	{
	  /* Reallocate line buffer to double size to hold the
	     overlong line.  */
	  buflen = buflen * 2;
	  *pbuf = xrealloc (*pbuf, buflen);
	  buffer = (*pbuf)+i;
	}
      else if (i >= buflen)
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
      && gfc_option.fixed_line_length > 0
      && !preprocessor_flag
      && c != EOF)
    while (i++ < buflen)
      *buffer++ = ' ';

  *buffer = '\0';

  return trunc_flag;
}


/* Get a gfc_file structure, initialize it and add it to
   the file stack.  */

static gfc_file *
get_file (char *name, enum lc_reason reason ATTRIBUTE_UNUSED)
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
  int escaped;

  c++;
  while (*c == ' ' || *c == '\t')
    c++;

  if (*c < '0' || *c > '9')
    goto bad_cpp_line;

  line = atoi (c);

  /* Set new line number.  */
  current_file->line = line;

  c = strchr (c, ' '); 
  if (c == NULL)
    /* No file name given.  */
    return;



  /* Skip spaces.  */
  while (*c == ' ' || *c == '\t')
    c++;

  /* Skip quote.  */
  if (*c != '"')
    goto bad_cpp_line;
  ++c;

  filename = c;

  /* Make filename end at quote.  */
  escaped = false;
  while (*c && ! (! escaped && *c == '"'))
    {
      if (escaped)
        escaped = false;
      else
        escaped = *c == '\\';
      ++c;
    }

  if (! *c)
    /* Preprocessor line has no closing quote.  */
    goto bad_cpp_line;

  *c++ = '\0';



  /* Get flags.  */
  
  flag[1] = flag[2] = flag[3] = flag[4] = flag[5] = false;

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
  
  if (flag[1] || flag[3]) /* Starting new file.  */
    {
      f = get_file (filename, LC_RENAME);
      f->up = current_file;
      current_file = f;
    }
  
  if (flag[2]) /* Ending current file.  */
    {
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

  return;

 bad_cpp_line:
  gfc_warning_now ("%s:%d: Illegal preprocessor directive", 
		   current_file->filename, current_file->line);
  current_file->line++;
}


static try load_file (char *, bool);

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
load_file (char *filename, bool initial)
{
  char *line;
  gfc_linebuf *b;
  gfc_file *f;
  FILE *input;
  int len;

  for (f = current_file; f; f = f->up)
    if (strcmp (filename, f->filename) == 0)
      {
	gfc_error_now ("File '%s' is being included recursively", filename);
	return FAILURE;
      }

  if (initial)
    {
      input = gfc_open_file (filename);
      if (input == NULL)
	{
	  gfc_error_now ("Can't open file '%s'", filename);
	  return FAILURE;
	}
    }
  else
    {
      input = gfc_open_included_file (filename);
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

  for (;;) 
    {
      int trunc = load_line (input, &line);

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


/* Determine the source form from the filename extension.  We assume
   case insensitivity.  */

static gfc_source_form
form_from_filename (const char *filename)
{

  static const struct
  {
    const char *extension;
    gfc_source_form form;
  }
  exttype[] =
  {
    {
    ".f90", FORM_FREE}
    ,
    {
    ".f95", FORM_FREE}
    ,
    {
    ".f", FORM_FIXED}
    ,
    {
    ".for", FORM_FIXED}
    ,
    {
    "", FORM_UNKNOWN}
  };		/* sentinel value */

  gfc_source_form f_form;
  const char *fileext;
  int i;

  /* Find end of file name.  */
  i = 0;
  while ((i < PATH_MAX) && (filename[i] != '\0'))
    i++;

  /* Improperly terminated or too-long filename.  */
  if (i == PATH_MAX)
    return FORM_UNKNOWN;

  /* Find last period.  */
  while (i >= 0 && (filename[i] != '.'))
    i--;

  /* Did we see a file extension?  */
  if (i < 0)
    return FORM_UNKNOWN; /* Nope  */

  /* Get file extension and compare it to others.  */
  fileext = &(filename[i]);

  i = -1;
  f_form = FORM_UNKNOWN;
  do
    {
      i++;
      if (strcasecmp (fileext, exttype[i].extension) == 0)
	{
	  f_form = exttype[i].form;
	  break;
	}
    }
  while (exttype[i].form != FORM_UNKNOWN);

  return f_form;
}


/* Open a new file and start scanning from that file. Returns SUCCESS
   if everything went OK, FAILURE otherwise.  If form == FORM_UKNOWN
   it tries to determine the source form from the filename, defaulting
   to free form.  */

try
gfc_new_file (const char *filename, gfc_source_form form)
{
  try result;

  if (filename != NULL)
    {
      gfc_source_file = gfc_getmem (strlen (filename) + 1);
      strcpy (gfc_source_file, filename);
    }
  else
    gfc_source_file = NULL;

  /* Decide which form the file will be read in as.  */

  if (form != FORM_UNKNOWN)
    gfc_current_form = form;
  else
    {
      gfc_current_form = form_from_filename (filename);

      if (gfc_current_form == FORM_UNKNOWN)
	{
	  gfc_current_form = FORM_FREE;
	  gfc_warning_now ("Reading file '%s' as free form.", 
			   (filename[0] == '\0') ? "<stdin>" : filename); 
	}
    }

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
