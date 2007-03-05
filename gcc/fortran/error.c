/* Handle errors.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006 Free Software
   Foundation, Inc.
   Contributed by Andy Vaught & Niels Kristian Bech Jensen

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

/* Handle the inevitable errors.  A major catch here is that things
   flagged as errors in one match subroutine can conceivably be legal
   elsewhere.  This means that error messages are recorded and saved
   for possible use later.  If a line does not match a legal
   construction, then the saved error message is reported.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "gfortran.h"

int gfc_suppress_error = 0;

static int terminal_width, buffer_flag, errors, warnings;

static gfc_error_buf error_buffer, warning_buffer, *cur_error_buffer;


/* Per-file error initialization.  */

void
gfc_error_init_1 (void)
{
  terminal_width = gfc_terminal_width ();
  errors = 0;
  warnings = 0;
  buffer_flag = 0;
}


/* Set the flag for buffering errors or not.  */

void
gfc_buffer_error (int flag)
{
  buffer_flag = flag;
}


/* Add a single character to the error buffer or output depending on
   buffer_flag.  */

static void
error_char (char c)
{
  if (buffer_flag)
    {
      if (cur_error_buffer->index >= cur_error_buffer->allocated)
	{
	  cur_error_buffer->allocated =
	    cur_error_buffer->allocated
	    ? cur_error_buffer->allocated * 2 : 1000;
	  cur_error_buffer->message
	    = xrealloc (cur_error_buffer->message,
			cur_error_buffer->allocated);
	}
      cur_error_buffer->message[cur_error_buffer->index++] = c;
    }
  else
    {
      if (c != 0)
	{
	  /* We build up complete lines before handing things
	     over to the library in order to speed up error printing.  */
	  static char *line;
	  static size_t allocated = 0, index = 0;

	  if (index + 1 >= allocated)
	    {
	      allocated = allocated ? allocated * 2 : 1000;
	      line = xrealloc (line, allocated);
	    }
	  line[index++] = c;
	  if (c == '\n')
	    {
	      line[index] = '\0';
	      fputs (line, stderr);
	      index = 0;
	    }
	}
    }
}


/* Copy a string to wherever it needs to go.  */

static void
error_string (const char *p)
{
  while (*p)
    error_char (*p++);
}


/* Print a formatted integer to the error buffer or output.  */

#define IBUF_LEN 30

static void
error_integer (int i)
{
  char *p, int_buf[IBUF_LEN];

  if (i < 0)
    {
      i = -i;
      error_char ('-');
    }

  p = int_buf + IBUF_LEN - 1;
  *p-- = '\0';

  if (i == 0)
    *p-- = '0';

  while (i > 0)
    {
      *p-- = i % 10 + '0';
      i = i / 10;
    }

  error_string (p + 1);
}


/* Show the file, where it was included, and the source line, give a
   locus.  Calls error_printf() recursively, but the recursion is at
   most one level deep.  */

static void error_printf (const char *, ...) ATTRIBUTE_GCC_GFC(1,2);

static void
show_locus (locus * loc, int c1, int c2)
{
  gfc_linebuf *lb;
  gfc_file *f;
  char c, *p;
  int i, m, offset, cmax;

  /* TODO: Either limit the total length and number of included files
     displayed or add buffering of arbitrary number of characters in
     error messages.  */

  /* Write out the error header line, giving the source file and error
     location (in GNU standard "[file]:[line].[column]:" format),
     followed by an "included by" stack and a blank line.  This header
     format is matched by a testsuite parser defined in
     lib/gfortran-dg.exp.  */

  lb = loc->lb;
  f = lb->file;

  error_string (f->filename);
  error_char (':');
    
#ifdef USE_MAPPED_LOCATION
  error_integer (LOCATION_LINE (lb->location));
#else
  error_integer (lb->linenum);
#endif

  if ((c1 > 0) || (c2 > 0))
    error_char ('.');

  if (c1 > 0)
    error_integer (c1);

  if ((c1 > 0) && (c2 > 0))
    error_char ('-');

  if (c2 > 0)
    error_integer (c2);

  error_char (':');
  error_char ('\n');

  for (;;)
    {
      i = f->inclusion_line;

      f = f->included_by;
      if (f == NULL) break;

      error_printf ("    Included at %s:%d:", f->filename, i);
    }

  error_char ('\n');

  /* Calculate an appropriate horizontal offset of the source line in
     order to get the error locus within the visible portion of the
     line.  Note that if the margin of 5 here is changed, the
     corresponding margin of 10 in show_loci should be changed.  */

  offset = 0;

  /* When the loci is not associated with a column, it will have a
     value of zero.  We adjust this to 1 so that it will appear.  */
     
  if (c1 == 0)
    c1 = 1;
  if (c2 == 0)
    c2 = 1;

  /* If the two loci would appear in the same column, we shift
     '2' one column to the right, so as to print '12' rather than
     just '1'.  We do this here so it will be accounted for in the
     margin calculations.  */

  if (c1 == c2)
    c2 += 1;

  cmax = (c1 < c2) ? c2 : c1;
  if (cmax > terminal_width - 5)
    offset = cmax - terminal_width + 5;

  /* TODO: Is there a good reason for the following apparently-redundant
     check, and the similar ones in the single-locus cases below?  */

  if (offset < 0)
    offset = 0;

  /* Show the line itself, taking care not to print more than what can
     show up on the terminal.  Tabs are converted to spaces, and 
     nonprintable characters are converted to a "\xNN" sequence.  */

  /* TODO: Although setting i to the terminal width is clever, it fails
     to work correctly when nonprintable characters exist.  A better 
     solution should be found.  */

  p = lb->line + offset;
  i = strlen (p);
  if (i > terminal_width)
    i = terminal_width - 1;

  for (; i > 0; i--)
    {
      c = *p++;
      if (c == '\t')
	c = ' ';

      if (ISPRINT (c))
	error_char (c);
      else
	{
	  error_char ('\\');
	  error_char ('x');

	  m = ((c >> 4) & 0x0F) + '0';
	  if (m > '9')
	    m += 'A' - '9' - 1;
	  error_char (m);

	  m = (c & 0x0F) + '0';
	  if (m > '9')
	    m += 'A' - '9' - 1;
	  error_char (m);
	}
    }

  error_char ('\n');

  /* Show the '1' and/or '2' corresponding to the column of the error
     locus.  Note that a value of -1 for c1 or c2 will simply cause 
     the relevant number not to be printed.  */

  c1 -= offset;
  c2 -= offset;

  for (i = 1; i <= cmax; i++)
    {
      if (i == c1)
	error_char ('1');
      else if (i == c2)
	error_char ('2');
      else
	error_char (' ');
    }

  error_char ('\n');

}


/* As part of printing an error, we show the source lines that caused
   the problem.  We show at least one, and possibly two loci; the two
   loci may or may not be on the same source line.  */

static void
show_loci (locus * l1, locus * l2)
{
  int m, c1, c2;

  if (l1 == NULL || l1->lb == NULL)
    {
      error_printf ("<During initialization>\n");
      return;
    }

  /* While calculating parameters for printing the loci, we consider possible
     reasons for printing one per line.  If appropriate, print the loci
     individually; otherwise we print them both on the same line.  */

  c1 = l1->nextc - l1->lb->line;
  if (l2 == NULL)
    {
      show_locus (l1, c1, -1);
      return;
    }

  c2 = l2->nextc - l2->lb->line;

  if (c1 < c2)
    m = c2 - c1;
  else
    m = c1 - c2;

  /* Note that the margin value of 10 here needs to be less than the 
     margin of 5 used in the calculation of offset in show_locus.  */

  if (l1->lb != l2->lb || m > terminal_width - 10)
    {
      show_locus (l1, c1, -1);
      show_locus (l2, -1, c2);
      return;
    }

  show_locus (l1, c1, c2);

  return;

}


/* Workhorse for the error printing subroutines.  This subroutine is
   inspired by g77's error handling and is similar to printf() with
   the following %-codes:

   %c Character, %d or %i Integer, %s String, %% Percent
   %L  Takes locus argument
   %C  Current locus (no argument)

   If a locus pointer is given, the actual source line is printed out
   and the column is indicated.  Since we want the error message at
   the bottom of any source file information, we must scan the
   argument list twice -- once to determine whether the loci are 
   present and record this for printing, and once to print the error
   message after and loci have been printed.  A maximum of two locus
   arguments are permitted.
   
   This function is also called (recursively) by show_locus in the
   case of included files; however, as show_locus does not resupply
   any loci, the recursion is at most one level deep.  */

#define MAX_ARGS 10

static void ATTRIBUTE_GCC_GFC(2,0)
error_print (const char *type, const char *format0, va_list argp)
{
  enum { TYPE_CURRENTLOC, TYPE_LOCUS, TYPE_INTEGER, TYPE_CHAR, TYPE_STRING,
	 NOTYPE };
  struct
  {
    int type;
    int pos;
    union
    {
      int intval;
      char charval;
      const char * stringval;
    } u;
  } arg[MAX_ARGS], spec[MAX_ARGS];
  /* spec is the array of specifiers, in the same order as they
     appear in the format string.  arg is the array of arguments,
     in the same order as they appear in the va_list.  */

  char c;
  int i, n, have_l1, pos, maxpos;
  locus *l1, *l2, *loc;
  const char *format;

  l1 = l2 = NULL;

  have_l1 = 0;
  pos = -1;
  maxpos = -1;

  n = 0;
  format = format0;

  for (i = 0; i < MAX_ARGS; i++)
    {
      arg[i].type = NOTYPE;
      spec[i].pos = -1;
    }

  /* First parse the format string for position specifiers.  */
  while (*format)
    {
      c = *format++;
      if (c != '%')
	continue;

      if (*format == '%')
	continue;

      if (ISDIGIT (*format))
	{
	  /* This is a position specifier.  For example, the number
	     12 in the format string "%12$d", which specifies the third
	     argument of the va_list, formatted in %d format.
	     For details, see "man 3 printf".  */
	  pos = atoi(format) - 1;
	  gcc_assert (pos >= 0);
	  while (ISDIGIT(*format))
	    format++;
	  gcc_assert (*format++ == '$');
	}
      else
	pos++;

      c = *format++;

      if (pos > maxpos)
	maxpos = pos;

      switch (c)
	{
	  case 'C':
	    arg[pos].type = TYPE_CURRENTLOC;
	    break;

	  case 'L':
	    arg[pos].type = TYPE_LOCUS;
	    break;

	  case 'd':
	  case 'i':
	    arg[pos].type = TYPE_INTEGER;
	    break;

	  case 'c':
	    arg[pos].type = TYPE_CHAR;
	    break;

	  case 's':
	    arg[pos].type = TYPE_STRING;
	    break;

	  default:
	    gcc_unreachable ();
	}

      spec[n++].pos = pos;
    }

  /* Then convert the values for each %-style argument.  */
  for (pos = 0; pos <= maxpos; pos++)
    {
      gcc_assert (arg[pos].type != NOTYPE);
      switch (arg[pos].type)
	{
	  case TYPE_CURRENTLOC:
	    loc = &gfc_current_locus;
	    /* Fall through.  */

	  case TYPE_LOCUS:
	    if (arg[pos].type == TYPE_LOCUS)
	      loc = va_arg (argp, locus *);

	    if (have_l1)
	      {
		l2 = loc;
		arg[pos].u.stringval = "(2)";
	      }
	    else
	      {
		l1 = loc;
		have_l1 = 1;
		arg[pos].u.stringval = "(1)";
	      }
	    break;

	  case TYPE_INTEGER:
	    arg[pos].u.intval = va_arg (argp, int);
	    break;

	  case TYPE_CHAR:
	    arg[pos].u.charval = (char) va_arg (argp, int);
	    break;

	  case TYPE_STRING:
	    arg[pos].u.stringval = (const char *) va_arg (argp, char *);
	    break;

	  default:
	    gcc_unreachable ();
	}
    }

  for (n = 0; spec[n].pos >= 0; n++)
    spec[n].u = arg[spec[n].pos].u;

  /* Show the current loci if we have to.  */
  if (have_l1)
    show_loci (l1, l2);

  if (*type)
    {
      error_string (type);
      error_char (' ');
    }

  have_l1 = 0;
  format = format0;
  n = 0;

  for (; *format; format++)
    {
      if (*format != '%')
	{
	  error_char (*format);
	  continue;
	}

      format++;
      if (ISDIGIT(*format))
	{
	  /* This is a position specifier.  See comment above.  */
	  while (ISDIGIT(*format))
	    format++;
	    
	  /* Skip over the dollar sign.  */
	  format++;
	}
	
      switch (*format)
	{
	case '%':
	  error_char ('%');
	  break;

	case 'c':
	  error_char (spec[n++].u.charval);
	  break;

	case 's':
	case 'C':		/* Current locus */
	case 'L':		/* Specified locus */
	  error_string (spec[n++].u.stringval);
	  break;

	case 'd':
	case 'i':
	  error_integer (spec[n++].u.intval);
	  break;
	}
    }

  error_char ('\n');
}


/* Wrapper for error_print().  */

static void
error_printf (const char *nocmsgid, ...)
{
  va_list argp;

  va_start (argp, nocmsgid);
  error_print ("", _(nocmsgid), argp);
  va_end (argp);
}


/* Increment the number of errors, and check whether too many have 
   been printed.  */

static void
gfc_increment_error_count (void)
{
  errors++;
  if ((gfc_option.max_errors != 0) && (errors >= gfc_option.max_errors))
    gfc_fatal_error ("Error count reached limit of %d.", gfc_option.max_errors);
}


/* Issue a warning.  */

void
gfc_warning (const char *nocmsgid, ...)
{
  va_list argp;

  if (inhibit_warnings)
    return;

  warning_buffer.flag = 1;
  warning_buffer.index = 0;
  cur_error_buffer = &warning_buffer;

  va_start (argp, nocmsgid);
  error_print (_("Warning:"), _(nocmsgid), argp);
  va_end (argp);

  error_char ('\0');

  if (buffer_flag == 0)
    warnings++;
}


/* Whether, for a feature included in a given standard set (GFC_STD_*),
   we should issue an error or a warning, or be quiet.  */

notification
gfc_notification_std (int std)
{
  bool warning;

  warning = ((gfc_option.warn_std & std) != 0) && !inhibit_warnings;
  if ((gfc_option.allow_std & std) != 0 && !warning)
    return SILENT;

  return warning ? WARNING : ERROR;
}


/* Possibly issue a warning/error about use of a nonstandard (or deleted)
   feature.  An error/warning will be issued if the currently selected
   standard does not contain the requested bits.  Return FAILURE if
   an error is generated.  */

try
gfc_notify_std (int std, const char *nocmsgid, ...)
{
  va_list argp;
  bool warning;

  warning = ((gfc_option.warn_std & std) != 0)
	    && !inhibit_warnings;
  if ((gfc_option.allow_std & std) != 0
      && !warning)
    return SUCCESS;

  if (gfc_suppress_error)
    return warning ? SUCCESS : FAILURE;
  
  cur_error_buffer = warning ? &warning_buffer : &error_buffer;
  cur_error_buffer->flag = 1;
  cur_error_buffer->index = 0;

  va_start (argp, nocmsgid);
  if (warning)
    error_print (_("Warning:"), _(nocmsgid), argp);
  else
    error_print (_("Error:"), _(nocmsgid), argp);
  va_end (argp);

  error_char ('\0');

  if (buffer_flag == 0)
    {
      if (warning)
	warnings++;
      else
	gfc_increment_error_count();
    }

  return warning ? SUCCESS : FAILURE;
}


/* Immediate warning (i.e. do not buffer the warning).  */

void
gfc_warning_now (const char *nocmsgid, ...)
{
  va_list argp;
  int i;

  if (inhibit_warnings)
    return;

  i = buffer_flag;
  buffer_flag = 0;
  warnings++;

  va_start (argp, nocmsgid);
  error_print (_("Warning:"), _(nocmsgid), argp);
  va_end (argp);

  error_char ('\0');
  buffer_flag = i;
}


/* Clear the warning flag.  */

void
gfc_clear_warning (void)
{
  warning_buffer.flag = 0;
}


/* Check to see if any warnings have been saved.
   If so, print the warning.  */

void
gfc_warning_check (void)
{
  if (warning_buffer.flag)
    {
      warnings++;
      if (warning_buffer.message != NULL)
	fputs (warning_buffer.message, stderr);
      warning_buffer.flag = 0;
    }
}


/* Issue an error.  */

void
gfc_error (const char *nocmsgid, ...)
{
  va_list argp;

  if (gfc_suppress_error)
    return;

  error_buffer.flag = 1;
  error_buffer.index = 0;
  cur_error_buffer = &error_buffer;

  va_start (argp, nocmsgid);
  error_print (_("Error:"), _(nocmsgid), argp);
  va_end (argp);

  error_char ('\0');

  if (buffer_flag == 0)
    gfc_increment_error_count();
}


/* Immediate error.  */

void
gfc_error_now (const char *nocmsgid, ...)
{
  va_list argp;
  int i;

  error_buffer.flag = 1;
  error_buffer.index = 0;
  cur_error_buffer = &error_buffer;

  i = buffer_flag;
  buffer_flag = 0;

  va_start (argp, nocmsgid);
  error_print (_("Error:"), _(nocmsgid), argp);
  va_end (argp);

  error_char ('\0');

  gfc_increment_error_count();

  buffer_flag = i;

  if (flag_fatal_errors)
    exit (1);
}


/* Fatal error, never returns.  */

void
gfc_fatal_error (const char *nocmsgid, ...)
{
  va_list argp;

  buffer_flag = 0;

  va_start (argp, nocmsgid);
  error_print (_("Fatal Error:"), _(nocmsgid), argp);
  va_end (argp);

  exit (3);
}


/* This shouldn't happen... but sometimes does.  */

void
gfc_internal_error (const char *format, ...)
{
  va_list argp;

  buffer_flag = 0;

  va_start (argp, format);

  show_loci (&gfc_current_locus, NULL);
  error_printf ("Internal Error at (1):");

  error_print ("", format, argp);
  va_end (argp);

  exit (ICE_EXIT_CODE);
}


/* Clear the error flag when we start to compile a source line.  */

void
gfc_clear_error (void)
{
  error_buffer.flag = 0;
}


/* Tests the state of error_flag.  */

int
gfc_error_flag_test (void)
{
  return error_buffer.flag;
}


/* Check to see if any errors have been saved.
   If so, print the error.  Returns the state of error_flag.  */

int
gfc_error_check (void)
{
  int rc;

  rc = error_buffer.flag;

  if (error_buffer.flag)
    {
      if (error_buffer.message != NULL)
	fputs (error_buffer.message, stderr);
      error_buffer.flag = 0;

      gfc_increment_error_count();

      if (flag_fatal_errors)
	exit (1);
    }

  return rc;
}


/* Save the existing error state.  */

void
gfc_push_error (gfc_error_buf * err)
{
  err->flag = error_buffer.flag;
  if (error_buffer.flag)
    err->message = xstrdup (error_buffer.message);

  error_buffer.flag = 0;
}


/* Restore a previous pushed error state.  */

void
gfc_pop_error (gfc_error_buf * err)
{
  error_buffer.flag = err->flag;
  if (error_buffer.flag)
    {
      size_t len = strlen (err->message) + 1;
      gcc_assert (len <= error_buffer.allocated);
      memcpy (error_buffer.message, err->message, len);
      gfc_free (err->message);
    }
}


/* Free a pushed error state, but keep the current error state.  */

void
gfc_free_error (gfc_error_buf * err)
{
  if (err->flag)
    gfc_free (err->message);
}


/* Debug wrapper for printf.  */

void
gfc_status (const char *cmsgid, ...)
{
  va_list argp;

  va_start (argp, cmsgid);

  vprintf (_(cmsgid), argp);

  va_end (argp);
}


/* Subroutine for outputting a single char so that we don't have to go
   around creating a lot of 1-character strings.  */

void
gfc_status_char (char c)
{
  putchar (c);
}


/* Report the number of warnings and errors that occurred to the caller.  */

void
gfc_get_errors (int *w, int *e)
{
  if (w != NULL)
    *w = warnings;
  if (e != NULL)
    *e = errors;
}
