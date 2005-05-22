/* Handle errors.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation,
   Inc.
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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

static int terminal_width, buffer_flag, errors,
  use_warning_buffer, warnings;

static char *error_ptr, *warning_ptr;

static gfc_error_buf error_buffer, warning_buffer;


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
      if (use_warning_buffer)
	{
	  *warning_ptr++ = c;
	  if (warning_ptr - warning_buffer.message >= MAX_ERROR_MESSAGE)
	    gfc_internal_error ("error_char(): Warning buffer overflow");
	}
      else
	{
	  *error_ptr++ = c;
	  if (error_ptr - error_buffer.message >= MAX_ERROR_MESSAGE)
	    gfc_internal_error ("error_char(): Error buffer overflow");
	}
    }
  else
    {
      if (c != 0)
	{
	  /* We build up complete lines before handing things
	     over to the library in order to speed up error printing.  */
	  static char line[MAX_ERROR_MESSAGE + 1];
	  static int index = 0;

	  line[index++] = c;
	  if (c == '\n' || index == MAX_ERROR_MESSAGE)
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


/* Show the file, where it was included and the source line, give a
   locus.  Calls error_printf() recursively, but the recursion is at
   most one level deep.  */

static void error_printf (const char *, ...) ATTRIBUTE_PRINTF_1;

static void
show_locus (int offset, locus * loc)
{
  gfc_linebuf *lb;
  gfc_file *f;
  char c, *p;
  int i, m;

  /* TODO: Either limit the total length and number of included files
     displayed or add buffering of arbitrary number of characters in
     error messages.  */

  lb = loc->lb;
  f = lb->file;
  error_printf ("In file %s:%d\n", f->filename,
#ifdef USE_MAPPED_LOCATION
		LOCATION_LINE (lb->location)
#else
		lb->linenum
#endif
		);

  for (;;)
    {
      i = f->inclusion_line;

      f = f->included_by;
      if (f == NULL) break;

      error_printf ("    Included at %s:%d\n", f->filename, i);
    }

  /* Show the line itself, taking care not to print more than what can
     show up on the terminal.  Tabs are converted to spaces.  */

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
}


/* As part of printing an error, we show the source lines that caused
   the problem.  We show at least one, possibly two loci.  If we're
   showing two loci and they both refer to the same file and line, we
   only print the line once.  */

static void
show_loci (locus * l1, locus * l2)
{
  int offset, flag, i, m, c1, c2, cmax;

  if (l1 == NULL)
    {
      error_printf ("<During initialization>\n");
      return;
    }

  c1 = l1->nextc - l1->lb->line;
  c2 = 0;
  if (l2 == NULL)
    goto separate;

  c2 = l2->nextc - l2->lb->line;

  if (c1 < c2)
    m = c2 - c1;
  else
    m = c1 - c2;


  if (l1->lb != l2->lb || m > terminal_width - 10)
    goto separate;

  offset = 0;
  cmax = (c1 < c2) ? c2 : c1;
  if (cmax > terminal_width - 5)
    offset = cmax - terminal_width + 5;

  if (offset < 0)
    offset = 0;

  c1 -= offset;
  c2 -= offset;

  show_locus (offset, l1);

  /* Arrange that '1' and '2' will show up even if the two columns are equal.  */
  for (i = 1; i <= cmax; i++)
    {
      flag = 0;
      if (i == c1)
	{
	  error_char ('1');
	  flag = 1;
	}
      if (i == c2)
	{
	  error_char ('2');
	  flag = 1;
	}
      if (flag == 0)
	error_char (' ');
    }

  error_char ('\n');

  return;

separate:
  offset = 0;

  if (c1 > terminal_width - 5)
    {
      offset = c1 - 5;
      if (offset < 0)
	offset = 0;
      c1 = c1 - offset;
    }

  show_locus (offset, l1);
  for (i = 1; i < c1; i++)
    error_char (' ');

  error_char ('1');
  error_char ('\n');

  if (l2 != NULL)
    {
      offset = 0;

      if (c2 > terminal_width - 20)
	{
	  offset = c2 - 20;
	  if (offset < 0)
	    offset = 0;
	  c2 = c2 - offset;
	}

      show_locus (offset, l2);

      for (i = 1; i < c2; i++)
	error_char (' ');

      error_char ('2');
      error_char ('\n');
    }
}


/* Workhorse for the error printing subroutines.  This subroutine is
   inspired by g77's error handling and is similar to printf() with
   the following %-codes:

   %c Character, %d Integer, %s String, %% Percent
   %L  Takes locus argument
   %C  Current locus (no argument)

   If a locus pointer is given, the actual source line is printed out
   and the column is indicated.  Since we want the error message at
   the bottom of any source file information, we must scan the
   argument list twice.  A maximum of two locus arguments are
   permitted.  */

#define IBUF_LEN 30
#define MAX_ARGS 10

static void
error_print (const char *type, const char *format0, va_list argp)
{
  char c, *p, int_buf[IBUF_LEN], c_arg[MAX_ARGS], *cp_arg[MAX_ARGS];
  int i, n, have_l1, i_arg[MAX_ARGS];
  locus *l1, *l2, *loc;
  const char *format;

  l1 = l2 = loc = NULL;

  have_l1 = 0;

  n = 0;
  format = format0;

  while (*format)
    {
      c = *format++;
      if (c == '%')
	{
	  c = *format++;

	  switch (c)
	    {
	    case '%':
	      break;

	    case 'L':
	      loc = va_arg (argp, locus *);
	      /* Fall through */

	    case 'C':
	      if (c == 'C')
		loc = &gfc_current_locus;

	      if (have_l1)
		{
		  l2 = loc;
		}
	      else
		{
		  l1 = loc;
		  have_l1 = 1;
		}
	      break;

	    case 'd':
	    case 'i':
	      i_arg[n++] = va_arg (argp, int);
	      break;

	    case 'c':
	      c_arg[n++] = va_arg (argp, int);
	      break;

	    case 's':
	      cp_arg[n++] = va_arg (argp, char *);
	      break;
	    }
	}
    }

  /* Show the current loci if we have to.  */
  if (have_l1)
    show_loci (l1, l2);
  error_string (type);
  error_char (' ');

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
      switch (*format)
	{
	case '%':
	  error_char ('%');
	  break;

	case 'c':
	  error_char (c_arg[n++]);
	  break;

	case 's':
	  error_string (cp_arg[n++]);
	  break;

	case 'i':
	case 'd':
	  i = i_arg[n++];

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
	  break;

	case 'C':		/* Current locus */
	case 'L':		/* Specified locus */
	  error_string (have_l1 ? "(2)" : "(1)");
	  have_l1 = 1;
	  break;
	}
    }

  error_char ('\n');
}


/* Wrapper for error_print().  */

static void
error_printf (const char *format, ...)
{
  va_list argp;

  va_start (argp, format);
  error_print ("", format, argp);
  va_end (argp);
}


/* Issue a warning.  */

void
gfc_warning (const char *format, ...)
{
  va_list argp;

  if (inhibit_warnings)
    return;

  warning_buffer.flag = 1;
  warning_ptr = warning_buffer.message;
  use_warning_buffer = 1;

  va_start (argp, format);
  if (buffer_flag == 0)
    warnings++;
  error_print ("Warning:", format, argp);
  va_end (argp);

  error_char ('\0');
}


/* Possibly issue a warning/error about use of a nonstandard (or deleted)
   feature.  An error/warning will be issued if the currently selected
   standard does not contain the requested bits.  Return FAILURE if
   an error is generated.  */

try
gfc_notify_std (int std, const char *format, ...)
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
  
  if (warning)
    {
      warning_buffer.flag = 1;
      warning_ptr = warning_buffer.message;
      use_warning_buffer = 1;
    }
  else
    {
      error_buffer.flag = 1;
      error_ptr = error_buffer.message;
      use_warning_buffer = 0;
    }

  if (buffer_flag == 0)
    {
      if (warning)
	warnings++;
      else
	errors++;
    }
  va_start (argp, format);
  if (warning)
    error_print ("Warning:", format, argp);
  else
    error_print ("Error:", format, argp);
  va_end (argp);

  error_char ('\0');
  return warning ? SUCCESS : FAILURE;
}


/* Immediate warning (i.e. do not buffer the warning).  */

void
gfc_warning_now (const char *format, ...)
{
  va_list argp;
  int i;

  if (inhibit_warnings)
    return;

  i = buffer_flag;
  buffer_flag = 0;
  warnings++;

  va_start (argp, format);
  error_print ("Warning:", format, argp);
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
      fputs (warning_buffer.message, stderr);
      warning_buffer.flag = 0;
    }
}


/* Issue an error.  */

void
gfc_error (const char *format, ...)
{
  va_list argp;

  if (gfc_suppress_error)
    return;

  error_buffer.flag = 1;
  error_ptr = error_buffer.message;
  use_warning_buffer = 0;

  va_start (argp, format);
  if (buffer_flag == 0)
    errors++;
  error_print ("Error:", format, argp);
  va_end (argp);

  error_char ('\0');
}


/* Immediate error.  */

void
gfc_error_now (const char *format, ...)
{
  va_list argp;
  int i;

  error_buffer.flag = 1;
  error_ptr = error_buffer.message;

  i = buffer_flag;
  buffer_flag = 0;
  errors++;

  va_start (argp, format);
  error_print ("Error:", format, argp);
  va_end (argp);

  error_char ('\0');
  buffer_flag = i;
}


/* Fatal error, never returns.  */

void
gfc_fatal_error (const char *format, ...)
{
  va_list argp;

  buffer_flag = 0;

  va_start (argp, format);
  error_print ("Fatal Error:", format, argp);
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

  exit (4);
}


/* Clear the error flag when we start to compile a source line.  */

void
gfc_clear_error (void)
{
  error_buffer.flag = 0;
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
      errors++;
      fputs (error_buffer.message, stderr);
      error_buffer.flag = 0;
    }

  return rc;
}


/* Save the existing error state.  */

void
gfc_push_error (gfc_error_buf * err)
{
  err->flag = error_buffer.flag;
  if (error_buffer.flag)
    strcpy (err->message, error_buffer.message);

  error_buffer.flag = 0;
}


/* Restore a previous pushed error state.  */

void
gfc_pop_error (gfc_error_buf * err)
{
  error_buffer.flag = err->flag;
  if (error_buffer.flag)
    strcpy (error_buffer.message, err->message);
}


/* Debug wrapper for printf.  */

void
gfc_status (const char *format, ...)
{
  va_list argp;

  va_start (argp, format);

  vprintf (format, argp);

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
