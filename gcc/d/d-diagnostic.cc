/* d-diagnostics.cc -- D frontend interface to gcc diagnostics.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/globals.h"
#include "dmd/errors.h"

#include "tree.h"
#include "options.h"
#include "diagnostic.h"

#include "d-tree.h"


/* Rewrite the format string FORMAT to deal with any format extensions not
   supported by pp_format().

   The following format specifiers are handled:
   `...`: text within backticks gets quoted as '%<...%>'.
   %-10s: left-justify format flag is removed leaving '%s' remaining.
   %02x: zero-padding format flag is removed leaving '%x' remaining.
   %X: uppercase unsigned hexadecimals are rewritten as '%x'.  */

static char *
expand_d_format (const char *format)
{
  obstack buf;
  bool inbacktick = false;

  gcc_obstack_init (&buf);

  for (const char *p = format; *p;)
    {
      while (*p != '\0' && *p != '\\' && *p != '%' && *p != '`')
	{
	  obstack_1grow (&buf, *p);
	  p++;
	}

      if (*p == '\0')
	break;

      if (*p == '\\')
	{
	  if (p[1] == '`')
	    {
	      /* Escaped backtick, don't expand it as a quoted string.  */
	      obstack_1grow (&buf, '`');
	      p++;;
	    }
	  else
	    obstack_1grow (&buf, *p);

	  p++;
	  continue;
	}

      if (*p == '`')
	{
	  /* Text enclosed by `...` are translated as a quoted string.  */
	  if (inbacktick)
	    {
	      obstack_grow (&buf, "%>", 2);
	      inbacktick = false;
	    }
	  else
	    {
	      obstack_grow (&buf, "%<", 2);
	      inbacktick = true;
	    }
	  p++;
	  continue;
	}

      /* Check the conversion specification for unhandled flags.  */
      obstack_1grow (&buf, *p);
      p++;

    Lagain:
      switch (*p)
	{
	case '\0':
	  /* Malformed format string.  */
	  gcc_unreachable ();

	case '-':
	  /* Remove whitespace formatting.  */
	  p++;
	  while (ISDIGIT (*p))
	    p++;
	  goto Lagain;

	case '0':
	  /* Remove zero padding from format string.  */
	  while (ISDIGIT (*p))
	    p++;
	  goto Lagain;

	case 'X':
	  /* Hex format only supports lower-case.  */
	  obstack_1grow (&buf, 'x');
	  p++;
	  break;

	default:
	  break;
	}
    }

  gcc_assert (!inbacktick);
  obstack_1grow (&buf, '\0');
  return (char *) obstack_finish (&buf);
}

/* Rewrite the format string FORMAT to deal with any characters that require
   escaping before expand_d_format expands it.  */

static char *
escape_d_format (const char *format)
{
  bool quoted = false;
  size_t format_len = 0;
  obstack buf;

  gcc_obstack_init (&buf);

  /* If the format string is enclosed by two '`' characters, then don't escape
     the first and last characters.  */
  if (*format == '`')
    {
      format_len = strlen (format) - 1;
      if (format_len && format[format_len] == '`')
	quoted = true;
    }

  for (const char *p = format; *p; p++)
    {
      switch (*p)
	{
	case '%':
	  /* Escape `%' characters so that pp_format does not confuse them
	     for actual format specifiers.  */
	  obstack_1grow (&buf, '%');
	  break;

	case '`':
	  /* Escape '`' characters so that expand_d_format does not confuse them
	     for a quoted string.  */
	  if (!quoted || (p != format && p != (format + format_len)))
	    obstack_1grow (&buf, '\\');
	  break;

	default:
	  break;
	}

      obstack_1grow (&buf, *p);
    }

  obstack_1grow (&buf, '\0');
  return (char *) obstack_finish (&buf);
}

/* Helper routine for all error routines.  Reports a diagnostic specified by
   KIND at the explicit location LOC.  The message FORMAT comes from the dmd
   front-end, which does not get translated by the gcc diagnostic routines.  */

static void ATTRIBUTE_GCC_DIAG(3,0)
d_diagnostic_report_diagnostic (const Loc &loc, int opt, const char *format,
				va_list ap, diagnostic_t kind, bool verbatim)
{
  va_list argp;
  va_copy (argp, ap);

  if (loc.filename || !verbatim)
    {
      rich_location rich_loc (line_table, make_location_t (loc));
      diagnostic_info diagnostic;
      char *xformat = expand_d_format (format);

      diagnostic_set_info_translated (&diagnostic, xformat, &argp,
				      &rich_loc, kind);
      if (opt != 0)
	diagnostic.option_index = opt;

      diagnostic_report_diagnostic (global_dc, &diagnostic);
    }
  else
    {
      /* Write verbatim messages with no location direct to stream.  */
      text_info text;
      text.err_no = errno;
      text.args_ptr = &argp;
      text.format_spec = expand_d_format (format);
      text.x_data = NULL;

      pp_format_verbatim (global_dc->printer, &text);
      pp_newline_and_flush (global_dc->printer);
    }

  va_end (argp);
}

/* Print a hard error message with explicit location LOC with an optional
   message prefix PREFIX1 and PREFIX2, increasing the global or gagged
   error count.  */

void ATTRIBUTE_GCC_DIAG(2,0)
verror (const Loc &loc, const char *format, va_list ap,
	const char *prefix1, const char *prefix2, const char *)
{
  if (!global.gag || global.params.showGaggedErrors)
    {
      char *xformat;

      /* Build string and emit.  */
      if (prefix2 != NULL)
	xformat = xasprintf ("%s %s %s", escape_d_format (prefix1),
			     escape_d_format (prefix2), format);
      else if (prefix1 != NULL)
	xformat = xasprintf ("%s %s", escape_d_format (prefix1), format);
      else
	xformat = xasprintf ("%s", format);

      d_diagnostic_report_diagnostic (loc, 0, xformat, ap,
				      global.gag ? DK_ANACHRONISM : DK_ERROR,
				      false);
      free (xformat);
    }

  if (global.gag)
    global.gaggedErrors++;

  global.errors++;
}

/* Print supplementary message about the last error with explicit location LOC.
   This doesn't increase the global error count.  */

void ATTRIBUTE_GCC_DIAG(2,0)
verrorSupplemental (const Loc &loc, const char *format, va_list ap)
{
  if (global.gag && !global.params.showGaggedErrors)
    return;

  d_diagnostic_report_diagnostic (loc, 0, format, ap, DK_NOTE, false);
}

/* Print a warning message with explicit location LOC, increasing the
   global warning count.  */

void ATTRIBUTE_GCC_DIAG(2,0)
vwarning (const Loc &loc, const char *format, va_list ap)
{
  if (!global.gag && global.params.warnings != DIAGNOSTICoff)
    {
      /* Warnings don't count if not treated as errors.  */
      if (global.params.warnings == DIAGNOSTICerror)
	global.warnings++;

      d_diagnostic_report_diagnostic (loc, 0, format, ap, DK_WARNING, false);
    }
  else if (global.gag)
    global.gaggedWarnings++;
}

/* Print supplementary message about the last warning with explicit location
   LOC.  This doesn't increase the global warning count.  */

void ATTRIBUTE_GCC_DIAG(2,0)
vwarningSupplemental (const Loc &loc, const char *format, va_list ap)
{
  if (global.params.warnings == DIAGNOSTICoff || global.gag)
    return;

  d_diagnostic_report_diagnostic (loc, 0, format, ap, DK_NOTE, false);
}

/* Print a deprecation message with explicit location LOC with an optional
   message prefix PREFIX1 and PREFIX2, increasing the global warning or
   error count depending on how deprecations are treated.  */

void ATTRIBUTE_GCC_DIAG(2,0)
vdeprecation (const Loc &loc, const char *format, va_list ap,
	      const char *prefix1, const char *prefix2)
{
  if (global.params.useDeprecated == DIAGNOSTICerror)
    verror (loc, format, ap, prefix1, prefix2);
  else if (global.params.useDeprecated == DIAGNOSTICinform && !global.gag)
    {
      char *xformat;

      /* Build string and emit.  */
      if (prefix2 != NULL)
	xformat = xasprintf ("%s %s %s", escape_d_format (prefix1),
			     escape_d_format (prefix2), format);
      else if (prefix1 != NULL)
	xformat = xasprintf ("%s %s", escape_d_format (prefix1), format);
      else
	xformat = xasprintf ("%s", format);

      d_diagnostic_report_diagnostic (loc, OPT_Wdeprecated, xformat, ap,
				      DK_WARNING, false);
      free (xformat);
    }
  else if (global.gag)
    global.gaggedWarnings++;
}

/* Print supplementary message about the last deprecation with explicit
   location LOC.  This does not increase the global error count.  */

void ATTRIBUTE_GCC_DIAG(2,0)
vdeprecationSupplemental (const Loc &loc, const char *format, va_list ap)
{
  if (global.params.useDeprecated == DIAGNOSTICerror)
    verrorSupplemental (loc, format, ap);
  else if (global.params.useDeprecated == DIAGNOSTICinform && !global.gag)
    d_diagnostic_report_diagnostic (loc, 0, format, ap, DK_NOTE, false);
}

/* Print a verbose message with explicit location LOC.  */

void ATTRIBUTE_GCC_DIAG(2,0)
vmessage (const Loc &loc, const char *format, va_list ap)
{
  d_diagnostic_report_diagnostic (loc, 0, format, ap, DK_NOTE, true);
}

/* Print a tip message with prefix and highlighing.  */

void ATTRIBUTE_GCC_DIAG(1,0)
vtip (const char *format, va_list ap)
{
  if (!global.gag)
    d_diagnostic_report_diagnostic (Loc (), 0, format, ap, DK_DEBUG, true);
}

/* Call this after printing out fatal error messages to clean up and
   exit the compiler.  */

void
fatal (void)
{
  exit (FATAL_EXIT_CODE);
}
