/* Error and warning routines.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted and expanded for GCC by Jose E. Marchesi.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"

#include "a68.h"

/*
 * Error handling routines.
 */

#define TABULATE(n) (8 * (n / 8 + 1) - n)

/* Severities handled by the DIAGNOSTIC function defined below.  */

#define A68_ERROR 0
#define A68_WARNING 1
#define A68_FATAL 2
#define A68_SCAN_ERROR 3
#define A68_INFORM 4

/* Auxiliary function used to grow an obstack by the contents of some given
   string.  */

static void
obstack_append_str (obstack *b, const char *str)
{
  obstack_grow (b, str, strlen (str));
}

/* Give a diagnostic message.  */

#if __GNUC__ >= 10
#pragma GCC diagnostic ignored "-Wsuggest-attribute=format"
#endif

static bool
diagnostic (int sev, int opt,
	    NODE_T *p,
	    LINE_T *line,
	    char *pos,
	    const char *loc_str, va_list args)
{
  int res = 0;
  MOID_T *moid = NO_MOID;
  const char *t = loc_str;
  obstack b;

  /*
   * Synthesize diagnostic message.
   *
   * Legend for special symbols:
   * * as first character, copy rest of string literally
   * @ AST node
   * A AST node attribute
   * B keyword
   * C context
   * L line number
   * M moid - if error mode return without giving a message
   * O moid - operand
   * S quoted symbol, when possible with typographical display features
   * X expected attribute
   * Y string literal.
   * Z quoted string.  */

  static va_list argp; /* Note this is empty. */
  gcc_obstack_init (&b);

  if (t[0] == '*')
    obstack_append_str (&b, t + 1);
  else
    while (t[0] != '\0')
      {
	if (t[0] == '@')
	  {
            const char *nt = a68_attribute_name (ATTRIBUTE (p));
            if (t != NO_TEXT)
              obstack_append_str (&b, nt);
	    else
              obstack_append_str (&b, "construct");
          }
	else if (t[0] == 'A')
	  {
            enum a68_attribute att = (enum a68_attribute) va_arg (args, int);
            const char *nt = a68_attribute_name (att);
            if (nt != NO_TEXT)
              obstack_append_str (&b, nt);
	    else
              obstack_append_str (&b, "construct");
          }
	else if (t[0] == 'B')
	  {
            enum a68_attribute att = (enum a68_attribute) va_arg (args, int);
            KEYWORD_T *nt = a68_find_keyword_from_attribute (A68 (top_keyword), att);
            if (nt != NO_KEYWORD)
	      {
		const char *strop_keyword = a68_strop_keyword (TEXT (nt));

		obstack_append_str (&b, "%<");
		obstack_append_str (&b, strop_keyword);
		obstack_append_str (&b, "%>");
	      }
	    else
              obstack_append_str (&b, "keyword");
          }
	else if (t[0] == 'C')
	  {
            int att = va_arg (args, int);
	    const char *sort = NULL;

	    switch (att)
	      {
	      case NO_SORT: sort = "this"; break;
	      case SOFT: sort = "a soft"; break;
	      case WEAK: sort = "a weak"; break;
	      case MEEK: sort = "a meek"; break;
	      case FIRM: sort = "a firm"; break;
	      case STRONG: sort = "a strong"; break;
	      default:
		gcc_unreachable ();
	      }

	    obstack_append_str (&b, sort);
          }
	else if (t[0] == 'L')
	  {
	    LINE_T *a = va_arg (args, LINE_T *);
            gcc_assert (a != NO_LINE);
            if (NUMBER (a) == 0)
              obstack_append_str (&b, "in standard environment");
	    else if (p != NO_NODE && NUMBER (a) == LINE_NUMBER (p))
	      obstack_append_str (&b, "in this line");
	    else
	      {
		char d[18];
		if (snprintf (d, 18, "in line %d", NUMBER (a)) < 0)
		  gcc_unreachable ();
		obstack_append_str (&b, d);
	      }
          }
	else if (t[0] == 'M')
	  {
	    const char *moidstr = NULL;

            moid = va_arg (args, MOID_T *);
            if (moid == NO_MOID || moid == M_ERROR)
              moid = M_UNDEFINED;

            if (IS (moid, SERIES_MODE))
	      {
		if (PACK (moid) != NO_PACK && NEXT (PACK (moid)) == NO_PACK)
		  moidstr = a68_moid_to_string (MOID (PACK (moid)),
						MOID_ERROR_WIDTH, p);
		else
		  moidstr = a68_moid_to_string (moid, MOID_ERROR_WIDTH, p);
	      }
	    else
	      moidstr = a68_moid_to_string (moid, MOID_ERROR_WIDTH, p);

	    obstack_append_str (&b, "%<");
	    obstack_append_str (&b, moidstr);
	    obstack_append_str (&b, "%>");
          }
	else if (t[0] == 'O')
	  {
            moid = va_arg (args, MOID_T *);
            if (moid == NO_MOID || moid == M_ERROR)
              moid = M_UNDEFINED;
            if (moid == M_VOID)
              obstack_append_str (&b, "UNION (VOID, ..)");
	    else if (IS (moid, SERIES_MODE))
	      {
		const char *moidstr = NULL;

		if (PACK (moid) != NO_PACK && NEXT (PACK (moid)) == NO_PACK)
		  moidstr = a68_moid_to_string (MOID (PACK (moid)), MOID_ERROR_WIDTH, p);
		else
		  moidstr = a68_moid_to_string (moid, MOID_ERROR_WIDTH, p);
		obstack_append_str (&b, moidstr);
	      }
	    else
	      {
		const char *moidstr = a68_moid_to_string (moid, MOID_ERROR_WIDTH, p);
		obstack_append_str (&b, moidstr);
	      }
          }
	else if (t[0] == 'S')
	  {
            if (p != NO_NODE && NSYMBOL (p) != NO_TEXT)
	      {
		const char *txt = NSYMBOL (p);
		char *sym = NCHAR_IN_LINE (p);
		int n = 0, size = (int) strlen (txt);

		obstack_append_str (&b, "%<");
		if (txt[0] != sym[0] || (int) strlen (sym) < size)
		  obstack_append_str (&b, txt);
		else
		  {
		    while (n < size)
		      {
			if (ISPRINT (sym[0]))
			  obstack_1grow (&b, sym[0]);
			if (TOLOWER (txt[0]) == TOLOWER (sym[0]))
			  {
			    txt++;
			    n++;
			  }
			sym++;
		      }
		  }
		obstack_append_str (&b, "%>");
	      }
	    else
	      obstack_append_str (&b, "symbol");
          }
	else if (t[0] == 'X')
	  {
            enum a68_attribute att = (enum a68_attribute) (va_arg (args, int));
	    const char *att_name = a68_attribute_name (att);
	    obstack_append_str (&b, att_name);
          }
	else if (t[0] == 'Y')
	  {
            char *loc_string = va_arg (args, char *);
	    obstack_append_str (&b, loc_string);
          }
	else if (t[0] == 'Z')
	  {
            char *str = va_arg (args, char *);
	    obstack_append_str (&b, "%<");
	    obstack_append_str (&b, str);
	    obstack_append_str (&b, "%>");
          }
	else
	  obstack_1grow (&b, t[0]);

	t++;
       }

  obstack_1grow (&b, '\0');
  char *format = (char *) obstack_finish (&b);

  /* Construct a diagnostic message.  */
  if (sev == A68_WARNING)
    WARNING_COUNT (&A68_JOB)++;
  else if (sev != A68_INFORM)
    ERROR_COUNT (&A68_JOB)++;

  /* Emit the corresponding GCC diagnostic at the proper location.  */
  location_t loc = UNKNOWN_LOCATION;

  if (p != NO_NODE)
    loc = a68_get_node_location (p);
  else if (line != NO_LINE)
    {
      if (pos == NO_TEXT)
	pos = STRING (line);
      loc = a68_get_line_location (line, pos);
    }

  /* Prepare rich location and diagnostics.  */
  rich_location rich_loc (line_table, loc);
  diagnostics::diagnostic_info diagnostic;
  enum diagnostics::kind kind;

  switch (sev)
    {
    case A68_FATAL:
      kind = diagnostics::kind::fatal;
      break;
    case A68_INFORM:
      kind = diagnostics::kind::note;
      break;
    case A68_WARNING:
      kind = diagnostics::kind::warning;
      break;
    case A68_SCAN_ERROR:
    case A68_ERROR:
      kind = diagnostics::kind::error;
      break;
    default:
      gcc_unreachable ();
    }

  diagnostic_set_info (&diagnostic, format,
		       &argp,
		       &rich_loc, kind);
  if (opt != 0)
    diagnostic.m_option_id = opt;
  res = diagnostic_report_diagnostic (global_dc, &diagnostic);

  if (sev == A68_SCAN_ERROR)
    exit (FATAL_EXIT_CODE);
  return res;
}

/* Give an intelligible error and exit.  A line is provided rather than a
   node so this can be used at scanning time.  */

void
a68_scan_error (LINE_T * u, char *v, const char *txt, ...)
{
  va_list args;

  va_start (args, txt);
  diagnostic (A68_SCAN_ERROR, 0, NO_NODE, u, v, txt, args);
  va_end (args);
}

/* Report a compilation error.  */

void
a68_error (NODE_T *p, const char *loc_str, ...)
{
  va_list args;

  va_start (args, loc_str);
  diagnostic (A68_ERROR, 0, p, NO_LINE, NO_TEXT, loc_str, args);
  va_end (args);
}

/* Report a compilation error in a node's pragmat.  */

void
a68_error_in_pragmat (NODE_T *p, size_t off,
		      const char *loc_str, ...)
{
  va_list args;

  LINE_T *line = NPRAGMAT_LINE (p);
  char *pos = NPRAGMAT_CHAR_IN_LINE (p) + off;

  va_start (args, loc_str);
  diagnostic (A68_ERROR, 0, NO_NODE, line, pos, loc_str, args);
  va_end (args);
  a68_inform (p, "pragmat applies to this construct");
}

/* Report a compilation warning.
   This function returns a boolean indicating whether a warning was
   emitted.  */

bool
a68_warning (NODE_T *p, int opt,
	     const char *loc_str, ...)
{
  bool res;
  va_list args;

  va_start (args, loc_str);
  res = diagnostic (A68_WARNING, opt, p, NO_LINE, NO_TEXT, loc_str, args);
  va_end (args);
  return res;
}

/* Report a compilation note.  */

void
a68_inform (NODE_T *p, const char *loc_str, ...)
{
  va_list args;

  va_start (args, loc_str);
  diagnostic (A68_INFORM, 0, p, NO_LINE, NO_TEXT, loc_str, args);
  va_end (args);
}
