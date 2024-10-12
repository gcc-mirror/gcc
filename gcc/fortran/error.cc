/* Handle errors.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Niels Kristian Bech Jensen

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

/* Handle the inevitable errors.  A major catch here is that things
   flagged as errors in one match subroutine can conceivably be legal
   elsewhere.  This means that error messages are recorded and saved
   for possible use later.  If a line does not match a legal
   construction, then the saved error message is reported.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"

#include "diagnostic.h"
#include "diagnostic-color.h"
#include "tree-diagnostic.h" /* tree_diagnostics_defaults */
#include "diagnostic-format-text.h"

static int suppress_errors = 0;

static bool warnings_not_errors = false;

static int terminal_width;

/* True if the error/warnings should be buffered.  */
static bool buffered_p;

static gfc_error_buffer error_buffer;
/* These are always buffered buffers (.flush_p == false) to be used by
   the pretty-printer.  */
static output_buffer *pp_error_buffer, *pp_warning_buffer;
static int warningcount_buffered, werrorcount_buffered;


/* Return a location_t suitable for 'tree' for a gfortran locus.  During
   parsing in gfortran, loc->lb->location contains only the line number
   and LOCATION_COLUMN is 0; hence, the column has to be added when generating
   locations for 'tree'.  */

location_t
gfc_get_location_with_offset (locus *loc, unsigned offset)
{
  gcc_checking_assert (loc->nextc >= loc->lb->line);
  return linemap_position_for_loc_and_offset (line_table, loc->lb->location,
					      loc->nextc - loc->lb->line
					      + offset);
}

/* Return buffered_p.  */
bool
gfc_buffered_p (void)
{
  return buffered_p;
}

/* Return true if there output_buffer is empty.  */

static bool
gfc_output_buffer_empty_p (const output_buffer * buf)
{
  return output_buffer_last_position_in_text (buf) == NULL;
}

/* Go one level deeper suppressing errors.  */

void
gfc_push_suppress_errors (void)
{
  gcc_assert (suppress_errors >= 0);
  ++suppress_errors;
}

static void
gfc_error_opt (int opt, const char *gmsgid, va_list ap)  ATTRIBUTE_GCC_GFC(2,0);

static bool
gfc_warning (int opt, const char *gmsgid, va_list ap) ATTRIBUTE_GCC_GFC(2,0);


/* Leave one level of error suppressing.  */

void
gfc_pop_suppress_errors (void)
{
  gcc_assert (suppress_errors > 0);
  --suppress_errors;
}


/* Query whether errors are suppressed.  */

bool
gfc_query_suppress_errors (void)
{
  return suppress_errors > 0;
}


/* Determine terminal width (for trimming source lines in output).  */

static int
gfc_get_terminal_width (void)
{
  return isatty (STDERR_FILENO) ? get_terminal_width () : INT_MAX;
}


/* Per-file error initialization.  */

void
gfc_error_init_1 (void)
{
  terminal_width = gfc_get_terminal_width ();
  gfc_buffer_error (false);
}


/* Set the flag for buffering errors or not.  */

void
gfc_buffer_error (bool flag)
{
  buffered_p = flag;
}


static int
print_wide_char_into_buffer (gfc_char_t c, char *buf)
{
  static const char xdigit[16] = { '0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

  if (gfc_wide_is_printable (c) || c == '\t')
    {
      buf[1] = '\0';
      /* Tabulation is output as a space.  */
      buf[0] = (unsigned char) (c == '\t' ? ' ' : c);
      return 1;
    }
  else if (c < ((gfc_char_t) 1 << 8))
    {
      buf[4] = '\0';
      buf[3] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[2] = xdigit[c & 0x0F];

      buf[1] = 'x';
      buf[0] = '\\';
      return 4;
    }
  else if (c < ((gfc_char_t) 1 << 16))
    {
      buf[6] = '\0';
      buf[5] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[4] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[3] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[2] = xdigit[c & 0x0F];

      buf[1] = 'u';
      buf[0] = '\\';
      return 6;
    }
  else
    {
      buf[10] = '\0';
      buf[9] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[8] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[7] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[6] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[5] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[4] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[3] = xdigit[c & 0x0F];
      c = c >> 4;
      buf[2] = xdigit[c & 0x0F];

      buf[1] = 'U';
      buf[0] = '\\';
      return 10;
    }
}

static char wide_char_print_buffer[11];

const char *
gfc_print_wide_char (gfc_char_t c)
{
  print_wide_char_into_buffer (c, wide_char_print_buffer);
  return wide_char_print_buffer;
}


/* Clear any output buffered in a pretty-print output_buffer.  */

static void
gfc_clear_pp_buffer (output_buffer *this_buffer)
{
  pretty_printer *pp = global_dc->m_printer;
  output_buffer *tmp_buffer = pp_buffer (pp);
  pp_buffer (pp) = this_buffer;
  pp_clear_output_area (pp);
  pp_buffer (pp) = tmp_buffer;
  /* We need to reset last_location, otherwise we may skip caret lines
     when we actually give a diagnostic.  */
  global_dc->m_last_location = UNKNOWN_LOCATION;
}

/* The currently-printing diagnostic, for use by gfc_format_decoder,
   for colorizing %C and %L.  */

static diagnostic_info *curr_diagnostic;

/* A helper function to call diagnostic_report_diagnostic, while setting
   curr_diagnostic for the duration of the call.  */

static bool
gfc_report_diagnostic (diagnostic_info *diagnostic)
{
  gcc_assert (diagnostic != NULL);
  curr_diagnostic = diagnostic;
  bool ret = diagnostic_report_diagnostic (global_dc, diagnostic);
  curr_diagnostic = NULL;
  return ret;
}

/* This is just a helper function to avoid duplicating the logic of
   gfc_warning.  */

static bool
gfc_warning (int opt, const char *gmsgid, va_list ap)
{
  va_list argp;
  va_copy (argp, ap);

  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);
  bool fatal_errors = global_dc->m_fatal_errors;
  pretty_printer *pp = global_dc->m_printer;
  output_buffer *tmp_buffer = pp_buffer (pp);

  gfc_clear_pp_buffer (pp_warning_buffer);

  if (buffered_p)
    {
      pp_buffer (pp) = pp_warning_buffer;
      global_dc->m_fatal_errors = false;
      /* To prevent -fmax-errors= triggering.  */
      --werrorcount;
    }

  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc,
		       DK_WARNING);
  diagnostic.option_id = opt;
  bool ret = gfc_report_diagnostic (&diagnostic);

  if (buffered_p)
    {
      pp_buffer (pp) = tmp_buffer;
      global_dc->m_fatal_errors = fatal_errors;

      warningcount_buffered = 0;
      werrorcount_buffered = 0;
      /* Undo the above --werrorcount if not Werror, otherwise
	 werrorcount is correct already.  */
      if (!ret)
	++werrorcount;
      else if (diagnostic.kind == DK_ERROR)
	++werrorcount_buffered;
      else
	++werrorcount, --warningcount, ++warningcount_buffered;
    }

  va_end (argp);
  return ret;
}

/* Issue a warning.  */

bool
gfc_warning (int opt, const char *gmsgid, ...)
{
  va_list argp;

  va_start (argp, gmsgid);
  bool ret = gfc_warning (opt, gmsgid, argp);
  va_end (argp);
  return ret;
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


/* Return a string describing the nature of a standard violation
 * and/or the relevant version of the standard.  */

char const*
notify_std_msg(int std)
{

  if (std & GFC_STD_F2023_DEL)
    return _("Prohibited in Fortran 2023:");
  else if (std & GFC_STD_F2023)
    return _("Fortran 2023:");
  else if (std & GFC_STD_F2018_DEL)
    return _("Fortran 2018 deleted feature:");
  else if (std & GFC_STD_F2018_OBS)
    return _("Fortran 2018 obsolescent feature:");
  else if (std & GFC_STD_F2018)
    return _("Fortran 2018:");
  else if (std & GFC_STD_F2008_OBS)
    return _("Fortran 2008 obsolescent feature:");
  else if (std & GFC_STD_F2008)
    return "Fortran 2008:";
  else if (std & GFC_STD_F2003)
    return "Fortran 2003:";
  else if (std & GFC_STD_GNU)
    return _("GNU Extension:");
  else if (std & GFC_STD_LEGACY)
    return _("Legacy Extension:");
  else if (std & GFC_STD_F95_OBS)
    return _("Obsolescent feature:");
  else if (std & GFC_STD_F95_DEL)
    return _("Deleted feature:");
  else
    gcc_unreachable ();
}


/* Possibly issue a warning/error about use of a nonstandard (or deleted)
   feature.  An error/warning will be issued if the currently selected
   standard does not contain the requested bits.  Return false if
   an error is generated.  */

bool
gfc_notify_std (int std, const char *gmsgid, ...)
{
  va_list argp;
  const char *msg, *msg2;
  char *buffer;

  /* Determine whether an error or a warning is needed.  */
  const int wstd = std & gfc_option.warn_std;    /* Standard to warn about.  */
  const int estd = std & ~gfc_option.allow_std;  /* Standard to error about.  */
  const bool warning = (wstd != 0) && !inhibit_warnings;
  const bool error = (estd != 0);

  if (!error && !warning)
    return true;
  if (suppress_errors)
    return !error;

  if (error)
    msg = notify_std_msg (estd);
  else
    msg = notify_std_msg (wstd);

  msg2 = _(gmsgid);
  buffer = (char *) alloca (strlen (msg) + strlen (msg2) + 2);
  strcpy (buffer, msg);
  strcat (buffer, " ");
  strcat (buffer, msg2);

  va_start (argp, gmsgid);
  if (error)
    gfc_error_opt (0, buffer, argp);
  else
    gfc_warning (0, buffer, argp);
  va_end (argp);

  if (error)
    return false;
  else
    return (warning && !warnings_are_errors);
}


/* Called from output_format -- during diagnostic message processing
   to handle Fortran specific format specifiers with the following meanings:

   %C  Current locus (no argument)
   %L  Takes locus argument
*/
static bool
gfc_format_decoder (pretty_printer *pp, text_info *text, const char *spec,
		    int precision, bool wide, bool set_locus, bool hash,
		    bool *quoted, pp_token_list &formatted_token_list)
{
  unsigned offset = 0;
  switch (*spec)
    {
    case 'C':
    case 'L':
      {
	static const char *result[2] = { "(1)", "(2)" };
	locus *loc;
	if (*spec == 'C')
	  {
	    loc = &gfc_current_locus;
	    /* Point %C first offending character not the last good one. */
	    if (*loc->nextc != '\0')
	      offset++;
	  }
	else
	  loc = va_arg (*text->m_args_ptr, locus *);

	/* If location[0] != UNKNOWN_LOCATION means that we already
	   processed one of %C/%L.  */
	int loc_num = text->get_location (0) == UNKNOWN_LOCATION ? 0 : 1;
	location_t src_loc = gfc_get_location_with_offset (loc, offset);
	text->set_location (loc_num, src_loc, SHOW_RANGE_WITH_CARET);
	/* Colorize the markers to match the color choices of
	   diagnostic_show_locus (the initial location has a color given
	   by the "kind" of the diagnostic, the secondary location has
	   color "range1").  */
	gcc_assert (curr_diagnostic != NULL);
	const char *color
	  = (loc_num
	     ? "range1"
	     : diagnostic_get_color_for_kind (curr_diagnostic->kind));
	pp_string (pp, colorize_start (pp_show_color (pp), color));
	pp_string (pp, result[loc_num]);
	pp_string (pp, colorize_stop (pp_show_color (pp)));
	return true;
      }
    default:
      /* Fall through info the middle-end decoder, as e.g. stor-layout.cc
	 etc. diagnostics can use the FE printer while the FE is still
	 active.  */
      return default_tree_printer (pp, text, spec, precision, wide,
				   set_locus, hash, quoted,
				   formatted_token_list);
    }
}

/* Return a malloc'd string describing the kind of diagnostic.  The
   caller is responsible for freeing the memory.  */
static char *
gfc_diagnostic_build_kind_prefix (diagnostic_context *context,
				  const diagnostic_info *diagnostic)
{
  static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "gfc-diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
    "must-not-happen"
  };
  static const char *const diagnostic_kind_color[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (C),
#include "gfc-diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
    NULL
  };
  gcc_assert (diagnostic->kind < DK_LAST_DIAGNOSTIC_KIND);
  const char *text = _(diagnostic_kind_text[diagnostic->kind]);
  const char *text_cs = "", *text_ce = "";
  pretty_printer *const pp = context->m_printer;

  if (diagnostic_kind_color[diagnostic->kind])
    {
      text_cs = colorize_start (pp_show_color (pp),
				diagnostic_kind_color[diagnostic->kind]);
      text_ce = colorize_stop (pp_show_color (pp));
    }
  return build_message_string ("%s%s:%s ", text_cs, text, text_ce);
}

/* Return a malloc'd string describing a location.  The caller is
   responsible for freeing the memory.  */
static char *
gfc_diagnostic_build_locus_prefix (const diagnostic_location_print_policy &loc_policy,
				   expanded_location s,
				   bool colorize)
{
  const char *locus_cs = colorize_start (colorize, "locus");
  const char *locus_ce = colorize_stop (colorize);
  return (s.file == NULL
	  ? build_message_string ("%s%s:%s", locus_cs, progname, locus_ce )
	  : !strcmp (s.file, special_fname_builtin ())
	  ? build_message_string ("%s%s:%s", locus_cs, s.file, locus_ce)
	  : loc_policy.show_column_p ()
	  ? build_message_string ("%s%s:%d:%d:%s", locus_cs, s.file, s.line,
				  s.column, locus_ce)
	  : build_message_string ("%s%s:%d:%s", locus_cs, s.file, s.line, locus_ce));
}

/* Return a malloc'd string describing two locations.  The caller is
   responsible for freeing the memory.  */
static char *
gfc_diagnostic_build_locus_prefix (const diagnostic_location_print_policy &loc_policy,
				   expanded_location s, expanded_location s2,
				   bool colorize)
{
  const char *locus_cs = colorize_start (colorize, "locus");
  const char *locus_ce = colorize_stop (colorize);

  return (s.file == NULL
	  ? build_message_string ("%s%s:%s", locus_cs, progname, locus_ce )
	  : !strcmp (s.file, special_fname_builtin ())
	  ? build_message_string ("%s%s:%s", locus_cs, s.file, locus_ce)
	  : loc_policy.show_column_p ()
	  ? build_message_string ("%s%s:%d:%d-%d:%s", locus_cs, s.file, s.line,
				  MIN (s.column, s2.column),
				  MAX (s.column, s2.column), locus_ce)
	  : build_message_string ("%s%s:%d:%s", locus_cs, s.file, s.line,
				  locus_ce));
}

/* This function prints the locus (file:line:column), the diagnostic kind
   (Error, Warning) and (optionally) the relevant lines of code with
   annotation lines with '1' and/or '2' below them.

   With -fdiagnostic-show-caret (the default) it prints:

       [locus of primary range]:

          some code
                 1
       Error: Some error at (1)

  With -fno-diagnostic-show-caret or if the primary range is not
  valid, it prints:

       [locus of primary range]: Error: Some error at (1) and (2)
*/
static void
gfc_diagnostic_text_starter (diagnostic_text_output_format &text_output,
			     const diagnostic_info *diagnostic)
{
  diagnostic_context *const context = &text_output.get_context ();
  pretty_printer *const pp = text_output.get_printer ();
  char * kind_prefix = gfc_diagnostic_build_kind_prefix (context, diagnostic);

  expanded_location s1 = diagnostic_expand_location (diagnostic);
  expanded_location s2;
  bool one_locus = diagnostic->richloc->get_num_locations () < 2;
  bool same_locus = false;

  if (!one_locus)
    {
      s2 = diagnostic_expand_location (diagnostic, 1);
      same_locus = diagnostic_same_line (context, s1, s2);
    }

  diagnostic_location_print_policy loc_policy (text_output);
  const bool colorize = pp_show_color (pp);
  char * locus_prefix = (one_locus || !same_locus)
    ? gfc_diagnostic_build_locus_prefix (loc_policy, s1, colorize)
    : gfc_diagnostic_build_locus_prefix (loc_policy, s1, s2, colorize);

  if (!context->m_source_printing.enabled
      || diagnostic_location (diagnostic, 0) <= BUILTINS_LOCATION
      || diagnostic_location (diagnostic, 0) == context->m_last_location)
    {
      pp_set_prefix (pp,
		     concat (locus_prefix, " ", kind_prefix, NULL));
      free (locus_prefix);

      if (one_locus || same_locus)
	{
	  free (kind_prefix);
	  return;
	}
      /* In this case, we print the previous locus and prefix as:

	  [locus]:[prefix]: (1)

	 and we flush with a new line before setting the new prefix.  */
      pp_string (pp, "(1)");
      pp_newline (pp);
      locus_prefix = gfc_diagnostic_build_locus_prefix (loc_policy, s2, colorize);
      pp_set_prefix (pp,
		     concat (locus_prefix, " ", kind_prefix, NULL));
      free (kind_prefix);
      free (locus_prefix);
    }
  else
    {
      pp_verbatim (pp, "%s", locus_prefix);
      free (locus_prefix);
      /* Fortran uses an empty line between locus and caret line.  */
      pp_newline (pp);
      pp_set_prefix (pp, NULL);
      pp_newline (pp);
      diagnostic_show_locus (context, diagnostic->richloc, diagnostic->kind,
			     pp);
      /* If the caret line was shown, the prefix does not contain the
	 locus.  */
      pp_set_prefix (pp, kind_prefix);
    }
}

static void
gfc_diagnostic_start_span (const diagnostic_location_print_policy &loc_policy,
			   pretty_printer *pp,
			   expanded_location exploc)
{
  const bool colorize = pp_show_color (pp);
  char *locus_prefix
    = gfc_diagnostic_build_locus_prefix (loc_policy, exploc, colorize);
  pp_verbatim (pp, "%s", locus_prefix);
  free (locus_prefix);
  pp_newline (pp);
  /* Fortran uses an empty line between locus and caret line.  */
  pp_newline (pp);
}


static void
gfc_diagnostic_text_finalizer (diagnostic_text_output_format &text_output,
			       const diagnostic_info *diagnostic ATTRIBUTE_UNUSED,
			       diagnostic_t orig_diag_kind ATTRIBUTE_UNUSED)
{
  pretty_printer *const pp = text_output.get_printer ();
  pp_destroy_prefix (pp);
  pp_newline_and_flush (pp);
}

/* Immediate warning (i.e. do not buffer the warning) with an explicit
   location.  */

bool
gfc_warning_now_at (location_t loc, int opt, const char *gmsgid, ...)
{
  va_list argp;
  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, loc);
  bool ret;

  va_start (argp, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc, DK_WARNING);
  diagnostic.option_id = opt;
  ret = gfc_report_diagnostic (&diagnostic);
  va_end (argp);
  return ret;
}

/* Immediate warning (i.e. do not buffer the warning).  */

bool
gfc_warning_now (int opt, const char *gmsgid, ...)
{
  va_list argp;
  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);
  bool ret;

  va_start (argp, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc,
		       DK_WARNING);
  diagnostic.option_id = opt;
  ret = gfc_report_diagnostic (&diagnostic);
  va_end (argp);
  return ret;
}

/* Internal warning, do not buffer.  */

bool
gfc_warning_internal (int opt, const char *gmsgid, ...)
{
  va_list argp;
  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);
  bool ret;

  va_start (argp, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc,
		       DK_WARNING);
  diagnostic.option_id = opt;
  ret = gfc_report_diagnostic (&diagnostic);
  va_end (argp);
  return ret;
}

/* Immediate error (i.e. do not buffer).  */

void
gfc_error_now (const char *gmsgid, ...)
{
  va_list argp;
  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  error_buffer.flag = true;

  va_start (argp, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc, DK_ERROR);
  gfc_report_diagnostic (&diagnostic);
  va_end (argp);
}


/* Fatal error, never returns.  */

void
gfc_fatal_error (const char *gmsgid, ...)
{
  va_list argp;
  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_start (argp, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc, DK_FATAL);
  gfc_report_diagnostic (&diagnostic);
  va_end (argp);

  gcc_unreachable ();
}

/* Clear the warning flag.  */

void
gfc_clear_warning (void)
{
  gfc_clear_pp_buffer (pp_warning_buffer);
  warningcount_buffered = 0;
  werrorcount_buffered = 0;
}


/* Check to see if any warnings have been saved.
   If so, print the warning.  */

void
gfc_warning_check (void)
{
  if (! gfc_output_buffer_empty_p (pp_warning_buffer))
    {
      pretty_printer *pp = global_dc->m_printer;
      output_buffer *tmp_buffer = pp_buffer (pp);
      pp_buffer (pp) = pp_warning_buffer;
      pp_really_flush (pp);
      warningcount += warningcount_buffered;
      werrorcount += werrorcount_buffered;
      gcc_assert (warningcount_buffered + werrorcount_buffered == 1);
      pp_buffer (pp) = tmp_buffer;
      diagnostic_action_after_output (global_dc,
				      warningcount_buffered
				      ? DK_WARNING : DK_ERROR);
      diagnostic_check_max_errors (global_dc, true);
    }
}


/* Issue an error.  */

static void
gfc_error_opt (int opt, const char *gmsgid, va_list ap)
{
  va_list argp;
  va_copy (argp, ap);
  bool saved_abort_on_error = false;

  if (warnings_not_errors)
    {
      gfc_warning (opt, gmsgid, argp);
      va_end (argp);
      return;
    }

  if (suppress_errors)
    {
      va_end (argp);
      return;
    }

  diagnostic_info diagnostic;
  rich_location richloc (line_table, UNKNOWN_LOCATION);
  bool fatal_errors = global_dc->m_fatal_errors;
  pretty_printer *pp = global_dc->m_printer;
  output_buffer *tmp_buffer = pp_buffer (pp);

  gfc_clear_pp_buffer (pp_error_buffer);

  if (buffered_p)
    {
      /* To prevent -dH from triggering an abort on a buffered error,
	 save abort_on_error and restore it below.  */
      saved_abort_on_error = global_dc->m_abort_on_error;
      global_dc->m_abort_on_error = false;
      pp_buffer (pp) = pp_error_buffer;
      global_dc->m_fatal_errors = false;
      /* To prevent -fmax-errors= triggering, we decrease it before
	 report_diagnostic increases it.  */
      --errorcount;
    }

  diagnostic_set_info (&diagnostic, gmsgid, &argp, &richloc, DK_ERROR);
  gfc_report_diagnostic (&diagnostic);

  if (buffered_p)
    {
      pp_buffer (pp) = tmp_buffer;
      global_dc->m_fatal_errors = fatal_errors;
      global_dc->m_abort_on_error = saved_abort_on_error;

    }

  va_end (argp);
}


void
gfc_error_opt (int opt, const char *gmsgid, ...)
{
  va_list argp;
  va_start (argp, gmsgid);
  gfc_error_opt (opt, gmsgid, argp);
  va_end (argp);
}


void
gfc_error (const char *gmsgid, ...)
{
  va_list argp;
  va_start (argp, gmsgid);
  gfc_error_opt (0, gmsgid, argp);
  va_end (argp);
}


/* This shouldn't happen... but sometimes does.  */

void
gfc_internal_error (const char *gmsgid, ...)
{
  int e, w;
  va_list argp;
  diagnostic_info diagnostic;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  gfc_get_errors (&w, &e);
  if (e > 0)
    exit(EXIT_FAILURE);

  va_start (argp, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &argp, &rich_loc, DK_ICE);
  gfc_report_diagnostic (&diagnostic);
  va_end (argp);

  gcc_unreachable ();
}


/* Clear the error flag when we start to compile a source line.  */

void
gfc_clear_error (void)
{
  error_buffer.flag = false;
  warnings_not_errors = false;
  gfc_clear_pp_buffer (pp_error_buffer);
}


/* Tests the state of error_flag.  */

bool
gfc_error_flag_test (void)
{
  return error_buffer.flag
    || !gfc_output_buffer_empty_p (pp_error_buffer);
}


/* Check to see if any errors have been saved.
   If so, print the error.  Returns the state of error_flag.  */

bool
gfc_error_check (void)
{
  if (error_buffer.flag
      || ! gfc_output_buffer_empty_p (pp_error_buffer))
    {
      error_buffer.flag = false;
      pretty_printer *pp = global_dc->m_printer;
      output_buffer *tmp_buffer = pp_buffer (pp);
      pp_buffer (pp) = pp_error_buffer;
      pp_really_flush (pp);
      ++errorcount;
      gcc_assert (gfc_output_buffer_empty_p (pp_error_buffer));
      pp_buffer (pp) = tmp_buffer;
      diagnostic_action_after_output (global_dc, DK_ERROR);
      diagnostic_check_max_errors (global_dc, true);
      return true;
    }

  return false;
}

/* Move the text buffered from FROM to TO, then clear
   FROM. Independently if there was text in FROM, TO is also
   cleared. */

static void
gfc_move_error_buffer_from_to (gfc_error_buffer * buffer_from,
			       gfc_error_buffer * buffer_to)
{
  output_buffer * from = &(buffer_from->buffer);
  output_buffer * to =  &(buffer_to->buffer);

  buffer_to->flag = buffer_from->flag;
  buffer_from->flag = false;

  gfc_clear_pp_buffer (to);
  /* We make sure this is always buffered.  */
  to->m_flush_p = false;

  if (! gfc_output_buffer_empty_p (from))
    {
      const char *str = output_buffer_formatted_text (from);
      output_buffer_append_r (to, str, strlen (str));
      gfc_clear_pp_buffer (from);
    }
}

/* Save the existing error state.  */

void
gfc_push_error (gfc_error_buffer *err)
{
  gfc_move_error_buffer_from_to (&error_buffer, err);
}


/* Restore a previous pushed error state.  */

void
gfc_pop_error (gfc_error_buffer *err)
{
  gfc_move_error_buffer_from_to (err, &error_buffer);
}


/* Free a pushed error state, but keep the current error state.  */

void
gfc_free_error (gfc_error_buffer *err)
{
  gfc_clear_pp_buffer (&(err->buffer));
}


/* Report the number of warnings and errors that occurred to the caller.  */

void
gfc_get_errors (int *w, int *e)
{
  if (w != NULL)
    *w = warningcount + werrorcount;
  if (e != NULL)
    *e = errorcount + sorrycount + werrorcount;
}


/* Switch errors into warnings.  */

void
gfc_errors_to_warnings (bool f)
{
  warnings_not_errors = f;
}

void
gfc_diagnostics_init (void)
{
  diagnostic_text_starter (global_dc) = gfc_diagnostic_text_starter;
  diagnostic_start_span (global_dc) = gfc_diagnostic_start_span;
  diagnostic_text_finalizer (global_dc) = gfc_diagnostic_text_finalizer;
  diagnostic_format_decoder (global_dc) = gfc_format_decoder;
  global_dc->m_source_printing.caret_chars[0] = '1';
  global_dc->m_source_printing.caret_chars[1] = '2';
  pp_warning_buffer = new (XNEW (output_buffer)) output_buffer ();
  pp_warning_buffer->m_flush_p = false;
  /* pp_error_buffer is statically allocated.  This simplifies memory
     management when using gfc_push/pop_error. */
  pp_error_buffer = &(error_buffer.buffer);
  pp_error_buffer->m_flush_p = false;
}

void
gfc_diagnostics_finish (void)
{
  tree_diagnostics_defaults (global_dc);
  /* We still want to use the gfc starter and finalizer, not the tree
     defaults.  */
  diagnostic_text_starter (global_dc) = gfc_diagnostic_text_starter;
  diagnostic_text_finalizer (global_dc) = gfc_diagnostic_text_finalizer;
  global_dc->m_source_printing.caret_chars[0] = '^';
  global_dc->m_source_printing.caret_chars[1] = '^';
}
