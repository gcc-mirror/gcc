/* Classic text-based output of diagnostics.
   Copyright (C) 1999-2024 Free Software Foundation, Inc.

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


#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "intl.h"
#include "diagnostic.h"
#include "diagnostic-color.h"
#include "diagnostic-url.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "diagnostic-client-data-hooks.h"
#include "diagnostic-diagram.h"
#include "diagnostic-format-text.h"
#include "text-art/theme.h"

/* class diagnostic_text_output_format : public diagnostic_output_format.  */

diagnostic_text_output_format::~diagnostic_text_output_format ()
{
  /* Some of the errors may actually have been warnings.  */
  if (m_context.diagnostic_count (DK_WERROR))
    {
      pretty_printer *pp = m_context.m_printer;
      /* -Werror was given.  */
      if (m_context.warning_as_error_requested_p ())
	pp_verbatim (pp,
		     _("%s: all warnings being treated as errors"),
		     progname);
      /* At least one -Werror= was given.  */
      else
	pp_verbatim (pp,
		     _("%s: some warnings being treated as errors"),
		     progname);
      pp_newline_and_flush (pp);
    }
}

/* Implementation of diagnostic_output_format::on_report_diagnostic vfunc
   for GCC's standard textual output.  */

void
diagnostic_text_output_format::
on_report_diagnostic (const diagnostic_info &diagnostic,
		      diagnostic_t orig_diag_kind)
{
  pretty_printer *pp = m_context.m_printer;

  (*diagnostic_starter (&m_context)) (&m_context, &diagnostic);

  pp_output_formatted_text (pp, m_context.get_urlifier ());

  if (m_context.m_show_cwe)
    print_any_cwe (diagnostic);

  if (m_context.m_show_rules)
    print_any_rules (diagnostic);

  if (m_context.m_show_option_requested)
    print_option_information (diagnostic, orig_diag_kind);

  (*diagnostic_finalizer (&m_context)) (&m_context,
					&diagnostic,
					orig_diag_kind);
}

void
diagnostic_text_output_format::on_diagram (const diagnostic_diagram &diagram)
{
  pretty_printer *const pp = get_printer ();

  char *saved_prefix = pp_take_prefix (pp);
  pp_set_prefix (pp, NULL);
  /* Use a newline before and after and a two-space indent
     to make the diagram stand out a little from the wall of text.  */
  pp_newline (pp);
  diagram.get_canvas ().print_to_pp (pp, "  ");
  pp_newline (pp);
  pp_set_prefix (pp, saved_prefix);
  pp_flush (pp);
}

/* If DIAGNOSTIC has a CWE identifier, print it.

   For example, if the diagnostic metadata associates it with CWE-119,
   " [CWE-119]" will be printed, suitably colorized, and with a URL of a
   description of the security issue.  */

void
diagnostic_text_output_format::print_any_cwe (const diagnostic_info &diagnostic)
{
  if (diagnostic.metadata == NULL)
    return;

  int cwe = diagnostic.metadata->get_cwe ();
  if (cwe)
    {
      pretty_printer * const pp = get_printer ();
      char *saved_prefix = pp_take_prefix (pp);
      pp_string (pp, " [");
      const char *kind_color = diagnostic_get_color_for_kind (diagnostic.kind);
      pp_string (pp, colorize_start (pp_show_color (pp), kind_color));
      if (pp->supports_urls_p ())
	{
	  char *cwe_url = get_cwe_url (cwe);
	  pp_begin_url (pp, cwe_url);
	  free (cwe_url);
	}
      pp_printf (pp, "CWE-%i", cwe);
      pp_set_prefix (pp, saved_prefix);
      if (pp->supports_urls_p ())
	pp_end_url (pp);
      pp_string (pp, colorize_stop (pp_show_color (pp)));
      pp_character (pp, ']');
    }
}

/* If DIAGNOSTIC has any rules associated with it, print them.

   For example, if the diagnostic metadata associates it with a rule
   named "STR34-C", then " [STR34-C]" will be printed, suitably colorized,
   with any URL provided by the rule.  */

void
diagnostic_text_output_format::
print_any_rules (const diagnostic_info &diagnostic)
{
  if (diagnostic.metadata == NULL)
    return;

  for (unsigned idx = 0; idx < diagnostic.metadata->get_num_rules (); idx++)
    {
      const diagnostic_metadata::rule &rule
	= diagnostic.metadata->get_rule (idx);
      if (char *desc = rule.make_description ())
	{
	  pretty_printer * const pp = get_printer ();
	  char *saved_prefix = pp_take_prefix (pp);
	  pp_string (pp, " [");
	  const char *kind_color
	    = diagnostic_get_color_for_kind (diagnostic.kind);
	  pp_string (pp, colorize_start (pp_show_color (pp), kind_color));
	  char *url = NULL;
	  if (pp->supports_urls_p ())
	    {
	      url = rule.make_url ();
	      if (url)
		pp_begin_url (pp, url);
	    }
	  pp_string (pp, desc);
	  pp_set_prefix (pp, saved_prefix);
	  if (pp->supports_urls_p ())
	    if (url)
	      pp_end_url (pp);
	  free (url);
	  pp_string (pp, colorize_stop (pp_show_color (pp)));
	  pp_character (pp, ']');
	  free (desc);
	}
    }
}

/* Print any metadata about the option used to control DIAGNOSTIC to CONTEXT's
   printer, e.g. " [-Werror=uninitialized]".
   Subroutine of diagnostic_context::report_diagnostic.  */

void
diagnostic_text_output_format::
print_option_information (const diagnostic_info &diagnostic,
			  diagnostic_t orig_diag_kind)
{
  if (char *option_text
      = m_context.make_option_name (diagnostic.option_id,
				    orig_diag_kind, diagnostic.kind))
    {
      char *option_url = nullptr;
      pretty_printer * const pp = get_printer ();
      if (pp->supports_urls_p ())
	option_url = m_context.make_option_url (diagnostic.option_id);
      pp_string (pp, " [");
      const char *kind_color = diagnostic_get_color_for_kind (diagnostic.kind);
      pp_string (pp, colorize_start (pp_show_color (pp), kind_color));
      if (option_url)
	pp_begin_url (pp, option_url);
      pp_string (pp, option_text);
      if (option_url)
	{
	  pp_end_url (pp);
	  free (option_url);
	}
      pp_string (pp, colorize_stop (pp_show_color (pp)));
      pp_character (pp, ']');
      free (option_text);
    }
}
