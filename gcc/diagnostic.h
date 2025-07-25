/* Various declarations for language-independent diagnostics subroutines.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

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

#ifndef GCC_DIAGNOSTIC_H
#define GCC_DIAGNOSTIC_H

#include "unique-argv.h"
#include "rich-location.h"
#include "pretty-print.h"
#include "diagnostic-core.h"

namespace diagnostics {

  // diagnostics::digraphs
  namespace digraphs {
    class lazy_digraph;
  } // namespace diagnostics::digraphs

  // diagnostics::logical_locations
  namespace logical_locations {
    class manager;
  } // namespace diagnostics::logical_locations

  class buffer;
  class client_data_hooks;
  class diagram;
  class edit_context;
  class sink;
    class text_sink;

  class source_effect_info;

} // namespace diagnostics

namespace text_art
{
  class theme;
} // namespace text_art

namespace xml
{
  class printer;
} // namespace xml

/* An enum for controlling what units to use for the column number
   when diagnostics are output, used by the -fdiagnostics-column-unit option.
   Tabs will be expanded or not according to the value of -ftabstop.  The origin
   (default 1) is controlled by -fdiagnostics-column-origin.  */

enum diagnostics_column_unit
{
  /* The default from GCC 11 onwards: display columns.  */
  DIAGNOSTICS_COLUMN_UNIT_DISPLAY,

  /* The behavior in GCC 10 and earlier: simple bytes.  */
  DIAGNOSTICS_COLUMN_UNIT_BYTE
};

/* An enum for controlling how to print non-ASCII characters/bytes when
   a diagnostic suggests escaping the source code on output.  */

enum diagnostics_escape_format
{
  /* Escape non-ASCII Unicode characters in the form <U+XXXX> and
     non-UTF-8 bytes in the form <XX>.  */
  DIAGNOSTICS_ESCAPE_FORMAT_UNICODE,

  /* Escape non-ASCII bytes in the form <XX> (thus showing the underlying
     encoding of non-ASCII Unicode characters).  */
  DIAGNOSTICS_ESCAPE_FORMAT_BYTES
};

/* Enum for overriding the standard output format.  */

enum diagnostics_output_format
{
  /* The default: textual output.  */
  DIAGNOSTICS_OUTPUT_FORMAT_TEXT,

  /* SARIF-based output, as JSON to stderr.  */
  DIAGNOSTICS_OUTPUT_FORMAT_SARIF_STDERR,

  /* SARIF-based output, to a JSON file.  */
  DIAGNOSTICS_OUTPUT_FORMAT_SARIF_FILE
};

/* An enum for controlling how diagnostic paths should be printed.  */
enum diagnostic_path_format
{
  /* Don't print diagnostic paths.  */
  DPF_NONE,

  /* Print diagnostic paths by emitting a separate "note" for every event
     in the path.  */
  DPF_SEPARATE_EVENTS,

  /* Print diagnostic paths by consolidating events together where they
     are close enough, and printing such runs of events with multiple
     calls to diagnostic_show_locus, showing the individual events in
     each run via labels in the source.  */
  DPF_INLINE_EVENTS
};

/* An enum for capturing values of GCC_EXTRA_DIAGNOSTIC_OUTPUT,
   and for -fdiagnostics-parseable-fixits.  */

enum diagnostics_extra_output_kind
{
  /* No extra output, or an unrecognized value.  */
  EXTRA_DIAGNOSTIC_OUTPUT_none,

  /* Emit fix-it hints using the "fixits-v1" format, equivalent to
     -fdiagnostics-parseable-fixits.  */
  EXTRA_DIAGNOSTIC_OUTPUT_fixits_v1,

  /* Emit fix-it hints using the "fixits-v2" format.  */
  EXTRA_DIAGNOSTIC_OUTPUT_fixits_v2
};

/* Values for -fdiagnostics-text-art-charset=.  */

enum diagnostic_text_art_charset
{
  /* No text art diagrams shall be emitted.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_NONE,

  /* Use pure ASCII for text art diagrams.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_ASCII,

  /* Use ASCII + conservative use of other unicode characters
     in text art diagrams.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE,

  /* Use Emoji.  */
  DIAGNOSTICS_TEXT_ART_CHARSET_EMOJI
};

#include "diagnostics/diagnostic-info.h"
typedef diagnostics::diagnostic_info diagnostic_info;

#include "diagnostics/context.h"

/* Extension hooks for client.  */
#define diagnostic_context_auxiliary_data(DC) (DC)->m_client_aux_data
#define diagnostic_info_auxiliary_data(DI) (DI)->m_x_data

/* This diagnostics::context is used by front-ends that directly output
   diagnostic messages without going through `error', `warning',
   and similar functions.  */
extern diagnostics::context *global_dc;

/* The number of errors that have been issued so far.  Ideally, these
   would take a diagnostics::context as an argument.  */
#define errorcount global_dc->diagnostic_count (DK_ERROR)
/* Similarly, but for warnings.  */
#define warningcount global_dc->diagnostic_count (DK_WARNING)
/* Similarly, but for warnings promoted to errors.  */
#define werrorcount global_dc->diagnostic_count (DK_WERROR)
/* Similarly, but for sorrys.  */
#define sorrycount global_dc->diagnostic_count (DK_SORRY)

/* Returns nonzero if warnings should be emitted.  */
#define diagnostic_report_warnings_p(DC, LOC)				\
  (!(DC)->m_inhibit_warnings						\
   && !(in_system_header_at (LOC) && !(DC)->m_warn_system_headers))

/* Override the option index to be used for reporting a
   diagnostic.  */

inline void
diagnostic_set_option_id (diagnostic_info *info,
			  diagnostic_option_id option_id)
{
  info->m_option_id = option_id;
}

/* Diagnostic related functions.  */

inline void
diagnostic_initialize (diagnostics::context *context, int n_opts)
{
  context->initialize (n_opts);
}

inline void
diagnostic_color_init (diagnostics::context *context, int value = -1)
{
  context->color_init (value);
}

inline void
diagnostic_urls_init (diagnostics::context *context, int value = -1)
{
  context->urls_init (value);
}

inline void
diagnostic_finish (diagnostics::context *context)
{
  context->finish ();
}

inline void
diagnostic_show_locus (diagnostics::context *context,
		       const diagnostics::source_printing_options &opts,
		       rich_location *richloc,
		       diagnostic_t diagnostic_kind,
		       pretty_printer *pp,
		       diagnostics::source_effect_info *effect_info = nullptr)
{
  gcc_assert (context);
  gcc_assert (richloc);
  gcc_assert (pp);
  context->maybe_show_locus (*richloc, opts, diagnostic_kind, *pp, effect_info);
}

inline void
diagnostic_show_locus_as_html (diagnostics::context *context,
			       const diagnostics::source_printing_options &opts,
			       rich_location *richloc,
			       diagnostic_t diagnostic_kind,
			       xml::printer &xp,
			       diagnostics::source_effect_info *effect_info = nullptr,
			       diagnostics::html_label_writer *label_writer = nullptr)
{
  gcc_assert (context);
  gcc_assert (richloc);
  context->maybe_show_locus_as_html (*richloc, opts, diagnostic_kind, xp,
				     effect_info, label_writer);
}

/* Because we read source files a second time after the frontend did it the
   first time, we need to know how the frontend handled things like character
   set conversion and UTF-8 BOM stripping, in order to make everything
   consistent.  This function needs to be called by each frontend that requires
   non-default behavior, to inform the diagnostics infrastructure how input is
   to be processed.  The default behavior is to do no conversion and not to
   strip a UTF-8 BOM.

   The callback should return the input charset to be used to convert the given
   file's contents to UTF-8, or it should return NULL if no conversion is needed
   for this file.  SHOULD_SKIP_BOM only applies in case no conversion was
   performed, and if true, it will cause a UTF-8 BOM to be skipped at the
   beginning of the file.  (In case a conversion was performed, the BOM is
   rather skipped as part of the conversion process.)  */

inline void
diagnostic_initialize_input_context (diagnostics::context *context,
				     diagnostic_input_charset_callback ccb,
				     bool should_skip_bom)
{
  context->initialize_input_context (ccb, should_skip_bom);
}

/* Force diagnostics controlled by OPTIDX to be kind KIND.  */
inline diagnostic_t
diagnostic_classify_diagnostic (diagnostics::context *context,
				diagnostic_option_id option_id,
				diagnostic_t kind,
				location_t where)
{
  return context->classify_diagnostic (option_id, kind, where);
}

inline void
diagnostic_push_diagnostics (diagnostics::context *context,
			     location_t where)
{
  context->push_diagnostics (where);
}
inline void
diagnostic_pop_diagnostics (diagnostics::context *context,
			    location_t where)
{
  context->pop_diagnostics (where);
}

/* Report a diagnostic message (an error or a warning) as specified by
   DC.  This function is *the* subroutine in terms of which front-ends
   should implement their specific diagnostic handling modules.  The
   front-end independent format specifiers are exactly those described
   in the documentation of output_format.
   Return true if a diagnostic was printed, false otherwise.  */

inline bool
diagnostic_report_diagnostic (diagnostics::context *context,
			      diagnostic_info *diagnostic)
{
  context->begin_group ();
  bool warned = context->report_diagnostic (diagnostic);
  context->end_group ();
  return warned;
}

#ifdef ATTRIBUTE_GCC_DIAG
extern void diagnostic_set_info (diagnostic_info *, const char *, va_list *,
				 rich_location *, diagnostic_t) ATTRIBUTE_GCC_DIAG(2,0);
extern void diagnostic_set_info_translated (diagnostic_info *, const char *,
					    va_list *, rich_location *,
					    diagnostic_t)
     ATTRIBUTE_GCC_DIAG(2,0);
#endif

namespace diagnostics {

void default_text_starter (diagnostics::text_sink &,
			   const diagnostic_info *);
template <typename Sink>
void default_start_span_fn (const diagnostics::location_print_policy &,
			    Sink &sink,
			    expanded_location);
void default_text_finalizer (diagnostics::text_sink &,
			     const diagnostic_info *,
			     diagnostic_t);
} // namespace diagnostics

void diagnostic_set_caret_max_width (diagnostics::context *context, int value);

int get_terminal_width (void);

/* Return the location associated to this diagnostic. Parameter WHICH
   specifies which location. By default, expand the first one.  */

inline location_t
diagnostic_location (const diagnostic_info * diagnostic, int which = 0)
{
  return diagnostic->m_message.get_location (which);
}

/* Return the number of locations to be printed in DIAGNOSTIC.  */

inline unsigned int
diagnostic_num_locations (const diagnostic_info * diagnostic)
{
  return diagnostic->m_message.m_richloc->get_num_locations ();
}

/* Expand the location of this diagnostic. Use this function for
   consistency.  Parameter WHICH specifies which location. By default,
   expand the first one.  */

inline expanded_location
diagnostic_expand_location (const diagnostic_info * diagnostic, int which = 0)
{
  return diagnostic->m_richloc->get_expanded_location (which);
}

/* This is somehow the right-side margin of a caret line, that is, we
   print at least these many characters after the position pointed at
   by the caret.  */
const int CARET_LINE_MARGIN = 10;

/* Return true if the two locations can be represented within the same
   caret line.  This is used to build a prefix and also to determine
   whether to print one or two caret lines.  */

inline bool
diagnostic_same_line (const diagnostics::context *context,
		      expanded_location s1, expanded_location s2)
{
  return (s2.column && s1.line == s2.line
	  && (context->m_source_printing.max_width - CARET_LINE_MARGIN
	      > abs (s1.column - s2.column)));
}

extern const char *diagnostic_get_color_for_kind (diagnostic_t kind);

/* Pure text formatting support functions.  */

extern char *build_message_string (const char *, ...) ATTRIBUTE_PRINTF_1;

/* Compute the number of digits in the decimal representation of an integer.  */
extern int num_digits (int);

inline bool
warning_enabled_at (location_t loc, diagnostic_option_id option_id)
{
  return global_dc->warning_enabled_at (loc, option_id);
}

inline bool
option_unspecified_p (diagnostic_option_id option_id)
{
  return global_dc->option_unspecified_p (option_id);
}

extern char *get_cwe_url (int cwe);

extern const char *get_diagnostic_kind_text (diagnostic_t kind);

namespace diagnostics {

const char *maybe_line_and_column (int line, int col);

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTIC_H */
