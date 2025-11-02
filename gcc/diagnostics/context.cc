/* Language-independent diagnostic subroutines for the GNU Compiler Collection
   Copyright (C) 1999-2026 Free Software Foundation, Inc.
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


/* This file implements the language independent aspect of diagnostic
   message module.  */

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "demangle.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "diagnostics/color.h"
#include "diagnostics/url.h"
#include "diagnostics/metadata.h"
#include "diagnostics/paths.h"
#include "diagnostics/client-data-hooks.h"
#include "diagnostics/diagram.h"
#include "diagnostics/sink.h"
#include "diagnostics/sarif-sink.h"
#include "diagnostics/text-sink.h"
#include "diagnostics/changes.h"
#include "selftest.h"
#include "diagnostics/selftest-context.h"
#include "opts.h"
#include "cpplib.h"
#include "text-art/theme.h"
#include "pretty-print-urlifier.h"
#include "diagnostics/logical-locations.h"
#include "diagnostics/buffering.h"
#include "diagnostics/file-cache.h"
#include "diagnostics/dumping.h"
#include "diagnostics/logging.h"

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef GWINSZ_IN_SYS_IOCTL
# include <sys/ioctl.h>
#endif

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

static void real_abort (void) ATTRIBUTE_NORETURN;

/* Name of program invoked, sans directories.  */

const char *progname;


/* Return a malloc'd string containing MSG formatted a la printf.  The
   caller is responsible for freeing the memory.  */
char *
build_message_string (const char *msg, ...)
{
  char *str;
  va_list ap;

  va_start (ap, msg);
  str = xvasprintf (msg, ap);
  va_end (ap);

  return str;
}


/* Return the value of the getenv("COLUMNS") as an integer. If the
   value is not set to a positive integer, use ioctl to get the
   terminal width. If it fails, return INT_MAX.  */
int
get_terminal_width (void)
{
  const char * s = getenv ("COLUMNS");
  if (s != nullptr) {
    int n = atoi (s);
    if (n > 0)
      return n;
  }

#ifdef TIOCGWINSZ
  struct winsize w;
  w.ws_col = 0;
  if (ioctl (0, TIOCGWINSZ, &w) == 0 && w.ws_col > 0)
    return w.ws_col;
#endif

  return INT_MAX;
}

namespace diagnostics {

/* Set caret_max_width to value.  */

void
context::set_caret_max_width (int value)
{
  /* One minus to account for the leading empty space.  */
  value = value ? value - 1
    : (isatty (fileno (pp_buffer (get_reference_printer ())->m_stream))
       ? get_terminal_width () - 1 : INT_MAX);

  if (value <= 0)
    value = INT_MAX;

  m_source_printing.max_width = value;
}

/* Initialize the diagnostic message outputting machinery.  */

void
context::initialize (int n_opts)
{
  /* Allocate a basic pretty-printer.  Clients will replace this a
     much more elaborated pretty-printer if they wish.  */
  m_reference_printer = std::make_unique<pretty_printer> ().release ();

  m_file_cache = new file_cache ();
  m_diagnostic_counters.clear ();
  m_warning_as_error_requested = false;
  m_n_opts = n_opts;
  m_option_classifier.init (n_opts);
  m_source_printing.enabled = false;
  set_caret_max_width (pp_line_cutoff (get_reference_printer ()));
  for (int i = 0; i < rich_location::STATICALLY_ALLOCATED_RANGES; i++)
    m_source_printing.caret_chars[i] = '^';
  m_show_cwe = false;
  m_show_rules = false;
  m_path_format = DPF_NONE;
  m_show_path_depths = false;
  m_show_option_requested = false;
  m_abort_on_error = false;
  m_show_column = false;
  m_pedantic_errors = false;
  m_permissive = false;
  m_opt_permissive = 0;
  m_fatal_errors = false;
  m_inhibit_warnings = false;
  m_warn_system_headers = false;
  m_max_errors = 0;
  m_internal_error = nullptr;
  m_adjust_diagnostic_info = nullptr;
  m_text_callbacks.m_begin_diagnostic = default_text_starter;
  m_text_callbacks.m_text_start_span
    = default_start_span_fn<to_text>;
  m_text_callbacks.m_html_start_span
    = default_start_span_fn<to_html>;
  m_text_callbacks.m_end_diagnostic = default_text_finalizer;
  m_option_id_mgr = nullptr;
  m_urlifier_stack = new auto_vec<urlifier_stack_node> ();
  m_last_location = UNKNOWN_LOCATION;
  m_client_aux_data = nullptr;
  m_lock = 0;
  m_inhibit_notes_p = false;

  m_source_printing.colorize_source_p = false;
  m_source_printing.show_labels_p = false;
  m_source_printing.show_line_numbers_p = false;
  m_source_printing.min_margin_width = 0;
  m_source_printing.show_ruler_p = false;
  m_source_printing.show_event_links_p = false;

  m_column_options.m_column_unit = DIAGNOSTICS_COLUMN_UNIT_DISPLAY;
  m_column_options.m_column_origin = 1;
  m_column_options.m_tabstop = 8;

  m_report_bug = false;
  m_extra_output_kind = EXTRA_DIAGNOSTIC_OUTPUT_none;
  if (const char *var = getenv ("GCC_EXTRA_DIAGNOSTIC_OUTPUT"))
    {
      if (!strcmp (var, "fixits-v1"))
	m_extra_output_kind = EXTRA_DIAGNOSTIC_OUTPUT_fixits_v1;
      else if (!strcmp (var, "fixits-v2"))
	m_extra_output_kind = EXTRA_DIAGNOSTIC_OUTPUT_fixits_v2;
      /* Silently ignore unrecognized values.  */
    }
  m_escape_format = DIAGNOSTICS_ESCAPE_FORMAT_UNICODE;
  m_fixits_change_set = nullptr;
  m_diagnostic_groups.m_group_nesting_depth = 0;
  m_diagnostic_groups.m_diagnostic_nesting_level = 0;
  m_diagnostic_groups.m_emission_count = 0;
  m_diagnostic_groups.m_inhibiting_notes_from = 0;
  m_sinks.safe_push (new text_sink (*this, nullptr, true));
  m_set_locations_cb = nullptr;
  m_client_data_hooks = nullptr;
  m_diagrams.m_theme = nullptr;
  m_original_argv = nullptr;
  m_diagnostic_buffer = nullptr;
  m_logger = nullptr;

  enum diagnostic_text_art_charset text_art_charset
    = DIAGNOSTICS_TEXT_ART_CHARSET_EMOJI;
  if (const char *lang = getenv ("LANG"))
    {
      /* For LANG=C, don't assume the terminal supports anything
	 other than ASCII.  */
      if (!strcmp (lang, "C"))
	text_art_charset = DIAGNOSTICS_TEXT_ART_CHARSET_ASCII;
    }
  set_text_art_charset (text_art_charset);

  if (const char *name = getenv ("GCC_DIAGNOSTICS_LOG"))
    {
      if (name[0] != '\0')
	{
	  /* Try to write a log to the named path.  */
	  if (FILE *outfile = fopen (name, "w"))
	    m_logger = new logging::logger
	      (output_file (outfile, true,
			    label_text::take (xstrdup (name))));
	}
      else
	/* Write a log to stderr.  */
	m_logger = new logging::logger
	  (output_file
	   (stderr, false,
	    label_text::borrow ("stderr")));
    }

  if (m_logger)
    m_logger->log_printf ("diagnostics::context::initialize");
}

/* Maybe initialize the color support. We require clients to do this
   explicitly, since most clients don't want color.  When called
   without a VALUE, it initializes with DIAGNOSTICS_COLOR_DEFAULT.  */

void
context::color_init (int value)
{
  /* value == -1 is the default value.  */
  if (value < 0)
    {
      /* If DIAGNOSTICS_COLOR_DEFAULT is -1, default to
	 -fdiagnostics-color=auto if GCC_COLORS is in the environment,
	 otherwise default to -fdiagnostics-color=never, for other
	 values default to that
	 -fdiagnostics-color={never,auto,always}.  */
      if (DIAGNOSTICS_COLOR_DEFAULT == -1)
	{
	  if (!getenv ("GCC_COLORS"))
	    return;
	  value = DIAGNOSTICS_COLOR_AUTO;
	}
      else
	value = DIAGNOSTICS_COLOR_DEFAULT;
    }
  pp_show_color (m_reference_printer)
    = colorize_init ((diagnostic_color_rule_t) value);
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      pp_show_color (sink_->get_printer ())
	= pp_show_color (m_reference_printer);
}

/* Initialize URL support within this context based on VALUE,
   handling "auto".  */

void
context::urls_init (int value)
{
  /* value == -1 is the default value.  */
  if (value < 0)
    {
      /* If DIAGNOSTICS_URLS_DEFAULT is -1, default to
	 -fdiagnostics-urls=auto if GCC_URLS or TERM_URLS is in the
	 environment, otherwise default to -fdiagnostics-urls=never,
	 for other values default to that
	 -fdiagnostics-urls={never,auto,always}.  */
      if (DIAGNOSTICS_URLS_DEFAULT == -1)
	{
	  if (!getenv ("GCC_URLS") && !getenv ("TERM_URLS"))
	    return;
	  value = DIAGNOSTICS_URL_AUTO;
	}
      else
	value = DIAGNOSTICS_URLS_DEFAULT;
    }

  m_reference_printer->set_url_format
    (determine_url_format ((diagnostic_url_rule_t) value));
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      sink_->get_printer ()->set_url_format
	(m_reference_printer->get_url_format ());
}

void
context::set_show_nesting (bool val)
{
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      if (auto text_sink_ = sink_->dyn_cast_text_sink ())
	text_sink_->set_show_nesting (val);
}

void
context::set_show_nesting_locations (bool val)
{
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      if (auto text_sink_ = sink_->dyn_cast_text_sink ())
	text_sink_->set_show_locations_in_nesting (val);
}

void
context::set_show_nesting_levels (bool val)
{
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      if (auto text_sink_ = sink_->dyn_cast_text_sink ())
	text_sink_->set_show_nesting_levels (val);
}

/* Create the file_cache, if not already created, and tell it how to
   translate files on input.  */
void
context::initialize_input_context (diagnostic_input_charset_callback ccb,
				   bool should_skip_bom)
{
  m_file_cache->initialize_input_context (ccb, should_skip_bom);
}

/* Do any cleaning up required after the last diagnostic is emitted.  */

void
context::finish ()
{
  if (m_logger)
    {
      m_logger->log_printf ("diagnostics::context::finish");
      /* We're cleaning up the logger before this function exits,
	 so we can't use auto_inc_depth here.  */
      m_logger->inc_depth ();
      dump (m_logger->get_stream (), m_logger->get_indent ());
    }

  for (auto iter : m_sinks)
    iter->finalize_extensions ();

  /* We might be handling a fatal error.
     Close any active diagnostic groups, which may trigger flushing
     sinks.  */
  while (m_diagnostic_groups.m_group_nesting_depth > 0)
    end_group ();

  set_diagnostic_buffer (nullptr);

  /* Clean ups.  */

  while (!m_sinks.is_empty ())
    delete m_sinks.pop ();

  if (m_diagrams.m_theme)
    {
      delete m_diagrams.m_theme;
      m_diagrams.m_theme = nullptr;
    }

  delete m_file_cache;
  m_file_cache = nullptr;

  m_option_classifier.fini ();

  delete m_reference_printer;
  m_reference_printer = nullptr;

  if (m_fixits_change_set)
    {
      delete m_fixits_change_set;
      m_fixits_change_set = nullptr;
    }

  if (m_client_data_hooks)
    {
      delete m_client_data_hooks;
      m_client_data_hooks = nullptr;
    }

  delete m_option_id_mgr;
  m_option_id_mgr = nullptr;

  if (m_urlifier_stack)
    {
      while (!m_urlifier_stack->is_empty ())
	pop_urlifier ();
      delete m_urlifier_stack;
      m_urlifier_stack = nullptr;
    }

  freeargv (m_original_argv);
  m_original_argv = nullptr;

  delete m_logger;
  m_logger = nullptr;
}

/* Dump state of this diagnostics::context to OUT, for debugging.  */

void
context::dump (FILE *outfile, int indent) const
{
  dumping::emit_heading (outfile, indent, "diagnostics::context");
  m_diagnostic_counters.dump (outfile, indent + 2);
  dumping::emit_heading (outfile, indent + 2, "reference printer");
  if (m_reference_printer)
    m_reference_printer->dump (outfile, indent + 4);
  else
    dumping::emit_none (outfile, indent + 4);
  dumping::emit_heading (outfile, indent + 2, "output sinks");
  if (m_sinks.length () > 0)
    {
      for (unsigned i = 0; i < m_sinks.length (); ++i)
	{
	  dumping::emit_indent (outfile, indent + 4);
	  const sink *s = m_sinks[i];
	  fprintf (outfile, "sink %i (", i);
	  s->dump_kind (outfile);
	  fprintf (outfile, "):\n");
	  s->dump (outfile, indent + 6);
	}
    }
  else
    dumping::emit_none (outfile, indent + 4);
  dumping::emit_heading (outfile, indent + 2, "diagnostic buffer");
  if (m_diagnostic_buffer)
    m_diagnostic_buffer->dump (outfile, indent + 4);
  else
    dumping::emit_none (outfile, indent + 4);
  dumping::emit_heading (outfile, indent + 2, "file cache");
  if (m_file_cache)
    m_file_cache->dump (outfile, indent + 4);
  else
    dumping::emit_none (outfile, indent + 4);
  dumping::emit_heading (outfile, indent + 2, "client data hooks");
  if (m_client_data_hooks)
    m_client_data_hooks->dump (outfile, indent + 4);
  else
    dumping::emit_none (outfile, indent + 4);
}

/* Return true if sufficiently severe diagnostics have been seen that
   we ought to exit with a non-zero exit code.  */

bool
context::execution_failed_p () const
{
  return (diagnostic_count (kind::fatal)
	  || diagnostic_count (kind::error)
	  || diagnostic_count (kind::sorry)
	  || diagnostic_count (kind::werror));
}

void
context::remove_all_output_sinks ()
{
  while (!m_sinks.is_empty ())
    delete m_sinks.pop ();
}

void
context::set_sink (std::unique_ptr<sink> sink_)
{
  DIAGNOSTICS_LOG_SCOPE_PRINTF0 (m_logger, "diagnostics::context::set_sink");
  if (m_logger)
    sink_->dump (m_logger->get_stream (), m_logger->get_indent ());
  remove_all_output_sinks ();
  m_sinks.safe_push (sink_.release ());
}

sink &
context::get_sink (size_t idx) const
{
  gcc_assert (idx < m_sinks.length ());
  gcc_assert (m_sinks[idx]);
  return *m_sinks[idx];
}

void
context::add_sink (std::unique_ptr<sink> sink_)
{
  DIAGNOSTICS_LOG_SCOPE_PRINTF0 (m_logger, "diagnostics::context::add_sink");
  if (m_logger)
    sink_->dump (m_logger->get_stream (), m_logger->get_indent ());
  m_sinks.safe_push (sink_.release ());
}

/* Return true if there are no machine-readable formats writing to stderr.  */

bool
context::supports_fnotice_on_stderr_p () const
{
  for (auto sink_ : m_sinks)
    if (sink_->machine_readable_stderr_p ())
      return false;
  return true;
}

void
context::set_main_input_filename (const char *filename)
{
  for (auto sink_ : m_sinks)
    sink_->set_main_input_filename (filename);
}

void
context::set_client_data_hooks (std::unique_ptr<client_data_hooks> hooks)
{
  delete m_client_data_hooks;
  /* Ideally the field would be a std::unique_ptr here.  */
  m_client_data_hooks = hooks.release ();
}

void
context::set_original_argv (unique_argv original_argv)
{
  /* Ideally we'd use a unique_argv for m_original_argv, but
     diagnostics::context doesn't yet have a ctor/dtor pair.  */

  // Ensure any old value is freed
  freeargv (m_original_argv);

  // Take ownership of the new value
  m_original_argv = original_argv.release ();
}

void
context::set_option_id_manager (std::unique_ptr<option_id_manager> mgr,
				unsigned lang_mask)
{
  delete m_option_id_mgr;
  m_option_id_mgr = mgr.release ();
  m_lang_mask = lang_mask;
}

void
context::push_owned_urlifier (std::unique_ptr<urlifier> ptr)
{
  gcc_assert (m_urlifier_stack);
  const urlifier_stack_node node = { ptr.release (), true };
  m_urlifier_stack->safe_push (node);
}

void
context::push_borrowed_urlifier (const urlifier &loan)
{
  gcc_assert (m_urlifier_stack);
  const urlifier_stack_node node = { const_cast <urlifier *> (&loan), false };
  m_urlifier_stack->safe_push (node);
}

void
context::pop_urlifier ()
{
  gcc_assert (m_urlifier_stack);
  gcc_assert (m_urlifier_stack->length () > 0);

  const urlifier_stack_node node = m_urlifier_stack->pop ();
  if (node.m_owned)
    delete node.m_urlifier;
}

const logical_locations::manager *
context::get_logical_location_manager () const
{
  if (!m_client_data_hooks)
    return nullptr;
  return m_client_data_hooks->get_logical_location_manager ();
}

const urlifier *
context::get_urlifier () const
{
  if (!m_urlifier_stack)
    return nullptr;
  if (m_urlifier_stack->is_empty ())
    return nullptr;
  return m_urlifier_stack->last ().m_urlifier;
}


/* Set PP as the reference printer for this context.
   Refresh all output sinks.  */

void
context::set_pretty_printer (std::unique_ptr<pretty_printer> pp)
{
  delete m_reference_printer;
  m_reference_printer = pp.release ();
  refresh_output_sinks ();
}

/* Give all output sinks a chance to rebuild their pretty_printer.  */

void
context::refresh_output_sinks ()
{
  for (auto sink_ : m_sinks)
    sink_->update_printer ();
}

/* Set FORMAT_DECODER on the reference printer and on the pretty_printer
   of all output sinks.  */

void
context::set_format_decoder (printer_fn format_decoder)
{
  pp_format_decoder (m_reference_printer) = format_decoder;
  for (auto sink_ : m_sinks)
    pp_format_decoder (sink_->get_printer ()) = format_decoder;
}

void
context::set_show_highlight_colors (bool val)
{
  pp_show_highlight_colors (m_reference_printer) = val;
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      pp_show_highlight_colors (sink_->get_printer ()) = val;
}

void
context::set_prefixing_rule (diagnostic_prefixing_rule_t rule)
{
  pp_prefixing_rule (m_reference_printer) = rule;
  for (auto sink_ : m_sinks)
    if (sink_->follows_reference_printer_p ())
      pp_prefixing_rule (sink_->get_printer ()) = rule;
}

void
context::initialize_fixits_change_set ()
{
  delete m_fixits_change_set;
  gcc_assert (m_file_cache);
  m_fixits_change_set = new changes::change_set (*m_file_cache);
}

// class client_data_hooks

void
client_data_hooks::dump (FILE *outfile, int indent) const
{
  dumping::emit_heading (outfile, indent, "logical location manager");
  if (auto mgr = get_logical_location_manager ())
    mgr->dump (outfile, indent + 2);
  else
    dumping::emit_none (outfile, indent + 2);
}

} // namespace diagnostics

/* Initialize DIAGNOSTIC, where the message MSG has already been
   translated.  */
void
diagnostic_set_info_translated (diagnostics::diagnostic_info *diagnostic,
				const char *msg, va_list *args,
				rich_location *richloc,
				enum diagnostics::kind kind)
{
  gcc_assert (richloc);
  diagnostic->m_message.m_err_no = errno;
  diagnostic->m_message.m_args_ptr = args;
  diagnostic->m_message.m_format_spec = msg;
  diagnostic->m_message.m_richloc = richloc;
  diagnostic->m_richloc = richloc;
  diagnostic->m_metadata = nullptr;
  diagnostic->m_kind = kind;
  diagnostic->m_option_id = 0;
}

/* Initialize DIAGNOSTIC, where the message GMSGID has not yet been
   translated.  */
void
diagnostic_set_info (diagnostics::diagnostic_info *diagnostic,
		     const char *gmsgid, va_list *args,
		     rich_location *richloc,
		     enum diagnostics::kind kind)
{
  gcc_assert (richloc);
  diagnostic_set_info_translated (diagnostic, _(gmsgid), args, richloc, kind);
}

namespace diagnostics {

static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "diagnostics/kinds.def"
#undef DEFINE_DIAGNOSTIC_KIND
  "must-not-happen"
};

/* Get unlocalized string describing KIND.  */

const char *
get_text_for_kind (enum kind kind)
{
  return diagnostic_kind_text[static_cast<int> (kind)];
}

static const char *const diagnostic_kind_debug_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (#K),
#include "diagnostics/kinds.def"
#undef DEFINE_DIAGNOSTIC_KIND
  "must-not-happen"
};

const char *
get_debug_string_for_kind (enum kind kind)
{
  return diagnostic_kind_debug_text[static_cast<int> (kind)];
}

static const char *const diagnostic_kind_color[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (C),
#include "diagnostics/kinds.def"
#undef DEFINE_DIAGNOSTIC_KIND
  nullptr
};

/* Get a color name for diagnostics of type KIND
   Result could be nullptr.  */

const char *
get_color_for_kind (enum kind kind)
{
  return diagnostic_kind_color[static_cast<int> (kind)];
}

/* Given an expanded_location, convert the column (which is in 1-based bytes)
   to the requested units, without converting the origin.
   Return -1 if the column is invalid (<= 0).  */

static int
convert_column_unit (file_cache &fc,
		     enum diagnostics_column_unit column_unit,
		     int tabstop,
		     expanded_location s)
{
  if (s.column <= 0)
    return -1;

  switch (column_unit)
    {
    default:
      gcc_unreachable ();

    case DIAGNOSTICS_COLUMN_UNIT_DISPLAY:
      {
	cpp_char_column_policy policy (tabstop, cpp_wcwidth);
	return location_compute_display_column (fc, s, policy);
      }

    case DIAGNOSTICS_COLUMN_UNIT_BYTE:
      return s.column;
    }
}

/* Given an expanded_location, convert the column (which is in 1-based bytes)
   to the requested units and origin.  Return -1 if the column is
   invalid (<= 0).  */
int
column_options::convert_column (file_cache &fc,
				expanded_location s) const
{
  int one_based_col = convert_column_unit (fc, m_column_unit, m_tabstop, s);
  if (one_based_col <= 0)
    return -1;
  return one_based_col + (m_column_origin - 1);
}

column_policy::column_policy (const context &dc)
: m_file_cache (dc.get_file_cache ()),
  m_column_options (dc.get_column_options ())
{
}

/* Given an expanded_location, convert the column (which is in 1-based bytes)
   to the requested units and origin.  Return -1 if the column is
   invalid (<= 0).  */
int
column_policy::converted_column (expanded_location s) const
{
  return m_column_options.convert_column (m_file_cache, s);
}

/* Return a string describing a location e.g. "foo.c:42:10".  */

label_text
column_policy::get_location_text (const expanded_location &s,
				  bool show_column,
				  bool colorize) const
{
  const char *locus_cs = colorize_start (colorize, "locus");
  const char *locus_ce = colorize_stop (colorize);
  const char *file = s.file ? s.file : progname;
  int line = 0;
  int col = -1;
  if (strcmp (file, special_fname_builtin ()))
    {
      line = s.line;
      if (show_column)
	col = converted_column (s);
    }

  const char *line_col = text_sink::maybe_line_and_column (line, col);
  return label_text::take (build_message_string ("%s%s%s:%s", locus_cs, file,
						 line_col, locus_ce));
}

location_print_policy::
location_print_policy (const context &dc)
: m_column_policy (dc),
  m_show_column (dc.m_show_column)
{
}

location_print_policy::
location_print_policy (const text_sink &text_output)
:
  m_column_policy (text_output.get_context ()),
  m_show_column (text_output.get_context ().m_show_column)
{
}

} // namespace diagnostics

/* Functions at which to stop the backtrace print.  It's not
   particularly helpful to print the callers of these functions.  */

static const char * const bt_stop[] =
{
  "main",
  "toplev::main",
  "execute_one_pass",
  "compile_file",
};

/* A callback function passed to the backtrace_full function.  */

static int
bt_callback (void *data, uintptr_t pc, const char *filename, int lineno,
	     const char *function)
{
  int *pcount = (int *) data;

  /* If we don't have any useful information, don't print
     anything.  */
  if (filename == nullptr && function == nullptr)
    return 0;

  /* Skip functions in context.cc.  */
  if (*pcount == 0
      && filename != nullptr
      && strcmp (lbasename (filename), "context.cc") == 0)
    return 0;

  /* Print up to 20 functions.  We could make this a --param, but
     since this is only for debugging just use a constant for now.  */
  if (*pcount >= 20)
    {
      /* Returning a non-zero value stops the backtrace.  */
      return 1;
    }
  ++*pcount;

  char *alc = nullptr;
  if (function != nullptr)
    {
      char *str = cplus_demangle_v3 (function,
				     (DMGL_VERBOSE | DMGL_ANSI
				      | DMGL_GNU_V3 | DMGL_PARAMS));
      if (str != nullptr)
	{
	  alc = str;
	  function = str;
	}

      for (size_t i = 0; i < ARRAY_SIZE (bt_stop); ++i)
	{
	  size_t len = strlen (bt_stop[i]);
	  if (strncmp (function, bt_stop[i], len) == 0
	      && (function[len] == '\0' || function[len] == '('))
	    {
	      if (alc != nullptr)
		free (alc);
	      /* Returning a non-zero value stops the backtrace.  */
	      return 1;
	    }
	}
    }

  fprintf (stderr, "0x%lx %s\n\t%s:%d\n",
	   (unsigned long) pc,
	   function == nullptr ? "???" : function,
	   filename == nullptr ? "???" : filename,
	   lineno);

  if (alc != nullptr)
    free (alc);

  return 0;
}

/* A callback function passed to the backtrace_full function.  This is
   called if backtrace_full has an error.  */

static void
bt_err_callback (void *data ATTRIBUTE_UNUSED, const char *msg, int errnum)
{
  if (errnum < 0)
    {
      /* This means that no debug info was available.  Just quietly
	 skip printing backtrace info.  */
      return;
    }
  fprintf (stderr, "%s%s%s\n", msg, errnum == 0 ? "" : ": ",
	   errnum == 0 ? "" : xstrerror (errnum));
}

namespace diagnostics {

/* Check if we've met the maximum error limit, and if so fatally exit
   with a message.
   FLUSH indicates whether a diagnostics::context::finish call is needed.  */

void
context::check_max_errors (bool flush)
{
  if (!m_max_errors)
    return;

  int count = (diagnostic_count (kind::error)
	       + diagnostic_count (kind::sorry)
	       + diagnostic_count (kind::werror));

  if (count >= m_max_errors)
    {
      fnotice (stderr,
	       "compilation terminated due to -fmax-errors=%u.\n",
	       m_max_errors);
      if (flush)
	finish ();
      exit (FATAL_EXIT_CODE);
    }
}

/* Take any action which is expected to happen after the diagnostic
   is written out.  This function does not always return.  */

void
context::action_after_output (enum kind diag_kind)
{
  switch (diag_kind)
    {
    case kind::debug:
    case kind::note:
    case kind::anachronism:
    case kind::warning:
      break;

    case kind::error:
    case kind::sorry:
      if (m_abort_on_error)
	real_abort ();
      if (m_fatal_errors)
	{
	  fnotice (stderr, "compilation terminated due to -Wfatal-errors.\n");
	  finish ();
	  exit (FATAL_EXIT_CODE);
	}
      break;

    case kind::ice:
    case kind::ice_nobt:
      {
	/* Attempt to ensure that any outputs are flushed e.g. that .sarif
	   files are written out.
	   Only do it once.  */
	static char **saved_argv = nullptr;
	if (!saved_argv)
	  {
	    saved_argv = m_original_argv;
	    m_original_argv = nullptr;
	    finish ();
	  }

	struct backtrace_state *state = nullptr;
	if (diag_kind == kind::ice)
	  state = backtrace_create_state (nullptr, 0, bt_err_callback, nullptr);
	int count = 0;
	if (state != nullptr)
	  backtrace_full (state, 2, bt_callback, bt_err_callback,
			  (void *) &count);

	if (m_abort_on_error)
	  real_abort ();

	bool space = false;
	for (auto *argv = saved_argv; *argv; space = true)
	  fnotice (stderr, &" %s"[1 - space], *argv++);
	fnotice (stderr, "\n");
	freeargv (saved_argv);

	if (m_report_bug)
	  fnotice (stderr, "Please submit a full bug report, "
		   "with preprocessed source.\n");
	else
	  fnotice (stderr, "Please submit a full bug report, "
		   "with preprocessed source (by using -freport-bug).\n");

	if (count > 0)
	  fnotice (stderr, "Please include the complete backtrace "
		   "with any bug report.\n");
	fnotice (stderr, "See %s for instructions.\n", bug_report_url);

	exit (ICE_EXIT_CODE);
      }

    case kind::fatal:
      if (m_abort_on_error)
	real_abort ();
      fnotice (stderr, "compilation terminated.\n");
      finish ();
      exit (FATAL_EXIT_CODE);

    default:
      gcc_unreachable ();
    }
}

/* State whether we should inhibit notes in the current diagnostic_group and
   its future children if any.  */

void
context::inhibit_notes_in_group (bool inhibit)
{
  int curr_depth = (m_diagnostic_groups.m_group_nesting_depth
		    + m_diagnostic_groups.m_diagnostic_nesting_level);

  if (inhibit)
    {
      /* If we're already inhibiting, there's nothing to do.  */
      if (m_diagnostic_groups.m_inhibiting_notes_from)
	return;

      /* Since we're called via warning/error/... that all have their own
	 diagnostic_group, we must consider that we started inhibiting in their
	 parent.  */
      gcc_assert (m_diagnostic_groups.m_group_nesting_depth > 0);
      m_diagnostic_groups.m_inhibiting_notes_from = curr_depth - 1;
    }
  else if (m_diagnostic_groups.m_inhibiting_notes_from)
    {
      /* Only cancel inhibition at the depth that set it up.  */
      if (curr_depth >= m_diagnostic_groups.m_inhibiting_notes_from)
	return;

      m_diagnostic_groups.m_inhibiting_notes_from = 0;
    }
}

/* Return whether notes must be inhibited in the current diagnostic_group.  */

bool
context::notes_inhibited_in_group () const
{
  if (m_diagnostic_groups.m_inhibiting_notes_from
      && (m_diagnostic_groups.m_group_nesting_depth
	  + m_diagnostic_groups.m_diagnostic_nesting_level
	  >= m_diagnostic_groups.m_inhibiting_notes_from))
    return true;
  return false;
}

/* class diagnostics::logical_locations::manager.  */

/* Return true iff this is a function or method.  */

bool
logical_locations::manager::function_p (key k) const
{
  switch (get_kind (k))
    {
    default:
      gcc_unreachable ();
    case kind::unknown:
    case kind::module_:
    case kind::namespace_:
    case kind::type:
    case kind::return_type:
    case kind::parameter:
    case kind::variable:
      return false;

    case kind::function:
    case kind::member:
      return true;
    }
}

/* Helper function for print_parseable_fixits.  Print TEXT to PP, obeying the
   escaping rules for -fdiagnostics-parseable-fixits.  */

static void
print_escaped_string (pretty_printer *pp, const char *text)
{
  gcc_assert (pp);
  gcc_assert (text);

  pp_character (pp, '"');
  for (const char *ch = text; *ch; ch++)
    {
      switch (*ch)
	{
	case '\\':
	  /* Escape backslash as two backslashes.  */
	  pp_string (pp, "\\\\");
	  break;
	case '\t':
	  /* Escape tab as "\t".  */
	  pp_string (pp, "\\t");
	  break;
	case '\n':
	  /* Escape newline as "\n".  */
	  pp_string (pp, "\\n");
	  break;
	case '"':
	  /* Escape doublequotes as \".  */
	  pp_string (pp, "\\\"");
	  break;
	default:
	  if (ISPRINT (*ch))
	    pp_character (pp, *ch);
	  else
	    /* Use octal for non-printable chars.  */
	    {
	      unsigned char c = (*ch & 0xff);
	      pp_printf (pp, "\\%o%o%o", (c / 64), (c / 8) & 007, c & 007);
	    }
	  break;
	}
    }
  pp_character (pp, '"');
}

/* Implementation of -fdiagnostics-parseable-fixits and
   GCC_EXTRA_DIAGNOSTIC_OUTPUT.
   Print a machine-parseable version of all fixits in RICHLOC to PP,
   using COLUMN_UNIT to express columns.
   Use TABSTOP when handling DIAGNOSTICS_COLUMN_UNIT_DISPLAY.  */

static void
print_parseable_fixits (file_cache &fc,
			pretty_printer *pp, rich_location *richloc,
			enum diagnostics_column_unit column_unit,
			int tabstop)
{
  gcc_assert (pp);
  gcc_assert (richloc);

  char *saved_prefix = pp_take_prefix (pp);
  pp_set_prefix (pp, nullptr);

  for (unsigned i = 0; i < richloc->get_num_fixit_hints (); i++)
    {
      const fixit_hint *hint = richloc->get_fixit_hint (i);
      location_t start_loc = hint->get_start_loc ();
      expanded_location start_exploc = expand_location (start_loc);
      pp_string (pp, "fix-it:");
      print_escaped_string (pp, start_exploc.file);
      /* For compatibility with clang, print as a half-open range.  */
      location_t next_loc = hint->get_next_loc ();
      expanded_location next_exploc = expand_location (next_loc);
      int start_col
	= convert_column_unit (fc, column_unit, tabstop, start_exploc);
      int next_col
	= convert_column_unit (fc, column_unit, tabstop, next_exploc);
      pp_printf (pp, ":{%i:%i-%i:%i}:",
		 start_exploc.line, start_col,
		 next_exploc.line, next_col);
      print_escaped_string (pp, hint->get_string ());
      pp_newline (pp);
    }

  pp_set_prefix (pp, saved_prefix);
}

/* Update the inlining info in this context for a DIAGNOSTIC.  */

void
context::get_any_inlining_info (diagnostic_info *diagnostic)
{
  auto &ilocs = diagnostic->m_iinfo.m_ilocs;

  if (m_set_locations_cb)
    /* Retrieve the locations into which the expression about to be
       diagnosed has been inlined, including those of all the callers
       all the way down the inlining stack.  */
    m_set_locations_cb (*this, diagnostic);
  else
    {
      /* When there's no callback use just the one location provided
	 by the caller of the diagnostic function.  */
      location_t loc = diagnostic_location (diagnostic);
      ilocs.safe_push (loc);
      diagnostic->m_iinfo.m_allsyslocs = in_system_header_at (loc);
    }
}

/* Generate a URL string describing CWE.  The caller is responsible for
   freeing the string.  */

char *
get_cwe_url (int cwe)
{
  return xasprintf ("https://cwe.mitre.org/data/definitions/%i.html", cwe);
}

/* Returns whether a DIAGNOSTIC should be printed, and adjusts diagnostic->kind
   as appropriate for #pragma GCC diagnostic and -Werror=foo.  */

bool
context::diagnostic_enabled (diagnostic_info *diagnostic)
{
  /* Update the inlining stack for this diagnostic.  */
  get_any_inlining_info (diagnostic);

  /* Diagnostics with no option or -fpermissive are always enabled.  */
  if (!diagnostic->m_option_id.m_idx
      || diagnostic->m_option_id == m_opt_permissive)
    return true;

  /* This tests if the user provided the appropriate -Wfoo or
     -Wno-foo option.  */
  if (!option_enabled_p (diagnostic->m_option_id))
    return false;

  /* This tests for #pragma diagnostic changes.  */
  enum kind diag_class
    = m_option_classifier.update_effective_level_from_pragmas (diagnostic);

  /* This tests if the user provided the appropriate -Werror=foo
     option.  */
  if (diag_class == kind::unspecified
      && !option_unspecified_p (diagnostic->m_option_id))
    {
      const enum kind new_kind
	= m_option_classifier.get_current_override (diagnostic->m_option_id);
      if (new_kind != kind::any)
	/* kind::any means the diagnostic is not to be ignored, but we don't want
	   to change it specifically to kind::error or kind::warning; we want to
	   preserve whatever the caller has specified.  */
	diagnostic->m_kind = new_kind;
    }

  /* This allows for future extensions, like temporarily disabling
     warnings for ranges of source code.  */
  if (diagnostic->m_kind == kind::ignored)
    return false;

  return true;
}

/* Returns whether warning OPT_ID is enabled at LOC.  */

bool
context::warning_enabled_at (location_t loc,
			     option_id opt_id)
{
  if (!diagnostic_report_warnings_p (this, loc))
    return false;

  rich_location richloc (line_table, loc);
  diagnostic_info diagnostic = {};
  diagnostic.m_option_id = opt_id;
  diagnostic.m_richloc = &richloc;
  diagnostic.m_message.m_richloc = &richloc;
  diagnostic.m_kind = kind::warning;
  return diagnostic_enabled (&diagnostic);
}

/* Emit a diagnostic within a diagnostic group on this context.  */

bool
context::emit_diagnostic_with_group (enum kind kind,
				     rich_location &richloc,
				     const metadata *metadata,
				     option_id opt_id,
				     const char *gmsgid, ...)
{
  begin_group ();

  va_list ap;
  va_start (ap, gmsgid);
  bool ret = emit_diagnostic_with_group_va (kind, richloc, metadata, opt_id,
					    gmsgid, &ap);
  va_end (ap);

  end_group ();

  return ret;
}

/* As above, but taking a va_list *.  */

bool
context::emit_diagnostic_with_group_va (enum kind kind,
					rich_location &richloc,
					const metadata *metadata,
					option_id opt_id,
					const char *gmsgid, va_list *ap)
{
  begin_group ();

  bool ret = diagnostic_impl (&richloc, metadata, opt_id,
			      gmsgid, ap, kind);

  end_group ();

  return ret;
}

/* Report a diagnostic message (an error or a warning) as specified by
   this diagnostics::context.
   front-end independent format specifiers are exactly those described
   in the documentation of output_format.
   Return true if a diagnostic was printed, false otherwise.  */

bool
context::report_diagnostic (diagnostic_info *diagnostic)
{
  auto logger = get_logger ();
  DIAGNOSTICS_LOG_SCOPE_PRINTF0 (logger, "diagnostics::context::report_diagnostic");

  enum kind orig_diag_kind = diagnostic->m_kind;

  /* Every call to report_diagnostic should be within a
     begin_group/end_group pair so that output formats can reliably
     flush diagnostics with on_end_group when the topmost group is ended.  */
  gcc_assert (m_diagnostic_groups.m_group_nesting_depth > 0);

  /* Give preference to being able to inhibit warnings, before they
     get reclassified to something else.  */
  bool was_warning = (diagnostic->m_kind == kind::warning
		      || diagnostic->m_kind == kind::pedwarn);
  if (was_warning && m_inhibit_warnings)
    {
      inhibit_notes_in_group ();
      if (m_logger)
	m_logger->log_printf ("rejecting: inhibiting warnings");
      return false;
    }

  if (m_adjust_diagnostic_info)
    m_adjust_diagnostic_info (*this, diagnostic);

  if (diagnostic->m_kind == kind::pedwarn)
    {
      diagnostic->m_kind = m_pedantic_errors ? kind::error : kind::warning;

      /* We do this to avoid giving the message for -pedantic-errors.  */
      orig_diag_kind = diagnostic->m_kind;
    }

  if (diagnostic->m_kind == kind::note && m_inhibit_notes_p)
    {
      if (m_logger)
	m_logger->log_printf ("rejecting: inhibiting notes");
      return false;
    }

  /* If the user requested that warnings be treated as errors, so be
     it.  Note that we do this before the next block so that
     individual warnings can be overridden back to warnings with
     -Wno-error=*.  */
  if (m_warning_as_error_requested
      && diagnostic->m_kind == kind::warning)
    diagnostic->m_kind = kind::error;

  diagnostic->m_message.m_data = &diagnostic->m_x_data;

  /* Check to see if the diagnostic is enabled at the location and
     not disabled by #pragma GCC diagnostic anywhere along the inlining
     stack.  .  */
  if (!diagnostic_enabled (diagnostic))
    {
      if (m_logger)
	m_logger->log_printf ("rejecting: diagnostic not enabled");
      inhibit_notes_in_group ();
      return false;
    }

  if ((was_warning || diagnostic->m_kind == kind::warning)
      && ((!m_warn_system_headers
	   && diagnostic->m_iinfo.m_allsyslocs)
	  || m_inhibit_warnings))
    {
      /* Bail if the warning is not to be reported because all locations in the
	 inlining stack (if there is one) are in system headers.  */
      if (m_logger)
	m_logger->log_printf ("rejecting: warning in system header");
      return false;
    }

  if (diagnostic->m_kind == kind::note && notes_inhibited_in_group ())
    {
      /* Bail for all the notes in the diagnostic_group that started to inhibit notes.  */
      if (m_logger)
	m_logger->log_printf ("rejecting: notes inhibited within group");
      return false;
    }

  if (diagnostic->m_kind != kind::note && diagnostic->m_kind != kind::ice)
    check_max_errors (false);

  if (m_lock > 0)
    {
      /* If we're reporting an ICE in the middle of some other error,
	 try to flush out the previous error, then let this one
	 through.  Don't do this more than once.  */
      if ((diagnostic->m_kind == kind::ice
	   || diagnostic->m_kind == kind::ice_nobt)
	  && m_lock == 1)
	pp_newline_and_flush (m_reference_printer);
      else
	error_recursion ();
    }

  /* We are accepting the diagnostic, so should stop inhibiting notes.  */
  inhibit_notes_in_group (/*inhibit=*/false);

  m_lock++;

  if (diagnostic->m_kind == kind::ice || diagnostic->m_kind == kind::ice_nobt)
    {
      /* When not checking, ICEs are converted to fatal errors when an
	 error has already occurred.  This is counteracted by
	 abort_on_error.  */
      if (!CHECKING_P
	  && (diagnostic_count (kind::error) > 0
	      || diagnostic_count (kind::sorry) > 0)
	  && !m_abort_on_error)
	{
	  expanded_location s
	    = expand_location (diagnostic_location (diagnostic));
	  fnotice (stderr, "%s:%d: confused by earlier errors, bailing out\n",
		   s.file, s.line);
	  exit (ICE_EXIT_CODE);
	}
      if (m_internal_error)
	(*m_internal_error) (this,
			     diagnostic->m_message.m_format_spec,
			     diagnostic->m_message.m_args_ptr);
    }

  /* Increment the counter for the appropriate diagnostic kind, either
     within this context, or within the diagnostic_buffer.  */
  {
    const enum kind kind_for_count =
      ((diagnostic->m_kind == kind::error && orig_diag_kind == kind::warning)
       ? kind::werror
       : diagnostic->m_kind);
    counters &cs
      = (m_diagnostic_buffer
	 ? m_diagnostic_buffer->m_diagnostic_counters
	 : m_diagnostic_counters);
    ++cs.m_count_for_kind[static_cast<size_t> (kind_for_count)];
  }

  /* Is this the initial diagnostic within the stack of groups?  */
  if (m_diagnostic_groups.m_emission_count == 0)
    {
      DIAGNOSTICS_LOG_SCOPE_PRINTF0
	(get_logger (),
	 "diagnostics::context: beginning group");
      for (auto sink_ : m_sinks)
	sink_->on_begin_group ();
    }
  m_diagnostic_groups.m_emission_count++;

  va_list *orig_args = diagnostic->m_message.m_args_ptr;
  for (auto sink_ : m_sinks)
    {
      /* Formatting the message is done per-output-format,
	 so that each output format gets its own set of pp_token_lists
	 to work with.

	 Run phases 1 and 2 of formatting the message before calling
	 the format's on_report_diagnostic.
	 In particular, some format codes may have side-effects here which
	 need to happen before sending the diagnostic to the output format.
	 For example, Fortran's %C and %L formatting codes populate the
	 rich_location.
	 Such side-effects must be idempotent, since they are run per
	 output-format.

	 Make a duplicate of the varargs for each call to pp_format,
	 so that each has its own set to consume.  */
      va_list copied_args;
      va_copy (copied_args, *orig_args);
      diagnostic->m_message.m_args_ptr = &copied_args;
      pp_format (sink_->get_printer (), &diagnostic->m_message);
      va_end (copied_args);

      /* Call vfunc in the output format.  This is responsible for
	 phase 3 of formatting, and for printing the result.  */
      sink_->on_report_diagnostic (*diagnostic, orig_diag_kind);
    }

  const int tabstop = get_column_options ().m_tabstop;

  switch (m_extra_output_kind)
    {
    default:
      break;
    case EXTRA_DIAGNOSTIC_OUTPUT_fixits_v1:
      print_parseable_fixits (get_file_cache (),
			      m_reference_printer, diagnostic->m_richloc,
			      DIAGNOSTICS_COLUMN_UNIT_BYTE,
			      tabstop);
      pp_flush (m_reference_printer);
      break;
    case EXTRA_DIAGNOSTIC_OUTPUT_fixits_v2:
      print_parseable_fixits (get_file_cache (),
			      m_reference_printer, diagnostic->m_richloc,
			      DIAGNOSTICS_COLUMN_UNIT_DISPLAY,
			      tabstop);
      pp_flush (m_reference_printer);
      break;
    }
  if (m_diagnostic_buffer == nullptr
      || diagnostic->m_kind == kind::ice
      || diagnostic->m_kind == kind::ice_nobt)
    action_after_output (diagnostic->m_kind);
  diagnostic->m_x_data = nullptr;

  if (m_fixits_change_set)
    if (diagnostic->m_richloc->fixits_can_be_auto_applied_p ())
      if (!m_diagnostic_buffer)
	m_fixits_change_set->add_fixits (diagnostic->m_richloc);

  m_lock--;

  if (!m_diagnostic_buffer)
    for (auto sink_ : m_sinks)
      sink_->after_diagnostic (*diagnostic);

  return true;
}

void
context::report_verbatim (text_info &text)
{
  va_list *orig_args = text.m_args_ptr;
  for (auto sink_ : m_sinks)
    {
      va_list copied_args;
      va_copy (copied_args, *orig_args);
      text.m_args_ptr = &copied_args;
      sink_->on_report_verbatim (text);
      va_end (copied_args);
    }
}

void
context::report_global_digraph (const lazily_created<digraphs::digraph> &ldg)
{
  for (auto sink_ : m_sinks)
    sink_->report_global_digraph (ldg);
}

/* Get the number of digits in the decimal representation of VALUE.  */

int
num_digits (uint64_t value)
{
  /* Perhaps simpler to use log10 for this, but doing it this way avoids
     using floating point.  */

  if (value == 0)
    return 1;

  int digits = 0;
  while (value > 0)
    {
      digits++;
      value /= 10;
    }
  return digits;
}

} // namespace diagnostics

/* Given a partial pathname as input, return another pathname that
   shares no directory elements with the pathname of __FILE__.  This
   is used by fancy_abort() to print `internal compiler error in expr.cc'
   instead of `internal compiler error in ../../GCC/gcc/expr.cc'.  */

const char *
trim_filename (const char *name)
{
  static const char this_file[] = __FILE__;
  const char *p = name, *q = this_file;

  /* First skip any "../" in each filename.  This allows us to give a proper
     reference to a file in a subdirectory.  */
  while (p[0] == '.' && p[1] == '.' && IS_DIR_SEPARATOR (p[2]))
    p += 3;

  while (q[0] == '.' && q[1] == '.' && IS_DIR_SEPARATOR (q[2]))
    q += 3;

  /* Now skip any parts the two filenames have in common.  */
  while (*p == *q && *p != 0 && *q != 0)
    p++, q++;

  /* Now go backwards until the previous directory separator.  */
  while (p > name && !IS_DIR_SEPARATOR (p[-1]))
    p--;

  return p;
}

namespace diagnostics {

/* Implement emit_diagnostic, inform, warning, warning_at, pedwarn,
   permerror, error, error_at, error_at, sorry, fatal_error, internal_error,
   and internal_error_no_backtrace, as documented and defined below.  */
bool
context::diagnostic_impl (rich_location *richloc,
			  const metadata *metadata,
			  option_id opt_id,
			  const char *gmsgid,
			  va_list *ap, enum kind kind)
{
  logging::log_function_params
    (m_logger, "diagnostics::context::diagnostic_impl")
    .log_param_option_id ("option_id", opt_id)
    .log_param_kind ("kind", kind)
    .log_param_string ("gmsgid", gmsgid);
  logging::auto_inc_depth depth_sentinel (m_logger);

  diagnostic_info diagnostic;
  if (kind == diagnostics::kind::permerror)
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc,
			   (m_permissive
			    ? diagnostics::kind::warning
			    : diagnostics::kind::error));
      diagnostic.m_option_id = (opt_id.m_idx != -1 ? opt_id : m_opt_permissive);
    }
  else
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc, kind);
      if (kind == diagnostics::kind::warning
	  || kind == diagnostics::kind::pedwarn)
	diagnostic.m_option_id = opt_id;
    }
  diagnostic.m_metadata = metadata;

  bool ret = report_diagnostic (&diagnostic);
  if (m_logger)
    m_logger->log_bool_return ("diagnostics::context::diagnostic_impl", ret);
  return ret;
}

/* Implement inform_n, warning_n, and error_n, as documented and
   defined below.  */
bool
context::diagnostic_n_impl (rich_location *richloc,
			    const metadata *metadata,
			    option_id opt_id,
			    unsigned HOST_WIDE_INT n,
			    const char *singular_gmsgid,
			    const char *plural_gmsgid,
			    va_list *ap, enum kind kind)
{
  logging::log_function_params
    (m_logger, "diagnostics::context::diagnostic_n_impl")
    .log_param_option_id ("option_id", opt_id)
    .log_param_kind ("kind", kind)
    .log_params_n_gmsgids (n, singular_gmsgid, plural_gmsgid);
  logging::auto_inc_depth depth_sentinel (m_logger);

  diagnostic_info diagnostic;
  unsigned long gtn;

  if (sizeof n <= sizeof gtn)
    gtn = n;
  else
    /* Use the largest number ngettext can handle, otherwise
       preserve the six least significant decimal digits for
       languages where the plural form depends on them.  */
    gtn = n <= ULONG_MAX ? n : n % 1000000LU + 1000000LU;

  const char *text = ngettext (singular_gmsgid, plural_gmsgid, gtn);
  diagnostic_set_info_translated (&diagnostic, text, ap, richloc, kind);
  if (kind == diagnostics::kind::warning)
    diagnostic.m_option_id = opt_id;
  diagnostic.m_metadata = metadata;

  bool ret = report_diagnostic (&diagnostic);
  if (m_logger)
    m_logger->log_bool_return ("diagnostics::context::diagnostic_n_impl", ret);
  return ret;
}


/* Emit DIAGRAM to this context, respecting the output format.  */

void
context::emit_diagram (const diagram &diag)
{
  if (m_diagrams.m_theme == nullptr)
    return;

  for (auto sink_ : m_sinks)
    sink_->on_diagram (diag);
}

/* Inform the user that an error occurred while trying to report some
   other error.  This indicates catastrophic internal inconsistencies,
   so give up now.  But do try to flush out the previous error.
   This mustn't use internal_error, that will cause infinite recursion.  */

void
context::error_recursion ()
{
  if (m_lock < 3)
    pp_newline_and_flush (m_reference_printer);

  fnotice (stderr,
	   "internal compiler error: error reporting routines re-entered.\n");

  /* Call action_after_output to get the "please submit a bug report"
     message.  */
  action_after_output (kind::ice);

  /* Do not use gcc_unreachable here; that goes through internal_error
     and therefore would cause infinite recursion.  */
  real_abort ();
}

} // namespace diagnostics

/* Report an internal compiler error in a friendly manner.  This is
   the function that gets called upon use of abort() in the source
   code generally, thanks to a special macro.  */

void
fancy_abort (const char *file, int line, const char *function)
{
  /* If fancy_abort is called before the diagnostic subsystem is initialized,
     internal_error will crash internally in a way that prevents a
     useful message reaching the user.
     This can happen with libgccjit in the case of gcc_assert failures
     that occur outside of the libgccjit mutex that guards the rest of
     gcc's state, including global_dc (when global_dc may not be
     initialized yet, or might be in use by another thread).
     Handle such cases as gracefully as possible by falling back to a
     minimal abort handler that only relies on i18n.  */
  if (global_dc->get_reference_printer () == nullptr)
    {
      /* Print the error message.  */
      fnotice (stderr, diagnostics::get_text_for_kind (diagnostics::kind::ice));
      fnotice (stderr, "in %s, at %s:%d", function, trim_filename (file), line);
      fputc ('\n', stderr);

      /* Attempt to print a backtrace.  */
      struct backtrace_state *state
	= backtrace_create_state (nullptr, 0, bt_err_callback, nullptr);
      int count = 0;
      if (state != nullptr)
	backtrace_full (state, 2, bt_callback, bt_err_callback,
			(void *) &count);

      /* We can't call warn_if_plugins or emergency_dump_function as these
	 rely on GCC state that might not be initialized, or might be in
	 use by another thread.  */

      /* Abort the process.  */
      real_abort ();
    }

  internal_error ("in %s, at %s:%d", function, trim_filename (file), line);
}

namespace diagnostics {

/* class diagnostics::context.  */

void
context::begin_group ()
{
  m_diagnostic_groups.m_group_nesting_depth++;
}

void
context::end_group ()
{
  if (--m_diagnostic_groups.m_group_nesting_depth == 0)
    {
      /* Handle the case where we've popped the final diagnostic group.
	 If any diagnostics were emitted, give the context a chance
	 to do something.  */
      if (m_diagnostic_groups.m_emission_count > 0)
	{
	  DIAGNOSTICS_LOG_SCOPE_PRINTF0
	    (get_logger (),
	     "diagnostics::context::end_group: ending group");
	  for (auto sink_ : m_sinks)
	    sink_->on_end_group ();
	}
      m_diagnostic_groups.m_emission_count = 0;
    }
  /* We're popping one level, so might need to stop inhibiting notes.  */
  inhibit_notes_in_group (/*inhibit=*/false);
}

void
context::push_nesting_level ()
{
  ++m_diagnostic_groups.m_diagnostic_nesting_level;
}

void
context::pop_nesting_level ()
{
  --m_diagnostic_groups.m_diagnostic_nesting_level;
  /* We're popping one level, so might need to stop inhibiting notes.  */
  inhibit_notes_in_group (/*inhibit=*/false);
}

void
context::set_nesting_level (int new_level)
{
  m_diagnostic_groups.m_diagnostic_nesting_level = new_level;
}

void
sink::dump (FILE *out, int indent) const
{
  dumping::emit_heading (out, indent, "printer");
  m_printer->dump (out, indent + 2);

  dumping::emit_heading (out, indent, "extensions");
  if (m_extensions.empty ())
    dumping::emit_none (out, indent + 2);
  else
    for (auto &ext : m_extensions)
      ext->dump (out, indent + 2);
}

void
sink::finalize_extensions ()
{
  for (auto &ext : m_extensions)
    ext->finalize ();
}

void
sink::on_report_verbatim (text_info &)
{
  /* No-op.  */
}

/* Set the output format for DC to FORMAT, using BASE_FILE_NAME for
   file-based output formats.  */

void
output_format_init (context &dc,
		    const char *main_input_filename_,
		    const char *base_file_name,
		    enum diagnostics_output_format format,
		    bool json_formatting)
{
  sink *new_sink = nullptr;
  switch (format)
    {
    default:
      gcc_unreachable ();
    case DIAGNOSTICS_OUTPUT_FORMAT_TEXT:
      /* The default; do nothing.  */
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_SARIF_STDERR:
      new_sink = &init_sarif_stderr (dc,
				     line_table,
				     json_formatting);
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_SARIF_FILE:
      new_sink = &init_sarif_file (dc,
				   line_table,
				   json_formatting,
				   base_file_name);
      break;
    }
  if (new_sink)
    new_sink->set_main_input_filename (main_input_filename_);
}

/* Initialize this context's m_diagrams based on CHARSET.
   Specifically, make a text_art::theme object for m_diagrams.m_theme,
   (or nullptr for "no diagrams").  */

void
context::set_text_art_charset (enum diagnostic_text_art_charset charset)
{
  delete m_diagrams.m_theme;
  switch (charset)
    {
    default:
      gcc_unreachable ();

    case DIAGNOSTICS_TEXT_ART_CHARSET_NONE:
      m_diagrams.m_theme = nullptr;
      break;

    case DIAGNOSTICS_TEXT_ART_CHARSET_ASCII:
      m_diagrams.m_theme = new text_art::ascii_theme ();
      break;

    case DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE:
      m_diagrams.m_theme = new text_art::unicode_theme ();
      break;

    case DIAGNOSTICS_TEXT_ART_CHARSET_EMOJI:
      m_diagrams.m_theme = new text_art::emoji_theme ();
      break;
    }
}

/* struct diagnostics::counters.  */

counters::counters ()
{
  clear ();
}

void
counters::dump (FILE *out, int indent) const
{
  dumping::emit_heading (out, indent, "counts");
  bool none = true;
  for (int i = 0; i < static_cast<int> (kind::last_diagnostic_kind); i++)
    if (m_count_for_kind[i] > 0)
      {
	dumping::emit_indent (out, indent + 2);
	fprintf (out, "%s%i\n",
		 get_text_for_kind (static_cast<enum kind> (i)),
		 m_count_for_kind[i]);
	none = false;
      }
  if (none)
    dumping::emit_none (out, indent + 2);
}

void
counters::move_to (counters &dest)
{
  for (int i = 0; i < static_cast<int> (kind::last_diagnostic_kind); i++)
    dest.m_count_for_kind[i] += m_count_for_kind[i];
  clear ();
}

void
counters::clear ()
{
  memset (&m_count_for_kind, 0, sizeof m_count_for_kind);
}

#if CHECKING_P

namespace selftest {

using line_table_test = ::selftest::line_table_test;
using temp_source_file = ::selftest::temp_source_file;

/* Helper function for test_print_escaped_string.  */

static void
assert_print_escaped_string (const ::selftest::location &loc,
			     const char *expected_output,
			     const char *input)
{
  pretty_printer pp;
  print_escaped_string (&pp, input);
  ASSERT_STREQ_AT (loc, expected_output, pp_formatted_text (&pp));
}

#define ASSERT_PRINT_ESCAPED_STRING_STREQ(EXPECTED_OUTPUT, INPUT) \
    assert_print_escaped_string (SELFTEST_LOCATION, EXPECTED_OUTPUT, INPUT)

/* Tests of print_escaped_string.  */

static void
test_print_escaped_string ()
{
  /* Empty string.  */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"\"", "");

  /* Non-empty string.  */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"hello world\"", "hello world");

  /* Various things that need to be escaped:  */
  /* Backslash.  */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"before\\\\after\"",
				     "before\\after");
  /* Tab.  */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"before\\tafter\"",
				     "before\tafter");
  /* Newline.  */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"before\\nafter\"",
				     "before\nafter");
  /* Double quote.  */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"before\\\"after\"",
				     "before\"after");

  /* Non-printable characters: BEL: '\a': 0x07 */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"before\\007after\"",
				     "before\aafter");
  /* Non-printable characters: vertical tab: '\v': 0x0b */
  ASSERT_PRINT_ESCAPED_STRING_STREQ ("\"before\\013after\"",
				     "before\vafter");
}

/* Tests of print_parseable_fixits.  */

/* Verify that print_parseable_fixits emits the empty string if there
   are no fixits.  */

static void
test_print_parseable_fixits_none ()
{
  pretty_printer pp;
  file_cache fc;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  print_parseable_fixits (fc, &pp, &richloc, DIAGNOSTICS_COLUMN_UNIT_BYTE, 8);
  ASSERT_STREQ ("", pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits does the right thing if there
   is an insertion fixit hint.  */

static void
test_print_parseable_fixits_insert ()
{
  pretty_printer pp;
  file_cache fc;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  linemap_add (line_table, LC_ENTER, false, "test.c", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, nullptr, 0);
  location_t where = linemap_position_for_column (line_table, 10);
  richloc.add_fixit_insert_before (where, "added content");

  print_parseable_fixits (fc, &pp, &richloc, DIAGNOSTICS_COLUMN_UNIT_BYTE, 8);
  ASSERT_STREQ ("fix-it:\"test.c\":{5:10-5:10}:\"added content\"\n",
		pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits does the right thing if there
   is an removal fixit hint.  */

static void
test_print_parseable_fixits_remove ()
{
  pretty_printer pp;
  file_cache fc;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  linemap_add (line_table, LC_ENTER, false, "test.c", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, nullptr, 0);
  source_range where;
  where.m_start = linemap_position_for_column (line_table, 10);
  where.m_finish = linemap_position_for_column (line_table, 20);
  richloc.add_fixit_remove (where);

  print_parseable_fixits (fc, &pp, &richloc, DIAGNOSTICS_COLUMN_UNIT_BYTE, 8);
  ASSERT_STREQ ("fix-it:\"test.c\":{5:10-5:21}:\"\"\n",
		pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits does the right thing if there
   is an replacement fixit hint.  */

static void
test_print_parseable_fixits_replace ()
{
  pretty_printer pp;
  file_cache fc;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  linemap_add (line_table, LC_ENTER, false, "test.c", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, nullptr, 0);
  source_range where;
  where.m_start = linemap_position_for_column (line_table, 10);
  where.m_finish = linemap_position_for_column (line_table, 20);
  richloc.add_fixit_replace (where, "replacement");

  print_parseable_fixits (fc, &pp, &richloc, DIAGNOSTICS_COLUMN_UNIT_BYTE, 8);
  ASSERT_STREQ ("fix-it:\"test.c\":{5:10-5:21}:\"replacement\"\n",
		pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits correctly handles
   DIAGNOSTICS_COLUMN_UNIT_BYTE vs DIAGNOSTICS_COLUMN_UNIT_COLUMN.  */

static void
test_print_parseable_fixits_bytes_vs_display_columns ()
{
  line_table_test ltt;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  /* 1-based byte offsets:     12345677778888999900001234567.  */
  const char *const content = "smile \xf0\x9f\x98\x82 colour\n";
  /* 1-based display cols:     123456[......7-8.....]9012345.  */
  const int tabstop = 8;

  temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
  file_cache fc;
  const char *const fname = tmp.get_filename ();

  linemap_add (line_table, LC_ENTER, false, fname, 0);
  linemap_line_start (line_table, 1, 100);
  linemap_add (line_table, LC_LEAVE, false, nullptr, 0);
  source_range where;
  where.m_start = linemap_position_for_column (line_table, 12);
  where.m_finish = linemap_position_for_column (line_table, 17);
  richloc.add_fixit_replace (where, "color");

  /* Escape fname.  */
  pretty_printer tmp_pp;
  print_escaped_string (&tmp_pp, fname);
  char *escaped_fname = xstrdup (pp_formatted_text (&tmp_pp));

  const int buf_len = strlen (escaped_fname) + 100;
  char *const expected = XNEWVEC (char, buf_len);

  {
    pretty_printer pp;
    print_parseable_fixits (fc, &pp, &richloc,
			    DIAGNOSTICS_COLUMN_UNIT_BYTE,
			    tabstop);
    snprintf (expected, buf_len,
	      "fix-it:%s:{1:12-1:18}:\"color\"\n", escaped_fname);
    ASSERT_STREQ (expected, pp_formatted_text (&pp));
  }
  {
    pretty_printer pp;
    print_parseable_fixits (fc, &pp, &richloc,
			    DIAGNOSTICS_COLUMN_UNIT_DISPLAY,
			    tabstop);
    snprintf (expected, buf_len,
	      "fix-it:%s:{1:10-1:16}:\"color\"\n", escaped_fname);
    ASSERT_STREQ (expected, pp_formatted_text (&pp));
  }

  XDELETEVEC (expected);
  free (escaped_fname);
}

/* Verify that
     diagnostics::column_policy::get_location_text (..., SHOW_COLUMN, ...)
   generates EXPECTED_LOC_TEXT, given FILENAME, LINE, COLUMN, with
   colorization disabled.  */

static void
assert_location_text (const char *expected_loc_text,
		      const char *filename, int line, int column,
		      bool show_column,
		      int origin = 1,
		      enum diagnostics_column_unit column_unit
			= DIAGNOSTICS_COLUMN_UNIT_BYTE)
{
  diagnostics::selftest::test_context dc;
  dc.get_column_options ().m_column_unit = column_unit;
  dc.get_column_options ().m_column_origin = origin;

  expanded_location xloc;
  xloc.file = filename;
  xloc.line = line;
  xloc.column = column;
  xloc.data = nullptr;
  xloc.sysp = false;

  diagnostics::column_policy column_policy (dc);
  label_text actual_loc_text
    = column_policy.get_location_text (xloc, show_column, false);
  ASSERT_STREQ (expected_loc_text, actual_loc_text.get ());
}

/* Verify that get_location_text works as expected.  */

static void
test_get_location_text ()
{
  const char *old_progname = progname;
  progname = "PROGNAME";
  assert_location_text ("PROGNAME:", nullptr, 0, 0, true);
  char *built_in_colon = concat (special_fname_builtin (), ":", (char *) 0);
  assert_location_text (built_in_colon, special_fname_builtin (),
			42, 10, true);
  free (built_in_colon);
  assert_location_text ("foo.c:42:10:", "foo.c", 42, 10, true);
  assert_location_text ("foo.c:42:9:", "foo.c", 42, 10, true, 0);
  assert_location_text ("foo.c:42:1010:", "foo.c", 42, 10, true, 1001);
  for (int origin = 0; origin != 2; ++origin)
    assert_location_text ("foo.c:42:", "foo.c", 42, 0, true, origin);
  assert_location_text ("foo.c:", "foo.c", 0, 10, true);
  assert_location_text ("foo.c:42:", "foo.c", 42, 10, false);
  assert_location_text ("foo.c:", "foo.c", 0, 10, false);

  diagnostics::text_sink::maybe_line_and_column (INT_MAX, INT_MAX);
  diagnostics::text_sink::maybe_line_and_column (INT_MIN, INT_MIN);

  {
    /* In order to test display columns vs byte columns, we need to create a
       file for location_get_source_line() to read.  */

    const char *const content = "smile \xf0\x9f\x98\x82\n";
    const int line_bytes = strlen (content) - 1;
    const int def_tabstop = 8;
    const cpp_char_column_policy policy (def_tabstop, cpp_wcwidth);
    const int display_width = cpp_display_width (content, line_bytes, policy);
    ASSERT_EQ (line_bytes - 2, display_width);
    temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
    const char *const fname = tmp.get_filename ();
    const int buf_len = strlen (fname) + 16;
    char *const expected = XNEWVEC (char, buf_len);

    snprintf (expected, buf_len, "%s:1:%d:", fname, line_bytes);
    assert_location_text (expected, fname, 1, line_bytes, true,
			  1, DIAGNOSTICS_COLUMN_UNIT_BYTE);

    snprintf (expected, buf_len, "%s:1:%d:", fname, line_bytes - 1);
    assert_location_text (expected, fname, 1, line_bytes, true,
			  0, DIAGNOSTICS_COLUMN_UNIT_BYTE);

    snprintf (expected, buf_len, "%s:1:%d:", fname, display_width);
    assert_location_text (expected, fname, 1, line_bytes, true,
			  1, DIAGNOSTICS_COLUMN_UNIT_DISPLAY);

    snprintf (expected, buf_len, "%s:1:%d:", fname, display_width - 1);
    assert_location_text (expected, fname, 1, line_bytes, true,
			  0, DIAGNOSTICS_COLUMN_UNIT_DISPLAY);

    XDELETEVEC (expected);
  }


  progname = old_progname;
}

/* Selftest for num_digits.  */

static void
test_num_digits ()
{
  ASSERT_EQ (1, num_digits (0));
  ASSERT_EQ (1, num_digits (9));
  ASSERT_EQ (2, num_digits (10));
  ASSERT_EQ (2, num_digits (99));
  ASSERT_EQ (3, num_digits (100));
  ASSERT_EQ (3, num_digits (999));
  ASSERT_EQ (4, num_digits (1000));
  ASSERT_EQ (4, num_digits (9999));
  ASSERT_EQ (5, num_digits (10000));
  ASSERT_EQ (5, num_digits (99999));
  ASSERT_EQ (6, num_digits (100000));
  ASSERT_EQ (6, num_digits (999999));
  ASSERT_EQ (7, num_digits (1000000));
  ASSERT_EQ (7, num_digits (9999999));
  ASSERT_EQ (8, num_digits (10000000));
  ASSERT_EQ (8, num_digits (99999999));
  ASSERT_EQ (20, num_digits (uint64_t (-1)));
}

/* Run all of the selftests within this file.

   According to https://gcc.gnu.org/pipermail/gcc/2021-November/237703.html
   there are some language-specific assumptions within these tests, so only
   run them from C/C++.  */

void
context_cc_tests ()
{
  test_print_escaped_string ();
  test_print_parseable_fixits_none ();
  test_print_parseable_fixits_insert ();
  test_print_parseable_fixits_remove ();
  test_print_parseable_fixits_replace ();
  test_print_parseable_fixits_bytes_vs_display_columns ();
  test_get_location_text ();
  test_num_digits ();
}

} // namespace diagnostics::selftest

#endif /* #if CHECKING_P */

} // namespace diagnostics

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

static void real_abort (void) ATTRIBUTE_NORETURN;

/* Really call the system 'abort'.  This has to go right at the end of
   this file, so that there are no functions after it that call abort
   and get the system abort instead of our macro.  */
#undef abort
static void
real_abort (void)
{
  abort ();
}
