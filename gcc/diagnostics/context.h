/* Declare diagnostics::context and related types.
   Copyright (C) 2000-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_CONTEXT_H
#define GCC_DIAGNOSTICS_CONTEXT_H

#include "lazily-created.h"
#include "unique-argv.h"
#include "diagnostics/option-classifier.h"
#include "diagnostics/option-id-manager.h"
#include "diagnostics/context-options.h"
#include "diagnostics/source-printing-options.h"
#include "diagnostics/column-options.h"
#include "diagnostics/counters.h"
#include "diagnostics/logging.h"

namespace diagnostics {

  namespace changes {
    class change_set;
  }

  namespace digraphs { class digraph; }

  namespace logical_locations {
    class manager;
  }

  class buffer;
  class client_data_hooks;
  class diagram;
  class sink;
    class text_sink;
  class metadata;

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

namespace diagnostics {

/*  Forward declarations.  */
class context;
class location_print_policy;
class source_print_policy;

typedef void (*text_starter_fn) (text_sink &,
				 const diagnostic_info *);

struct to_text;
struct to_html;

extern pretty_printer *get_printer (to_text &);

template <typename TextOrHtml>
using start_span_fn = void (*) (const location_print_policy &,
				TextOrHtml &text_or_html,
				expanded_location);

typedef void (*text_finalizer_fn) (text_sink &,
				   const diagnostic_info *,
				   enum kind);

/* A bundle of state for determining column numbers in diagnostics
   (tab stops, whether to start at 0 or 1, etc).
   Uses a file_cache to handle tabs.  */

class column_policy
{
public:
  column_policy (const context &dc);

  int converted_column (expanded_location s) const;

  label_text get_location_text (const expanded_location &s,
				bool show_column,
				bool colorize) const;

  int get_tabstop () const { return m_column_options.m_tabstop; }

private:
  file_cache &m_file_cache;
  column_options m_column_options;
};

/* A bundle of state for printing locations within diagnostics
   (e.g. "FILENAME:LINE:COLUMN"), to isolate the interactions between
   context and the start_span callbacks.  */

class location_print_policy
{
public:
  location_print_policy (const context &dc);
  location_print_policy (const text_sink &);

  bool show_column_p () const { return m_show_column; }

  const column_policy &
  get_column_policy () const { return m_column_policy; }

  void
  print_text_span_start (const context &dc,
			 pretty_printer &pp,
			 const expanded_location &exploc);

  void
  print_html_span_start (const context &dc,
			 xml::printer &xp,
			 const expanded_location &exploc);

private:
  column_policy m_column_policy;
  bool m_show_column;
};

/* Abstract base class for optionally supplying extra tags when writing
   out annotation labels in HTML output.  */

class html_label_writer
{
public:
  virtual ~html_label_writer () {}
  virtual void begin_label () = 0;
  virtual void end_label () = 0;
};

/* A bundle of state for printing source within a diagnostic,
   to isolate the interactions between context and the
   implementation of diagnostic_show_locus.  */

class source_print_policy
{
public:
  source_print_policy (const context &);
  source_print_policy (const context &,
		       const source_printing_options &);

  void
  print (pretty_printer &pp,
	 const rich_location &richloc,
	 enum kind diagnostic_kind,
	 source_effect_info *effect_info) const;

  void
  print_as_html (xml::printer &xp,
		 const rich_location &richloc,
		 enum kind diagnostic_kind,
		 source_effect_info *effect_info,
		 html_label_writer *label_writer) const;

  const source_printing_options &
  get_options () const { return m_options; }

  start_span_fn<to_text>
  get_text_start_span_fn () const { return m_text_start_span_cb; }

  start_span_fn<to_html>
  get_html_start_span_fn () const { return m_html_start_span_cb; }

  file_cache &
  get_file_cache () const { return m_file_cache; }

  enum diagnostics_escape_format
  get_escape_format () const
  {
    return m_escape_format;
  }

  text_art::theme *
  get_diagram_theme () const { return m_diagram_theme; }

  const column_policy &get_column_policy () const
  {
    return m_location_policy.get_column_policy ();
  }

  const location_print_policy &get_location_policy () const
  {
    return m_location_policy;
  }

private:
  const source_printing_options &m_options;
  location_print_policy m_location_policy;
  start_span_fn<to_text> m_text_start_span_cb;
  start_span_fn<to_html> m_html_start_span_cb;
  file_cache &m_file_cache;

  /* Other data copied from context.  */
  text_art::theme *m_diagram_theme;
  enum diagnostics_escape_format m_escape_format;
};

/* This class encapsulates the state of the diagnostics subsystem
   as a whole (either directly, or via owned objects of other classes, to
   avoid global variables).

   It has responsibility for:
   - being a central place for clients to report diagnostics
   - reporting those diagnostics to zero or more output sinks
     (e.g. text vs SARIF)
   - providing a "dump" member function for a debug dump of the state of
     the diagnostics subsytem
   - direct vs buffered diagnostics (see class diagnostics::buffer)
   - tracking the original argv of the program (for SARIF output)
   - crash-handling

   It delegates responsibility to various other classes:
   - the various output sinks (instances of diagnostics::sink
     subclasses)
   - formatting of messages (class pretty_printer)
   - an optional urlifier to inject URLs into formatted messages
   - counting the number of diagnostics reported of each kind
     (class diagnostics::counters)
   - calling out to a option_id_manager to determine if
     a particular warning is enabled or disabled
   - tracking pragmas that enable/disable warnings in a range of
     source code
   - a cache for use when quoting the user's source code (class file_cache)
   - a text_art::theme
   - a diagnostics::changes::change_set for generating patches from fix-it hints
   - diagnostics::client_data_hooks for metadata.

   Try to avoid adding new responsibilities to this class itself, to avoid
   the "blob" anti-pattern.  */

class context
{
public:
  /* Give access to m_text_callbacks.  */
  // FIXME: these need updating
  friend text_starter_fn &
  text_starter (context *dc);
  friend start_span_fn<to_text> &
  start_span (context *dc);
  friend text_finalizer_fn &
  text_finalizer (context *dc);

  friend class source_print_policy;
  friend class text_sink;
  friend class buffer;

  typedef void (*set_locations_callback_t) (const context &,
					    diagnostic_info *);

  void initialize (int n_opts);
  void color_init (int value);
  void urls_init (int value);
  void set_pretty_printer (std::unique_ptr<pretty_printer> pp);
  void refresh_output_sinks ();

  void finish ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  logging::logger *get_logger () { return m_logger; }

  bool execution_failed_p () const;

  void set_original_argv (unique_argv original_argv);
  const char * const *get_original_argv ()
  {
    return const_cast<const char * const *> (m_original_argv);
  }

  void set_set_locations_callback (set_locations_callback_t cb)
  {
    m_set_locations_cb = cb;
  }

  void
  initialize_input_context (diagnostic_input_charset_callback ccb,
			    bool should_skip_bom);

  void begin_group ();
  void end_group ();

  void push_nesting_level ();
  void pop_nesting_level ();
  void set_nesting_level (int new_level);

  bool warning_enabled_at (location_t loc, option_id opt_id);

  bool option_unspecified_p (option_id opt_id) const
  {
    return m_option_classifier.option_unspecified_p (opt_id);
  }

  bool emit_diagnostic_with_group (enum kind kind,
				   rich_location &richloc,
				   const metadata *metadata,
				   option_id opt_id,
				   const char *gmsgid, ...)
    ATTRIBUTE_GCC_DIAG(6,7);
  bool emit_diagnostic_with_group_va (enum kind kind,
				      rich_location &richloc,
				      const metadata *metadata,
				      option_id opt_id,
				      const char *gmsgid, va_list *ap)
    ATTRIBUTE_GCC_DIAG(6,0);

  bool report_diagnostic (diagnostic_info *);
  void report_verbatim (text_info &);

  /* Report a directed graph associated with the run as a whole
     to any sinks that support directed graphs.  */
  void
  report_global_digraph (const lazily_created<digraphs::digraph> &);

  enum kind
  classify_diagnostic (option_id opt_id,
		       enum kind new_kind,
		       location_t where)
  {
    logging::log_function_params
      (m_logger, "diagnostics::context::classify_diagnostics")
      .log_param_option_id ("option_id", opt_id)
      .log_param_kind ("new_kind", new_kind)
      .log_param_location_t ("where", where);
    logging::auto_inc_depth depth_sentinel (m_logger);

    return m_option_classifier.classify_diagnostic (this,
						    opt_id,
						    new_kind,
						    where);
  }

  void push_diagnostics (location_t where)
  {
    logging::log_function_params
      (m_logger, "diagnostics::context::push_diagnostics")
      .log_param_location_t ("where", where);
    logging::auto_inc_depth depth_sentinel (m_logger);

    m_option_classifier.push ();
  }
  void pop_diagnostics (location_t where)
  {
    logging::log_function_params
      (m_logger, "diagnostics::context::pop_diagnostics")
      .log_param_location_t ("where", where);
    logging::auto_inc_depth depth_sentinel (m_logger);

    m_option_classifier.pop (where);
  }

  void maybe_show_locus (const rich_location &richloc,
			 const source_printing_options &opts,
			 enum kind diagnostic_kind,
			 pretty_printer &pp,
			 source_effect_info *effect_info);
  void maybe_show_locus_as_html (const rich_location &richloc,
				 const source_printing_options &opts,
				 enum kind diagnostic_kind,
				 xml::printer &xp,
				 source_effect_info *effect_info,
				 html_label_writer *label_writer);

  void emit_diagram (const diagram &diag);

  /* Various setters for use by option-handling logic.  */
  void set_sink (std::unique_ptr<sink> sink_);
  void set_text_art_charset (enum diagnostic_text_art_charset charset);
  void set_client_data_hooks (std::unique_ptr<client_data_hooks> hooks);

  void push_owned_urlifier (std::unique_ptr<urlifier>);
  void push_borrowed_urlifier (const urlifier &);
  void pop_urlifier ();

  void initialize_fixits_change_set ();
  void set_warning_as_error_requested (bool val)
  {
    m_warning_as_error_requested = val;
  }
  void set_report_bug (bool val) { m_report_bug = val; }
  void set_extra_output_kind (enum diagnostics_extra_output_kind kind)
  {
    m_extra_output_kind = kind;
  }
  void set_show_cwe (bool val) { m_show_cwe = val;  }
  void set_show_rules (bool val) { m_show_rules = val; }
  void set_show_highlight_colors (bool val);
  void set_path_format (enum diagnostic_path_format val)
  {
    m_path_format = val;
  }
  void set_show_path_depths (bool val) { m_show_path_depths = val; }
  void set_show_option_requested (bool val) { m_show_option_requested = val; }
  void set_show_nesting (bool val);
  void set_show_nesting_locations (bool val);
  void set_show_nesting_levels (bool val);
  void set_max_errors (int val) { m_max_errors = val; }
  void set_escape_format (enum diagnostics_escape_format val)
  {
    m_escape_format = val;
  }

  void set_format_decoder (printer_fn format_decoder);
  void set_prefixing_rule (diagnostic_prefixing_rule_t rule);

  /* Various accessors.  */
  bool warning_as_error_requested_p () const
  {
    return m_warning_as_error_requested;
  }
  bool show_path_depths_p () const { return m_show_path_depths; }
  sink &get_sink (size_t idx) const;
  enum diagnostic_path_format get_path_format () const { return m_path_format; }
  enum diagnostics_escape_format get_escape_format () const
  {
    return m_escape_format;
  }

  file_cache &
  get_file_cache () const
  {
    gcc_assert (m_file_cache);
    return *m_file_cache;
  }

  changes::change_set *get_fixits_change_set () const
  {
    return m_fixits_change_set;
  }
  const client_data_hooks *get_client_data_hooks () const
  {
    return m_client_data_hooks;
  }

  const logical_locations::manager *
  get_logical_location_manager () const;

  const urlifier *get_urlifier () const;

  text_art::theme *get_diagram_theme () const { return m_diagrams.m_theme; }

  int &diagnostic_count (enum kind kind)
  {
    return m_diagnostic_counters.m_count_for_kind[static_cast<size_t> (kind)];
  }
  int diagnostic_count (enum kind kind) const
  {
    return m_diagnostic_counters.get_count (kind);
  }

  /* Option-related member functions.  */
  inline bool option_enabled_p (option_id opt_id) const
  {
    if (!m_option_id_mgr)
      return true;
    return m_option_id_mgr->option_enabled_p (opt_id);
  }

  inline char *make_option_name (option_id opt_id,
				 enum kind orig_diag_kind,
				 enum kind diag_kind) const
  {
    if (!m_option_id_mgr)
      return nullptr;
    return m_option_id_mgr->make_option_name (opt_id,
					      orig_diag_kind,
					      diag_kind);
  }

  inline char *make_option_url (option_id opt_id) const
  {
    if (!m_option_id_mgr)
      return nullptr;
    return m_option_id_mgr->make_option_url (opt_id);
  }

  void
  set_option_id_manager (std::unique_ptr<option_id_manager> option_id_mgr,
			 unsigned lang_mask);

  unsigned get_lang_mask () const
  {
    return m_lang_mask;
  }

  bool diagnostic_impl (rich_location *, const metadata *,
			option_id, const char *,
			va_list *, enum kind) ATTRIBUTE_GCC_DIAG(5,0);
  bool diagnostic_n_impl (rich_location *, const metadata *,
			  option_id, unsigned HOST_WIDE_INT,
			  const char *, const char *, va_list *,
			  enum kind) ATTRIBUTE_GCC_DIAG(7,0);

  int get_diagnostic_nesting_level () const
  {
    return m_diagnostic_groups.m_diagnostic_nesting_level;
  }

  char *build_indent_prefix () const;

  int
  pch_save (FILE *f)
  {
    return m_option_classifier.pch_save (f);
  }

  int
  pch_restore (FILE *f)
  {
    return m_option_classifier.pch_restore (f);
  }


  void set_diagnostic_buffer (buffer *);
  buffer *get_diagnostic_buffer () const
  {
    return m_diagnostic_buffer;
  }
  void clear_diagnostic_buffer (buffer &);
  void flush_diagnostic_buffer (buffer &);

  std::unique_ptr<pretty_printer> clone_printer () const
  {
    return m_reference_printer->clone ();
  }

  pretty_printer *get_reference_printer () const
  {
    return m_reference_printer;
  }

  void
  add_sink (std::unique_ptr<sink>);

  void remove_all_output_sinks ();

  bool supports_fnotice_on_stderr_p () const;

  /* Raise SIGABRT on any diagnostic of severity kind::error or higher.  */
  void
  set_abort_on_error (bool val)
  {
    m_abort_on_error = val;
  }

  /* Accessor for use in serialization, e.g. by C++ modules.  */
  auto &
  get_classification_history ()
  {
    return m_option_classifier.m_classification_history;
  }

  void set_main_input_filename (const char *filename);

  void
  set_permissive_option (option_id opt_permissive)
  {
    m_opt_permissive = opt_permissive;
  }

  void
  set_fatal_errors (bool fatal_errors)
  {
    m_fatal_errors = fatal_errors;
  }

  void
  set_internal_error_callback (void (*cb) (context *,
					   const char *,
					   va_list *))
  {
    m_internal_error = cb;
  }

  void
  set_adjust_diagnostic_info_callback (void (*cb) (const context &,
						   diagnostic_info *))
  {
    m_adjust_diagnostic_info = cb;
  }

  void
  inhibit_notes () { m_inhibit_notes_p = true; }

  source_printing_options &
  get_source_printing_options ()
  {
    return m_source_printing;
  }
  const source_printing_options &
  get_source_printing_options () const
  {
    return m_source_printing;
  }

  column_options &get_column_options () { return m_column_options; }
  const column_options &get_column_options () const { return m_column_options; }

  void set_caret_max_width (int value);

private:
  void error_recursion () ATTRIBUTE_NORETURN;

  bool diagnostic_enabled (diagnostic_info *diagnostic);

  void get_any_inlining_info (diagnostic_info *diagnostic);

  void check_max_errors (bool flush);
  void action_after_output (enum kind diag_kind);

  /* Data members.
     Ideally, all of these would be private.  */

private:
  /* A reference instance of pretty_printer created by the client
     and owned by the context.  Used for cloning when creating/adding
     output formats.
     Owned by the context; this would be a std::unique_ptr if
     context had a proper ctor.  */
  pretty_printer *m_reference_printer;

  /* Cache of source code.
     Owned by the context; this would be a std::unique_ptr if
     context had a proper ctor.  */
  file_cache *m_file_cache;

  /* The number of times we have issued diagnostics.  */
  counters m_diagnostic_counters;

  /* True if it has been requested that warnings be treated as errors.  */
  bool m_warning_as_error_requested;

  /* The number of option indexes that can be passed to warning() et
     al.  */
  int m_n_opts;

  /* The stack of sets of overridden diagnostic option severities.  */
  option_classifier m_option_classifier;

  /* True if we should print any CWE identifiers associated with
     diagnostics.  */
  bool m_show_cwe;

  /* True if we should print any rules associated with diagnostics.  */
  bool m_show_rules;

  /* How should diagnostics::paths::path objects be printed.  */
  enum diagnostic_path_format m_path_format;

  /* True if we should print stack depths when printing diagnostic paths.  */
  bool m_show_path_depths;

  /* True if we should print the command line option which controls
     each diagnostic, if known.  */
  bool m_show_option_requested;

  /* True if we should raise a SIGABRT on errors.  */
  bool m_abort_on_error;

public:
  /* True if we should show the column number on diagnostics.  */
  bool m_show_column;

  /* True if pedwarns are errors.  */
  bool m_pedantic_errors;

  /* True if permerrors are warnings.  */
  bool m_permissive;

private:
  /* The option to associate with turning permerrors into warnings,
     if any.  */
  option_id m_opt_permissive;

  /* True if errors are fatal.  */
  bool m_fatal_errors;

public:
  /* True if all warnings should be disabled.  */
  bool m_inhibit_warnings;

  /* True if warnings should be given in system headers.  */
  bool m_warn_system_headers;

private:
  /* Maximum number of errors to report.  */
  int m_max_errors;

  /* Client-supplied callbacks for use in text output.  */
  struct {
    /* This function is called before any message is printed out.  It is
       responsible for preparing message prefix and such.  For example, it
       might say:
       In file included from "/usr/local/include/curses.h:5:
       from "/home/gdr/src/nifty_printer.h:56:
       ...
    */
    text_starter_fn m_begin_diagnostic;

    /* This function is called by diagnostic_show_locus in between
       disjoint spans of source code, so that the context can print
       something to indicate that a new span of source code has begun.  */
    start_span_fn<to_text> m_text_start_span;
    start_span_fn<to_html> m_html_start_span;

    /* This function is called after the diagnostic message is printed.  */
    text_finalizer_fn m_end_diagnostic;
  } m_text_callbacks;

  /* Client hook to report an internal error.  */
  void (*m_internal_error) (context *, const char *, va_list *);

  /* Client hook to adjust properties of the given diagnostic that we're
     about to issue, such as its kind.  */
  void (*m_adjust_diagnostic_info)(const context &, diagnostic_info *);

  /* Owned by the context; this would be a std::unique_ptr if
     context had a proper ctor.  */
  option_id_manager *m_option_id_mgr;
  unsigned m_lang_mask;

  /* A stack of optional hooks for adding URLs to quoted text strings in
     diagnostics.  Only used for the main diagnostic message.
     Typically a single one owner by the context, but can be temporarily
     overridden by a borrowed urlifier (e.g. on-stack).  */
  struct urlifier_stack_node
  {
    urlifier *m_urlifier;
    bool m_owned;
  };
  auto_vec<urlifier_stack_node> *m_urlifier_stack;

public:
  /* Auxiliary data for client.  */
  void *m_client_aux_data;

  /* Used to detect that the last caret was printed at the same location.  */
  location_t m_last_location;

private:
  int m_lock;

  bool m_inhibit_notes_p;

  source_printing_options m_source_printing;
  column_options m_column_options;

  /* True if -freport-bug option is used.  */
  bool m_report_bug;

  /* Used to specify additional diagnostic output to be emitted after the
     rest of the diagnostic.  This is for implementing
     -fdiagnostics-parseable-fixits and GCC_EXTRA_DIAGNOSTIC_OUTPUT.  */
  enum diagnostics_extra_output_kind m_extra_output_kind;

  /* How should non-ASCII/non-printable bytes be escaped when
     a diagnostic suggests escaping the source code on output.  */
  enum diagnostics_escape_format m_escape_format;

  /* If non-NULL, a diagnostics::changes::change_set to which fix-it hints
     should be applied, for generating patches.
     Owned by the context; this would be a std::unique_ptr if
     context had a proper ctor.  */
  changes::change_set *m_fixits_change_set;

  /* Fields relating to diagnostic groups.  */
  struct {
    /* How many diagnostic_group instances are currently alive.  */
    int m_group_nesting_depth;

    /* How many nesting levels have been pushed within this group.  */
    int m_diagnostic_nesting_level;

    /* How many diagnostics have been emitted since the bottommost
       diagnostic_group was pushed.  */
    int m_emission_count;

    /* The "group+diagnostic" nesting depth from which to inhibit notes.  */
    int m_inhibiting_notes_from;
  } m_diagnostic_groups;

  void inhibit_notes_in_group (bool inhibit = true);
  bool notes_inhibited_in_group () const;

  /* The various sinks to which diagnostics are to be outputted
     (text vs structured formats such as SARIF).
     The sinks are owned by the context; this would be a
     std::vector<std::unique_ptr> if context had a
     proper ctor.  */
  auto_vec<sink *> m_sinks;

  /* Callback to set the locations of call sites along the inlining
     stack corresponding to a diagnostic location.  Needed to traverse
     the BLOCK_SUPERCONTEXT() chain hanging off the LOCATION_BLOCK()
     of a diagnostic's location.  */
  set_locations_callback_t m_set_locations_cb;

  /* A bundle of hooks for providing data to the context about its client
     e.g. version information, plugins, etc.
     Used by SARIF output to give metadata about the client that's
     producing diagnostics.
     Owned by the context; this would be a std::unique_ptr if
     context had a proper ctor.  */
  client_data_hooks *m_client_data_hooks;

  /* Support for diagrams.  */
  struct
  {
    /* Theme to use when generating diagrams.
       Can be NULL (if text art is disabled).
       Owned by the context; this would be a std::unique_ptr if
       context had a proper ctor.  */
    text_art::theme *m_theme;

  } m_diagrams;

  /* Owned by the context.  */
  char **m_original_argv;

  /* Borrowed pointer to the active diagnostics::buffer, if any.
     If null (the default), then diagnostics that are reported to the
     context are immediately issued to the output format.
     If non-null, then diagnostics that are reported to the context
     are buffered in the buffer, and may be issued to the output format
     later (if the buffer is flushed), moved to other buffers, or
     discarded (if the buffer is cleared).  */
  buffer *m_diagnostic_buffer;

  /* Owned by the context.
     Debugging option: if non-NULL, report information to the logger
     on what the context is doing.  */
  logging::logger *m_logger;
};

/* Client supplied function to announce a diagnostic
   (for text-based diagnostic output).  */
inline text_starter_fn &
text_starter (context *dc)
{
  return dc->m_text_callbacks.m_begin_diagnostic;
}

/* Client supplied function called between disjoint spans of source code,
   so that the context can print
   something to indicate that a new span of source code has begun.  */
inline start_span_fn<to_text> &
start_span (context *dc)
{
  return dc->m_text_callbacks.m_text_start_span;
}

/* Client supplied function called after a diagnostic message is
   displayed (for text-based diagnostic output).  */
inline text_finalizer_fn &
text_finalizer (context *dc)
{
  return dc->m_text_callbacks.m_end_diagnostic;
}

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_CONTEXT_H */
