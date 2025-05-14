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

namespace text_art
{
  class theme;
} // namespace text_art

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

  /* JSON-based output, to stderr.  */
  DIAGNOSTICS_OUTPUT_FORMAT_JSON_STDERR,

  /* JSON-based output, to a file.  */
  DIAGNOSTICS_OUTPUT_FORMAT_JSON_FILE,

  /* SARIF-based output, as JSON to stderr.  */
  DIAGNOSTICS_OUTPUT_FORMAT_SARIF_STDERR,

  /* SARIF-based output, to a JSON file.  */
  DIAGNOSTICS_OUTPUT_FORMAT_SARIF_FILE
};

/* An enum for controlling how diagnostic_paths should be printed.  */
enum diagnostic_path_format
{
  /* Don't print diagnostic_paths.  */
  DPF_NONE,

  /* Print diagnostic_paths by emitting a separate "note" for every event
     in the path.  */
  DPF_SEPARATE_EVENTS,

  /* Print diagnostic_paths by consolidating events together where they
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

/* A diagnostic is described by the MESSAGE to send, the FILE and LINE of
   its context and its KIND (ice, error, warning, note, ...)  See complete
   list in diagnostic.def.  */
struct diagnostic_info
{
  diagnostic_info ()
    : message (), richloc (), metadata (), x_data (), kind (), option_id (),
      m_iinfo ()
  { }

  /* Text to be formatted.  */
  text_info message;

  /* The location at which the diagnostic is to be reported.  */
  rich_location *richloc;

  /* An optional bundle of metadata associated with the diagnostic
     (or NULL).  */
  const diagnostic_metadata *metadata;

  /* Auxiliary data for client.  */
  void *x_data;
  /* The kind of diagnostic it is about.  */
  diagnostic_t kind;
  /* Which OPT_* directly controls this diagnostic.  */
  diagnostic_option_id option_id;

  /* Inlining context containing locations for each call site along
     the inlining stack.  */
  struct inlining_info
  {
    /* Locations along the inlining stack.  */
    auto_vec<location_t, 8> m_ilocs;
    /* The abstract origin of the location.  */
    void *m_ao;
    /* Set if every M_ILOCS element is in a system header.  */
    bool m_allsyslocs;
  } m_iinfo;
};

/*  Forward declarations.  */
class diagnostic_location_print_policy;
class diagnostic_source_print_policy;

typedef void (*diagnostic_text_starter_fn) (diagnostic_text_output_format &,
					    const diagnostic_info *);

typedef void
(*diagnostic_start_span_fn) (const diagnostic_location_print_policy &,
			     pretty_printer *,
			     expanded_location);

typedef void (*diagnostic_text_finalizer_fn) (diagnostic_text_output_format &,
					      const diagnostic_info *,
					      diagnostic_t);

/* Abstract base class for the diagnostic subsystem to make queries
   about command-line options.  */

class diagnostic_option_manager
{
public:
  virtual ~diagnostic_option_manager () {}

  /* Return 1 if option OPTION_ID is enabled, 0 if it is disabled,
     or -1 if it isn't a simple on-off switch
     (or if the value is unknown, typically set later in target).  */
  virtual int option_enabled_p (diagnostic_option_id option_id) const = 0;

  /* Return malloced memory for the name of the option OPTION_ID
     which enabled a diagnostic, originally of type ORIG_DIAG_KIND but
     possibly converted to DIAG_KIND by options such as -Werror.
     May return NULL if no name is to be printed.
     May be passed 0 as well as the index of a particular option.  */
  virtual char *make_option_name (diagnostic_option_id option_id,
				  diagnostic_t orig_diag_kind,
				  diagnostic_t diag_kind) const = 0;

  /* Return malloced memory for a URL describing the option that controls
     a diagnostic.
     May return NULL if no URL is available.
     May be passed 0 as well as the index of a particular option.  */
  virtual char *make_option_url (diagnostic_option_id option_id) const = 0;
};

class edit_context;
class diagnostic_client_data_hooks;
class logical_location_manager;
class diagnostic_diagram;
class diagnostic_source_effect_info;
class diagnostic_output_format;
  class diagnostic_text_output_format;
class diagnostic_buffer;

/* A stack of sets of classifications: each entry in the stack is
   a mapping from option index to diagnostic severity that can be changed
   via pragmas.  The stack can be pushed and popped.  */

class diagnostic_option_classifier
{
public:
  void init (int n_opts);
  void fini ();

  /* Save all diagnostic classifications in a stack.  */
  void push ();

  /* Restore the topmost classification set off the stack.  If the stack
     is empty, revert to the state based on command line parameters.  */
  void pop (location_t where);

  bool option_unspecified_p (diagnostic_option_id option_id) const
  {
    return get_current_override (option_id) == DK_UNSPECIFIED;
  }

  diagnostic_t get_current_override (diagnostic_option_id option_id) const
  {
    gcc_assert (option_id.m_idx < m_n_opts);
    return m_classify_diagnostic[option_id.m_idx];
  }

  diagnostic_t
  classify_diagnostic (const diagnostic_context *context,
		       diagnostic_option_id option_id,
		       diagnostic_t new_kind,
		       location_t where);

  diagnostic_t
  update_effective_level_from_pragmas (diagnostic_info *diagnostic) const;

  int pch_save (FILE *);
  int pch_restore (FILE *);

private:
  /* Each time a diagnostic's classification is changed with a pragma,
     we record the change and the location of the change in an array of
     these structs.  */
  struct diagnostic_classification_change_t
  {
    location_t location;

    /* For DK_POP, this is the index of the corresponding push (as stored
       in m_push_list).
       Otherwise, this is an option index.  */
    int option;

    diagnostic_t kind;
  };

  int m_n_opts;

  /* For each option index that can be passed to warning() et al
     (OPT_* from options.h when using this code with the core GCC
     options), this array may contain a new kind that the diagnostic
     should be changed to before reporting, or DK_UNSPECIFIED to leave
     it as the reported kind, or DK_IGNORED to not report it at
     all.  */
  diagnostic_t *m_classify_diagnostic;

  /* History of all changes to the classifications above.  This list
     is stored in location-order, so we can search it, either
     binary-wise or end-to-front, to find the most recent
     classification for a given diagnostic, given the location of the
     diagnostic.  */
  vec<diagnostic_classification_change_t> m_classification_history;

  /* For pragma push/pop.  */
  vec<int> m_push_list;
};

/* A bundle of options relating to printing the user's source code
   (potentially with a margin, underlining, labels, etc).  */

struct diagnostic_source_printing_options
{
  /* True if we should print the source line with a caret indicating
     the location.
     Corresponds to -fdiagnostics-show-caret.  */
  bool enabled;

  /* Maximum width of the source line printed.  */
  int max_width;

  /* Character used at the caret when printing source locations.  */
  char caret_chars[rich_location::STATICALLY_ALLOCATED_RANGES];

  /* When printing source code, should the characters at carets and ranges
     be colorized? (assuming colorization is on at all).
     This should be true for frontends that generate range information
     (so that the ranges of code are colorized),
     and false for frontends that merely specify points within the
     source code (to avoid e.g. colorizing just the first character in
     a token, which would look strange).  */
  bool colorize_source_p;

  /* When printing source code, should labelled ranges be printed?
     Corresponds to -fdiagnostics-show-labels.  */
  bool show_labels_p;

  /* When printing source code, should there be a left-hand margin
     showing line numbers?
     Corresponds to -fdiagnostics-show-line-numbers.  */
  bool show_line_numbers_p;

  /* If printing source code, what should the minimum width of the margin
     be?  Line numbers will be right-aligned, and padded to this width.
     Corresponds to -fdiagnostics-minimum-margin-width=VALUE.  */
  int min_margin_width;

  /* Usable by plugins; if true, print a debugging ruler above the
     source output.  */
  bool show_ruler_p;

  /* When printing events in an inline path, should we print lines
     visualizing links between related events (e.g. for CFG paths)?
     Corresponds to -fdiagnostics-show-event-links.  */
  bool show_event_links_p;
};

/* A bundle of state for determining column numbers in diagnostics
   (tab stops, whether to start at 0 or 1, etc).
   Uses a file_cache to handle tabs.  */

class diagnostic_column_policy
{
public:
  diagnostic_column_policy (const diagnostic_context &dc);

  int converted_column (expanded_location s) const;

  label_text get_location_text (const expanded_location &s,
				bool show_column,
				bool colorize) const;

  int get_tabstop () const { return m_tabstop; }

private:
  file_cache &m_file_cache;
  enum diagnostics_column_unit m_column_unit;
  int m_column_origin;
  int m_tabstop;
};

/* A bundle of state for printing locations within diagnostics
   (e.g. "FILENAME:LINE:COLUMN"), to isolate the interactions between
   diagnostic_context and the start_span callbacks.  */

class diagnostic_location_print_policy
{
public:
  diagnostic_location_print_policy (const diagnostic_context &dc);
  diagnostic_location_print_policy (const diagnostic_text_output_format &);

  bool show_column_p () const { return m_show_column; }

  const diagnostic_column_policy &
  get_column_policy () const { return m_column_policy; }

private:
  diagnostic_column_policy m_column_policy;
  bool m_show_column;
};

/* A bundle of state for printing source within a diagnostic,
   to isolate the interactions between diagnostic_context and the
   implementation of diagnostic_show_locus.  */

class diagnostic_source_print_policy
{
public:
  diagnostic_source_print_policy (const diagnostic_context &);
  diagnostic_source_print_policy (const diagnostic_context &,
				  const diagnostic_source_printing_options &);

  void
  print (pretty_printer &pp,
	 const rich_location &richloc,
	 diagnostic_t diagnostic_kind,
	 diagnostic_source_effect_info *effect_info) const;

  const diagnostic_source_printing_options &
  get_options () const { return m_options; }

  diagnostic_start_span_fn
  get_start_span_fn () const { return m_start_span_cb; }

  file_cache &
  get_file_cache () const { return m_file_cache; }

  enum diagnostics_escape_format
  get_escape_format () const
  {
    return m_escape_format;
  }

  text_art::theme *
  get_diagram_theme () const { return m_diagram_theme; }

  const diagnostic_column_policy &get_column_policy () const
  {
    return m_location_policy.get_column_policy ();
  }

  const diagnostic_location_print_policy &get_location_policy () const
  {
    return m_location_policy;
  }

private:
  const diagnostic_source_printing_options &m_options;
  class diagnostic_location_print_policy m_location_policy;
  diagnostic_start_span_fn m_start_span_cb;
  file_cache &m_file_cache;

  /* Other data copied from diagnostic_context.  */
  text_art::theme *m_diagram_theme;
  enum diagnostics_escape_format m_escape_format;
};

/* A collection of counters of diagnostics, per-kind
   (e.g. "3 errors and 1 warning"), for use by both diagnostic_context
   and by diagnostic_buffer.  */

struct diagnostic_counters
{
  diagnostic_counters ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  int get_count (diagnostic_t kind) const { return m_count_for_kind[kind]; }

  void move_to (diagnostic_counters &dest);
  void clear ();

  int m_count_for_kind[DK_LAST_DIAGNOSTIC_KIND];
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
   - direct vs buffered diagnostics (see class diagnostic_buffer)
   - tracking the original argv of the program (for SARIF output)
   - crash-handling

   It delegates responsibility to various other classes:
   - the various output sinks (instances of diagnostic_output_format
     subclasses)
   - formatting of messages (class pretty_printer)
   - an optional urlifier to inject URLs into formatted messages
   - counting the number of diagnostics reported of each kind
     (class diagnostic_counters)
   - calling out to a diagnostic_option_manager to determine if
     a particular warning is enabled or disabled
   - tracking pragmas that enable/disable warnings in a range of
     source code
   - a cache for use when quoting the user's source code (class file_cache)
   - a text_art::theme
   - an edit_context for generating patches from fix-it hints
   - diagnostic_client_data_hooks for metadata.

   Try to avoid adding new responsibilities to this class itself, to avoid
   the "blob" anti-pattern.  */

class diagnostic_context
{
public:
  /* Give access to m_text_callbacks.  */
  friend diagnostic_text_starter_fn &
  diagnostic_text_starter (diagnostic_context *context);
  friend diagnostic_start_span_fn &
  diagnostic_start_span (diagnostic_context *context);
  friend diagnostic_text_finalizer_fn &
  diagnostic_text_finalizer (diagnostic_context *context);

  friend class diagnostic_source_print_policy;
  friend class diagnostic_text_output_format;
  friend class diagnostic_buffer;

  typedef void (*set_locations_callback_t) (diagnostic_context *,
					    diagnostic_info *);

  void initialize (int n_opts);
  void color_init (int value);
  void urls_init (int value);
  void set_pretty_printer (std::unique_ptr<pretty_printer> pp);
  void refresh_output_sinks ();

  void finish ();

  void dump (FILE *out) const;
  void DEBUG_FUNCTION dump () const { dump (stderr); }

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

  bool warning_enabled_at (location_t loc, diagnostic_option_id option_id);

  bool option_unspecified_p (diagnostic_option_id option_id) const
  {
    return m_option_classifier.option_unspecified_p (option_id);
  }

  bool emit_diagnostic_with_group (diagnostic_t kind,
				   rich_location &richloc,
				   const diagnostic_metadata *metadata,
				   diagnostic_option_id option_id,
				   const char *gmsgid, ...)
    ATTRIBUTE_GCC_DIAG(6,7);
  bool emit_diagnostic_with_group_va (diagnostic_t kind,
				      rich_location &richloc,
				      const diagnostic_metadata *metadata,
				      diagnostic_option_id option_id,
				      const char *gmsgid, va_list *ap)
    ATTRIBUTE_GCC_DIAG(6,0);

  bool report_diagnostic (diagnostic_info *);
  void report_verbatim (text_info &);

  diagnostic_t
  classify_diagnostic (diagnostic_option_id option_id,
		       diagnostic_t new_kind,
		       location_t where)
  {
    return m_option_classifier.classify_diagnostic (this,
						    option_id,
						    new_kind,
						    where);
  }

  void push_diagnostics (location_t where ATTRIBUTE_UNUSED)
  {
    m_option_classifier.push ();
  }
  void pop_diagnostics (location_t where)
  {
    m_option_classifier.pop (where);
  }

  void maybe_show_locus (const rich_location &richloc,
			 const diagnostic_source_printing_options &opts,
			 diagnostic_t diagnostic_kind,
			 pretty_printer &pp,
			 diagnostic_source_effect_info *effect_info);

  void emit_diagram (const diagnostic_diagram &diagram);

  /* Various setters for use by option-handling logic.  */
  void set_output_format (std::unique_ptr<diagnostic_output_format> output_format);
  void set_text_art_charset (enum diagnostic_text_art_charset charset);
  void set_client_data_hooks (std::unique_ptr<diagnostic_client_data_hooks> hooks);

  void push_owned_urlifier (std::unique_ptr<urlifier>);
  void push_borrowed_urlifier (const urlifier &);
  void pop_urlifier ();

  void create_edit_context ();
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
  diagnostic_output_format &get_output_format (size_t idx) const;
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

  edit_context *get_edit_context () const
  {
    return m_edit_context_ptr;
  }
  const diagnostic_client_data_hooks *get_client_data_hooks () const
  {
    return m_client_data_hooks;
  }

  const logical_location_manager *
  get_logical_location_manager () const;

  const urlifier *get_urlifier () const;

  text_art::theme *get_diagram_theme () const { return m_diagrams.m_theme; }

  int &diagnostic_count (diagnostic_t kind)
  {
    return m_diagnostic_counters.m_count_for_kind[kind];
  }
  int diagnostic_count (diagnostic_t kind) const
  {
    return m_diagnostic_counters.get_count (kind);
  }

  /* Option-related member functions.  */
  inline bool option_enabled_p (diagnostic_option_id option_id) const
  {
    if (!m_option_mgr)
      return true;
    return m_option_mgr->option_enabled_p (option_id);
  }

  inline char *make_option_name (diagnostic_option_id option_id,
				 diagnostic_t orig_diag_kind,
				 diagnostic_t diag_kind) const
  {
    if (!m_option_mgr)
      return nullptr;
    return m_option_mgr->make_option_name (option_id,
					   orig_diag_kind,
					   diag_kind);
  }

  inline char *make_option_url (diagnostic_option_id option_id) const
  {
    if (!m_option_mgr)
      return nullptr;
    return m_option_mgr->make_option_url (option_id);
  }

  void
  set_option_manager (std::unique_ptr<diagnostic_option_manager> mgr,
		      unsigned lang_mask);

  unsigned get_lang_mask () const
  {
    return m_lang_mask;
  }

  bool diagnostic_impl (rich_location *, const diagnostic_metadata *,
			diagnostic_option_id, const char *,
			va_list *, diagnostic_t) ATTRIBUTE_GCC_DIAG(5,0);
  bool diagnostic_n_impl (rich_location *, const diagnostic_metadata *,
			  diagnostic_option_id, unsigned HOST_WIDE_INT,
			  const char *, const char *, va_list *,
			  diagnostic_t) ATTRIBUTE_GCC_DIAG(7,0);

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


  void set_diagnostic_buffer (diagnostic_buffer *);
  diagnostic_buffer *get_diagnostic_buffer () const
  {
    return m_diagnostic_buffer;
  }
  void clear_diagnostic_buffer (diagnostic_buffer &);
  void flush_diagnostic_buffer (diagnostic_buffer &);

  std::unique_ptr<pretty_printer> clone_printer () const
  {
    return m_reference_printer->clone ();
  }

  pretty_printer *get_reference_printer () const
  {
    return m_reference_printer;
  }

  void
  add_sink (std::unique_ptr<diagnostic_output_format>);

  void remove_all_output_sinks ();

  bool supports_fnotice_on_stderr_p () const;

  /* Raise SIGABRT on any diagnostic of severity DK_ERROR or higher.  */
  void
  set_abort_on_error (bool val)
  {
    m_abort_on_error = val;
  }

private:
  void error_recursion () ATTRIBUTE_NORETURN;

  bool diagnostic_enabled (diagnostic_info *diagnostic);

  void get_any_inlining_info (diagnostic_info *diagnostic);

  void check_max_errors (bool flush);
  void action_after_output (diagnostic_t diag_kind);

  /* Data members.
     Ideally, all of these would be private.  */

private:
  /* A reference instance of pretty_printer created by the client
     and owned by the context.  Used for cloning when creating/adding
     output formats.
     Owned by the context; this would be a std::unique_ptr if
     diagnostic_context had a proper ctor.  */
  pretty_printer *m_reference_printer;

  /* Cache of source code.
     Owned by the context; this would be a std::unique_ptr if
     diagnostic_context had a proper ctor.  */
  file_cache *m_file_cache;

  /* The number of times we have issued diagnostics.  */
  diagnostic_counters m_diagnostic_counters;

  /* True if it has been requested that warnings be treated as errors.  */
  bool m_warning_as_error_requested;

  /* The number of option indexes that can be passed to warning() et
     al.  */
  int m_n_opts;

  /* The stack of sets of overridden diagnostic option severities.  */
  diagnostic_option_classifier m_option_classifier;

  /* True if we should print any CWE identifiers associated with
     diagnostics.  */
  bool m_show_cwe;

  /* True if we should print any rules associated with diagnostics.  */
  bool m_show_rules;

  /* How should diagnostic_path objects be printed.  */
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

  /* The option to associate with turning permerrors into warnings,
     if any.  */
  diagnostic_option_id m_opt_permissive;

  /* True if errors are fatal.  */
  bool m_fatal_errors;

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
    diagnostic_text_starter_fn m_begin_diagnostic;

    /* This function is called by diagnostic_show_locus in between
       disjoint spans of source code, so that the context can print
       something to indicate that a new span of source code has begun.  */
    diagnostic_start_span_fn m_start_span;

    /* This function is called after the diagnostic message is printed.  */
    diagnostic_text_finalizer_fn m_end_diagnostic;
  } m_text_callbacks;

public:
  /* Client hook to report an internal error.  */
  void (*m_internal_error) (diagnostic_context *, const char *, va_list *);

  /* Client hook to adjust properties of the given diagnostic that we're
     about to issue, such as its kind.  */
  void (*m_adjust_diagnostic_info)(diagnostic_context *, diagnostic_info *);

private:
  /* Owned by the context; this would be a std::unique_ptr if
     diagnostic_context had a proper ctor.  */
  diagnostic_option_manager *m_option_mgr;
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

public:
  bool m_inhibit_notes_p;

  diagnostic_source_printing_options m_source_printing;

private:
  /* True if -freport-bug option is used.  */
  bool m_report_bug;

  /* Used to specify additional diagnostic output to be emitted after the
     rest of the diagnostic.  This is for implementing
     -fdiagnostics-parseable-fixits and GCC_EXTRA_DIAGNOSTIC_OUTPUT.  */
  enum diagnostics_extra_output_kind m_extra_output_kind;

public:
  /* What units to use when outputting the column number.  */
  enum diagnostics_column_unit m_column_unit;

  /* The origin for the column number (1-based or 0-based typically).  */
  int m_column_origin;

  /* The size of the tabstop for tab expansion.  */
  int m_tabstop;

private:
  /* How should non-ASCII/non-printable bytes be escaped when
     a diagnostic suggests escaping the source code on output.  */
  enum diagnostics_escape_format m_escape_format;

  /* If non-NULL, an edit_context to which fix-it hints should be
     applied, for generating patches.
     Owned by the context; this would be a std::unique_ptr if
     diagnostic_context had a proper ctor.  */
  edit_context *m_edit_context_ptr;

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
     std::vector<std::unique_ptr> if diagnostic_context had a
     proper ctor.  */
  auto_vec<diagnostic_output_format *> m_output_sinks;

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
     diagnostic_context had a proper ctor.  */
  diagnostic_client_data_hooks *m_client_data_hooks;

  /* Support for diagrams.  */
  struct
  {
    /* Theme to use when generating diagrams.
       Can be NULL (if text art is disabled).
       Owned by the context; this would be a std::unique_ptr if
       diagnostic_context had a proper ctor.  */
    text_art::theme *m_theme;

  } m_diagrams;

  /* Owned by the context.  */
  char **m_original_argv;

  /* Borrowed pointer to the active diagnostic_buffer, if any.
     If null (the default), then diagnostics that are reported to the
     context are immediately issued to the output format.
     If non-null, then diagnostics that are reported to the context
     are buffered in the buffer, and may be issued to the output format
     later (if the buffer is flushed), moved to other buffers, or
     discarded (if the buffer is cleared).  */
  diagnostic_buffer *m_diagnostic_buffer;
};

inline void
diagnostic_inhibit_notes (diagnostic_context * context)
{
  context->m_inhibit_notes_p = true;
}


/* Client supplied function to announce a diagnostic
   (for text-based diagnostic output).  */
inline diagnostic_text_starter_fn &
diagnostic_text_starter (diagnostic_context *context)
{
  return context->m_text_callbacks.m_begin_diagnostic;
}

/* Client supplied function called between disjoint spans of source code,
   so that the context can print
   something to indicate that a new span of source code has begun.  */
inline diagnostic_start_span_fn &
diagnostic_start_span (diagnostic_context *context)
{
  return context->m_text_callbacks.m_start_span;
}

/* Client supplied function called after a diagnostic message is
   displayed (for text-based diagnostic output).  */
inline diagnostic_text_finalizer_fn &
diagnostic_text_finalizer (diagnostic_context *context)
{
  return context->m_text_callbacks.m_end_diagnostic;
}

/* Extension hooks for client.  */
#define diagnostic_context_auxiliary_data(DC) (DC)->m_client_aux_data
#define diagnostic_info_auxiliary_data(DI) (DI)->x_data

/* This diagnostic_context is used by front-ends that directly output
   diagnostic messages without going through `error', `warning',
   and similar functions.  */
extern diagnostic_context *global_dc;

/* The number of errors that have been issued so far.  Ideally, these
   would take a diagnostic_context as an argument.  */
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
  info->option_id = option_id;
}

/* Diagnostic related functions.  */

inline void
diagnostic_initialize (diagnostic_context *context, int n_opts)
{
  context->initialize (n_opts);
}

inline void
diagnostic_color_init (diagnostic_context *context, int value = -1)
{
  context->color_init (value);
}

inline void
diagnostic_urls_init (diagnostic_context *context, int value = -1)
{
  context->urls_init (value);
}

inline void
diagnostic_finish (diagnostic_context *context)
{
  context->finish ();
}

inline void
diagnostic_show_locus (diagnostic_context *context,
		       const diagnostic_source_printing_options &opts,
		       rich_location *richloc,
		       diagnostic_t diagnostic_kind,
		       pretty_printer *pp,
		       diagnostic_source_effect_info *effect_info = nullptr)
{
  gcc_assert (context);
  gcc_assert (richloc);
  gcc_assert (pp);
  context->maybe_show_locus (*richloc, opts, diagnostic_kind, *pp, effect_info);
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
diagnostic_initialize_input_context (diagnostic_context *context,
				     diagnostic_input_charset_callback ccb,
				     bool should_skip_bom)
{
  context->initialize_input_context (ccb, should_skip_bom);
}

/* Force diagnostics controlled by OPTIDX to be kind KIND.  */
inline diagnostic_t
diagnostic_classify_diagnostic (diagnostic_context *context,
				diagnostic_option_id option_id,
				diagnostic_t kind,
				location_t where)
{
  return context->classify_diagnostic (option_id, kind, where);
}

inline void
diagnostic_push_diagnostics (diagnostic_context *context,
			     location_t where)
{
  context->push_diagnostics (where);
}
inline void
diagnostic_pop_diagnostics (diagnostic_context *context,
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
diagnostic_report_diagnostic (diagnostic_context *context,
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
void default_diagnostic_text_starter (diagnostic_text_output_format &,
				      const diagnostic_info *);
void default_diagnostic_start_span_fn (const diagnostic_location_print_policy &,
				       pretty_printer *,
				       expanded_location);
void default_diagnostic_text_finalizer (diagnostic_text_output_format &,
					const diagnostic_info *,
					diagnostic_t);
void diagnostic_set_caret_max_width (diagnostic_context *context, int value);

int get_terminal_width (void);

/* Return the location associated to this diagnostic. Parameter WHICH
   specifies which location. By default, expand the first one.  */

inline location_t
diagnostic_location (const diagnostic_info * diagnostic, int which = 0)
{
  return diagnostic->message.get_location (which);
}

/* Return the number of locations to be printed in DIAGNOSTIC.  */

inline unsigned int
diagnostic_num_locations (const diagnostic_info * diagnostic)
{
  return diagnostic->message.m_richloc->get_num_locations ();
}

/* Expand the location of this diagnostic. Use this function for
   consistency.  Parameter WHICH specifies which location. By default,
   expand the first one.  */

inline expanded_location
diagnostic_expand_location (const diagnostic_info * diagnostic, int which = 0)
{
  return diagnostic->richloc->get_expanded_location (which);
}

/* This is somehow the right-side margin of a caret line, that is, we
   print at least these many characters after the position pointed at
   by the caret.  */
const int CARET_LINE_MARGIN = 10;

/* Return true if the two locations can be represented within the same
   caret line.  This is used to build a prefix and also to determine
   whether to print one or two caret lines.  */

inline bool
diagnostic_same_line (const diagnostic_context *context,
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

const char *maybe_line_and_column (int line, int col);

#endif /* ! GCC_DIAGNOSTIC_H */
