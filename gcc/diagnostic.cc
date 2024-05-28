/* Language-independent diagnostic subroutines for the GNU Compiler Collection
   Copyright (C) 1999-2024 Free Software Foundation, Inc.
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
#include "diagnostic-color.h"
#include "diagnostic-url.h"
#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "diagnostic-client-data-hooks.h"
#include "diagnostic-diagram.h"
#include "edit-context.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "opts.h"
#include "cpplib.h"
#include "text-art/theme.h"
#include "pretty-print-urlifier.h"

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

#define pedantic_warning_kind(DC)			\
  ((DC)->m_pedantic_errors ? DK_ERROR : DK_WARNING)
#define permissive_error_kind(DC) ((DC)->m_permissive ? DK_WARNING : DK_ERROR)
#define permissive_error_option(DC) ((DC)->m_opt_permissive)

/* Prototypes.  */
static bool diagnostic_impl (rich_location *, const diagnostic_metadata *,
			     int, const char *,
			     va_list *, diagnostic_t) ATTRIBUTE_GCC_DIAG(4,0);
static bool diagnostic_n_impl (rich_location *, const diagnostic_metadata *,
			       int, unsigned HOST_WIDE_INT,
			       const char *, const char *, va_list *,
			       diagnostic_t) ATTRIBUTE_GCC_DIAG(6,0);

static void real_abort (void) ATTRIBUTE_NORETURN;

/* Name of program invoked, sans directories.  */

const char *progname;

/* A diagnostic_context surrogate for stderr.  */
static diagnostic_context global_diagnostic_context;
diagnostic_context *global_dc = &global_diagnostic_context;

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

/* Same as diagnostic_build_prefix, but only the source FILE is given.  */
char *
file_name_as_prefix (diagnostic_context *context, const char *f)
{
  const char *locus_cs
    = colorize_start (pp_show_color (context->printer), "locus");
  const char *locus_ce = colorize_stop (pp_show_color (context->printer));
  return build_message_string ("%s%s:%s ", locus_cs, f, locus_ce);
}



/* Return the value of the getenv("COLUMNS") as an integer. If the
   value is not set to a positive integer, use ioctl to get the
   terminal width. If it fails, return INT_MAX.  */
int
get_terminal_width (void)
{
  const char * s = getenv ("COLUMNS");
  if (s != NULL) {
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

/* Set caret_max_width to value.  */
void
diagnostic_set_caret_max_width (diagnostic_context *context, int value)
{
  /* One minus to account for the leading empty space.  */
  value = value ? value - 1 
    : (isatty (fileno (pp_buffer (context->printer)->stream))
       ? get_terminal_width () - 1: INT_MAX);
  
  if (value <= 0) 
    value = INT_MAX;

  context->m_source_printing.max_width = value;
}

void
diagnostic_option_classifier::init (int n_opts)
{
  m_n_opts = n_opts;
  m_classify_diagnostic = XNEWVEC (diagnostic_t, n_opts);
  for (int i = 0; i < n_opts; i++)
    m_classify_diagnostic[i] = DK_UNSPECIFIED;
  m_push_list = nullptr;
  m_n_push = 0;
}

void
diagnostic_option_classifier::fini ()
{
  XDELETEVEC (m_classify_diagnostic);
  m_classify_diagnostic = nullptr;
  free (m_push_list);
  m_n_push = 0;
}

/* Save all diagnostic classifications in a stack.  */

void
diagnostic_option_classifier::push ()
{
  m_push_list = (int *) xrealloc (m_push_list, (m_n_push + 1) * sizeof (int));
  m_push_list[m_n_push ++] = m_n_classification_history;
}

/* Restore the topmost classification set off the stack.  If the stack
   is empty, revert to the state based on command line parameters.  */

void
diagnostic_option_classifier::pop (location_t where)
{
  int jump_to;

  if (m_n_push)
    jump_to = m_push_list [-- m_n_push];
  else
    jump_to = 0;

  const int i = m_n_classification_history;
  m_classification_history =
    (diagnostic_classification_change_t *) xrealloc (m_classification_history, (i + 1)
						     * sizeof (diagnostic_classification_change_t));
  m_classification_history[i].location = where;
  m_classification_history[i].option = jump_to;
  m_classification_history[i].kind = DK_POP;
  m_n_classification_history ++;
}

/* Initialize the diagnostic message outputting machinery.  */

void
diagnostic_context::initialize (int n_opts)
{
  /* Allocate a basic pretty-printer.  Clients will replace this a
     much more elaborated pretty-printer if they wish.  */
  this->printer = XNEW (pretty_printer);
  new (this->printer) pretty_printer ();

  m_file_cache = new file_cache ();
  memset (m_diagnostic_count, 0, sizeof m_diagnostic_count);
  m_warning_as_error_requested = false;
  m_n_opts = n_opts;
  m_option_classifier.init (n_opts);
  m_source_printing.enabled = false;
  diagnostic_set_caret_max_width (this, pp_line_cutoff (this->printer));
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
  m_text_callbacks.m_begin_diagnostic = default_diagnostic_starter;
  m_text_callbacks.m_start_span = default_diagnostic_start_span_fn;
  m_text_callbacks.m_end_diagnostic = default_diagnostic_finalizer;
  m_option_callbacks.m_option_enabled_cb = nullptr;
  m_option_callbacks.m_option_state = nullptr;
  m_option_callbacks.m_make_option_name_cb = nullptr;
  m_option_callbacks.m_make_option_url_cb = nullptr;
  m_urlifier = nullptr;
  m_last_location = UNKNOWN_LOCATION;
  m_last_module = nullptr;
  m_client_aux_data = nullptr;
  m_lock = 0;
  m_inhibit_notes_p = false;
  m_source_printing.colorize_source_p = false;
  m_source_printing.show_labels_p = false;
  m_source_printing.show_line_numbers_p = false;
  m_source_printing.min_margin_width = 0;
  m_source_printing.show_ruler_p = false;
  m_source_printing.show_event_links_p = false;
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
  m_column_unit = DIAGNOSTICS_COLUMN_UNIT_DISPLAY;
  m_column_origin = 1;
  m_tabstop = 8;
  m_escape_format = DIAGNOSTICS_ESCAPE_FORMAT_UNICODE;
  m_edit_context_ptr = nullptr;
  m_diagnostic_groups.m_nesting_depth = 0;
  m_diagnostic_groups.m_emission_count = 0;
  m_output_format = new diagnostic_text_output_format (*this);
  m_set_locations_cb = nullptr;
  m_ice_handler_cb = nullptr;
  m_includes_seen = nullptr;
  m_client_data_hooks = nullptr;
  m_diagrams.m_theme = nullptr;

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
}

/* Maybe initialize the color support. We require clients to do this
   explicitly, since most clients don't want color.  When called
   without a VALUE, it initializes with DIAGNOSTICS_COLOR_DEFAULT.  */

void
diagnostic_context::color_init (int value)
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
  pp_show_color (this->printer)
    = colorize_init ((diagnostic_color_rule_t) value);
}

/* Initialize URL support within this context based on VALUE,
   handling "auto".  */

void
diagnostic_context::urls_init (int value)
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

  this->printer->url_format
    = determine_url_format ((diagnostic_url_rule_t) value);
}

/* Create the file_cache, if not already created, and tell it how to
   translate files on input.  */
void
diagnostic_context::
initialize_input_context (diagnostic_input_charset_callback ccb,
			  bool should_skip_bom)
{
  m_file_cache->initialize_input_context (ccb, should_skip_bom);
}

/* Do any cleaning up required after the last diagnostic is emitted.  */

void
diagnostic_context::finish ()
{
  delete m_output_format;
  m_output_format= nullptr;

  if (m_diagrams.m_theme)
    {
      delete m_diagrams.m_theme;
      m_diagrams.m_theme = nullptr;
    }

  delete m_file_cache;
  m_file_cache = nullptr;

  m_option_classifier.fini ();

  /* diagnostic_context::initialize allocates this->printer using XNEW
     and placement-new.  */
  this->printer->~pretty_printer ();
  XDELETE (this->printer);
  this->printer = nullptr;

  if (m_edit_context_ptr)
    {
      delete m_edit_context_ptr;
      m_edit_context_ptr = nullptr;
    }

  if (m_includes_seen)
    {
      delete m_includes_seen;
      m_includes_seen = nullptr;
    }

  if (m_client_data_hooks)
    {
      delete m_client_data_hooks;
      m_client_data_hooks = nullptr;
    }

  delete m_urlifier;
  m_urlifier = nullptr;
}

void
diagnostic_context::set_output_format (diagnostic_output_format *output_format)
{
  /* Ideally we'd use a std::unique_ptr here.  */
  delete m_output_format;
  m_output_format = output_format;
}

void
diagnostic_context::set_client_data_hooks (diagnostic_client_data_hooks *hooks)
{
  /* Ideally we'd use a std::unique_ptr here.  */
  delete m_client_data_hooks;
  m_client_data_hooks = hooks;
}

void
diagnostic_context::
set_option_hooks (diagnostic_option_enabled_cb option_enabled_cb,
		  void *option_state,
		  diagnostic_make_option_name_cb make_option_name_cb,
		  diagnostic_make_option_url_cb make_option_url_cb,
		  unsigned lang_mask)
{
  m_option_callbacks.m_option_enabled_cb = option_enabled_cb;
  m_option_callbacks.m_option_state = option_state;
  m_option_callbacks.m_make_option_name_cb = make_option_name_cb;
  m_option_callbacks.m_make_option_url_cb = make_option_url_cb;
  m_option_callbacks.m_lang_mask = lang_mask;
}

void
diagnostic_context::set_urlifier (urlifier *urlifier)
{
  /* Ideally we'd use a std::unique_ptr here.  */
  delete m_urlifier;
  m_urlifier = urlifier;
}

void
diagnostic_context::create_edit_context ()
{
  delete m_edit_context_ptr;
  gcc_assert (m_file_cache);
  m_edit_context_ptr = new edit_context (*m_file_cache);
}

/* Initialize DIAGNOSTIC, where the message MSG has already been
   translated.  */
void
diagnostic_set_info_translated (diagnostic_info *diagnostic, const char *msg,
				va_list *args, rich_location *richloc,
				diagnostic_t kind)
{
  gcc_assert (richloc);
  diagnostic->message.m_err_no = errno;
  diagnostic->message.m_args_ptr = args;
  diagnostic->message.m_format_spec = msg;
  diagnostic->message.m_richloc = richloc;
  diagnostic->richloc = richloc;
  diagnostic->metadata = NULL;
  diagnostic->kind = kind;
  diagnostic->option_index = 0;
}

/* Initialize DIAGNOSTIC, where the message GMSGID has not yet been
   translated.  */
void
diagnostic_set_info (diagnostic_info *diagnostic, const char *gmsgid,
		     va_list *args, rich_location *richloc,
		     diagnostic_t kind)
{
  gcc_assert (richloc);
  diagnostic_set_info_translated (diagnostic, _(gmsgid), args, richloc, kind);
}

static const char *const diagnostic_kind_color[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (C),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
  NULL
};

/* Get a color name for diagnostics of type KIND
   Result could be NULL.  */

const char *
diagnostic_get_color_for_kind (diagnostic_t kind)
{
  return diagnostic_kind_color[kind];
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
diagnostic_context::converted_column (expanded_location s) const
{
  int one_based_col = convert_column_unit (get_file_cache (),
					   m_column_unit, m_tabstop, s);
  if (one_based_col <= 0)
    return -1;
  return one_based_col + (m_column_origin - 1);
}

/* Return a formatted line and column ':%line:%column'.  Elided if
   line == 0 or col < 0.  (A column of 0 may be valid due to the
   -fdiagnostics-column-origin option.)
   The result is a statically allocated buffer.  */

static const char *
maybe_line_and_column (int line, int col)
{
  static char result[32];

  if (line)
    {
      size_t l
	= snprintf (result, sizeof (result),
		    col >= 0 ? ":%d:%d" : ":%d", line, col);
      gcc_checking_assert (l < sizeof (result));
    }
  else
    result[0] = 0;
  return result;
}

/* Return a string describing a location e.g. "foo.c:42:10".  */

label_text
diagnostic_context::get_location_text (const expanded_location &s) const
{
  pretty_printer *pp = this->printer;
  const char *locus_cs = colorize_start (pp_show_color (pp), "locus");
  const char *locus_ce = colorize_stop (pp_show_color (pp));
  const char *file = s.file ? s.file : progname;
  int line = 0;
  int col = -1;
  if (strcmp (file, special_fname_builtin ()))
    {
      line = s.line;
      if (m_show_column)
	col = this->converted_column (s);
    }

  const char *line_col = maybe_line_and_column (line, col);
  return label_text::take (build_message_string ("%s%s%s:%s", locus_cs, file,
						 line_col, locus_ce));
}

static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
  "must-not-happen"
};

/* Return a malloc'd string describing a location and the severity of the
   diagnostic, e.g. "foo.c:42:10: error: ".  The caller is responsible for
   freeing the memory.  */
char *
diagnostic_build_prefix (diagnostic_context *context,
			 const diagnostic_info *diagnostic)
{
  gcc_assert (diagnostic->kind < DK_LAST_DIAGNOSTIC_KIND);

  const char *text = _(diagnostic_kind_text[diagnostic->kind]);
  const char *text_cs = "", *text_ce = "";
  pretty_printer *pp = context->printer;

  if (diagnostic_kind_color[diagnostic->kind])
    {
      text_cs = colorize_start (pp_show_color (pp),
				diagnostic_kind_color[diagnostic->kind]);
      text_ce = colorize_stop (pp_show_color (pp));
    }

  const expanded_location s = diagnostic_expand_location (diagnostic);
  label_text location_text = context->get_location_text (s);

  char *result = build_message_string ("%s %s%s%s", location_text.get (),
				       text_cs, text, text_ce);
  return result;
}

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
  if (filename == NULL && function == NULL)
    return 0;

  /* Skip functions in diagnostic.cc.  */
  if (*pcount == 0
      && filename != NULL
      && strcmp (lbasename (filename), "diagnostic.cc") == 0)
    return 0;

  /* Print up to 20 functions.  We could make this a --param, but
     since this is only for debugging just use a constant for now.  */
  if (*pcount >= 20)
    {
      /* Returning a non-zero value stops the backtrace.  */
      return 1;
    }
  ++*pcount;

  char *alc = NULL;
  if (function != NULL)
    {
      char *str = cplus_demangle_v3 (function,
				     (DMGL_VERBOSE | DMGL_ANSI
				      | DMGL_GNU_V3 | DMGL_PARAMS));
      if (str != NULL)
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
	      if (alc != NULL)
		free (alc);
	      /* Returning a non-zero value stops the backtrace.  */
	      return 1;
	    }
	}
    }

  fprintf (stderr, "0x%lx %s\n\t%s:%d\n",
	   (unsigned long) pc,
	   function == NULL ? "???" : function,
	   filename == NULL ? "???" : filename,
	   lineno);

  if (alc != NULL)
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

/* Check if we've met the maximum error limit, and if so fatally exit
   with a message.
   FLUSH indicates whether a diagnostic_context::finish call is needed.  */

void
diagnostic_context::check_max_errors (bool flush)
{
  if (!m_max_errors)
    return;

  int count = (m_diagnostic_count[DK_ERROR]
	       + m_diagnostic_count[DK_SORRY]
	       + m_diagnostic_count[DK_WERROR]);

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
diagnostic_context::action_after_output (diagnostic_t diag_kind)
{
  switch (diag_kind)
    {
    case DK_DEBUG:
    case DK_NOTE:
    case DK_ANACHRONISM:
    case DK_WARNING:
      break;

    case DK_ERROR:
    case DK_SORRY:
      if (m_abort_on_error)
	real_abort ();
      if (m_fatal_errors)
	{
	  fnotice (stderr, "compilation terminated due to -Wfatal-errors.\n");
	  finish ();
	  exit (FATAL_EXIT_CODE);
	}
      break;

    case DK_ICE:
    case DK_ICE_NOBT:
      {
	/* Optional callback for attempting to handle ICEs gracefully.  */
	if (void (*ice_handler_cb) (diagnostic_context *) = m_ice_handler_cb)
	  {
	    /* Clear the callback, to avoid potentially re-entering
	       the routine if there's a crash within the handler.  */
	    m_ice_handler_cb = NULL;
	    ice_handler_cb (this);
	  }
	/* The context might have had diagnostic_finish called on
	   it at this point.  */

	struct backtrace_state *state = NULL;
	if (diag_kind == DK_ICE)
	  state = backtrace_create_state (NULL, 0, bt_err_callback, NULL);
	int count = 0;
	if (state != NULL)
	  backtrace_full (state, 2, bt_callback, bt_err_callback,
			  (void *) &count);

	if (m_abort_on_error)
	  real_abort ();

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

    case DK_FATAL:
      if (m_abort_on_error)
	real_abort ();
      fnotice (stderr, "compilation terminated.\n");
      finish ();
      exit (FATAL_EXIT_CODE);

    default:
      gcc_unreachable ();
    }
}

/* Only dump the "In file included from..." stack once for each file.  */

bool
diagnostic_context::includes_seen_p (const line_map_ordinary *map)
{
  /* No include path for main.  */
  if (MAIN_FILE_P (map))
    return true;

  /* Always identify C++ modules, at least for now.  */
  auto probe = map;
  if (linemap_check_ordinary (map)->reason == LC_RENAME)
    /* The module source file shows up as LC_RENAME inside LC_MODULE.  */
    probe = linemap_included_from_linemap (line_table, map);
  if (MAP_MODULE_P (probe))
    return false;

  if (!m_includes_seen)
    m_includes_seen = new hash_set<location_t, false, location_hash>;

  /* Hash the location of the #include directive to better handle files
     that are included multiple times with different macros defined.  */
  return m_includes_seen->add (linemap_included_from (map));
}

void
diagnostic_context::report_current_module (location_t where)
{
  const line_map_ordinary *map = NULL;

  if (pp_needs_newline (this->printer))
    {
      pp_newline (this->printer);
      pp_needs_newline (this->printer) = false;
    }

  if (where <= BUILTINS_LOCATION)
    return;

  linemap_resolve_location (line_table, where,
			    LRK_MACRO_DEFINITION_LOCATION,
			    &map);

  if (map && m_last_module != map)
    {
      m_last_module = map;
      if (!includes_seen_p (map))
	{
	  bool first = true, need_inc = true, was_module = MAP_MODULE_P (map);
	  expanded_location s = {};
	  do
	    {
	      where = linemap_included_from (map);
	      map = linemap_included_from_linemap (line_table, map);
	      bool is_module = MAP_MODULE_P (map);
	      s.file = LINEMAP_FILE (map);
	      s.line = SOURCE_LINE (map, where);
	      int col = -1;
	      if (first && m_show_column)
		{
		  s.column = SOURCE_COLUMN (map, where);
		  col = converted_column (s);
		}
	      const char *line_col = maybe_line_and_column (s.line, col);
	      static const char *const msgs[] =
		{
		 NULL,
		 N_("                 from"),
		 N_("In file included from"),	/* 2 */
		 N_("        included from"),
		 N_("In module"),		/* 4 */
		 N_("of module"),
		 N_("In module imported at"),	/* 6 */
		 N_("imported at"),
		};

	      unsigned index = (was_module ? 6 : is_module ? 4
				: need_inc ? 2 : 0) + !first;

	      pp_verbatim (this->printer, "%s%s %r%s%s%R",
			   first ? "" : was_module ? ", " : ",\n",
			   _(msgs[index]),
			   "locus", s.file, line_col);
	      first = false, need_inc = was_module, was_module = is_module;
	    }
	  while (!includes_seen_p (map));
	  pp_verbatim (this->printer, ":");
	  pp_newline (this->printer);
	}
    }
}

/* If DIAGNOSTIC has a diagnostic_path and this context supports
   printing paths, print the path.  */

void
diagnostic_context::show_any_path (const diagnostic_info &diagnostic)
{
  const diagnostic_path *path = diagnostic.richloc->get_path ();
  if (!path)
    return;

  if (m_print_path)
    m_print_path (this, path);
}

/* class diagnostic_event.  */

/* struct diagnostic_event::meaning.  */

void
diagnostic_event::meaning::dump_to_pp (pretty_printer *pp) const
{
  bool need_comma = false;
  pp_character (pp, '{');
  if (const char *verb_str = maybe_get_verb_str (m_verb))
    {
      pp_printf (pp, "verb: %qs", verb_str);
      need_comma = true;
    }
  if (const char *noun_str = maybe_get_noun_str (m_noun))
    {
      if (need_comma)
	pp_string (pp, ", ");
      pp_printf (pp, "noun: %qs", noun_str);
      need_comma = true;
    }
  if (const char *property_str = maybe_get_property_str (m_property))
    {
      if (need_comma)
	pp_string (pp, ", ");
      pp_printf (pp, "property: %qs", property_str);
      need_comma = true;
    }
  pp_character (pp, '}');
}

/* Get a string (or NULL) for V suitable for use within a SARIF
   threadFlowLocation "kinds" property (SARIF v2.1.0 section 3.38.8).  */

const char *
diagnostic_event::meaning::maybe_get_verb_str (enum verb v)
{
  switch (v)
    {
    default:
      gcc_unreachable ();
    case VERB_unknown:
      return NULL;
    case VERB_acquire:
      return "acquire";
    case VERB_release:
      return "release";
    case VERB_enter:
      return "enter";
    case VERB_exit:
      return "exit";
    case VERB_call:
      return "call";
    case VERB_return:
      return "return";
    case VERB_branch:
      return "branch";
    case VERB_danger:
      return "danger";
    }
}

/* Get a string (or NULL) for N suitable for use within a SARIF
   threadFlowLocation "kinds" property (SARIF v2.1.0 section 3.38.8).  */

const char *
diagnostic_event::meaning::maybe_get_noun_str (enum noun n)
{
  switch (n)
    {
    default:
      gcc_unreachable ();
    case NOUN_unknown:
      return NULL;
    case NOUN_taint:
      return "taint";
    case NOUN_sensitive:
      return "sensitive";
    case NOUN_function:
      return "function";
    case NOUN_lock:
      return "lock";
    case NOUN_memory:
      return "memory";
    case NOUN_resource:
      return "resource";
    }
}

/* Get a string (or NULL) for P suitable for use within a SARIF
   threadFlowLocation "kinds" property (SARIF v2.1.0 section 3.38.8).  */

const char *
diagnostic_event::meaning::maybe_get_property_str (enum property p)
{
  switch (p)
    {
    default:
      gcc_unreachable ();
    case PROPERTY_unknown:
      return NULL;
    case PROPERTY_true:
      return "true";
    case PROPERTY_false:
      return "false";
    }
}

/* class diagnostic_path.  */

/* Subroutint of diagnostic_path::interprocedural_p.
   Look for the first event in this path that is within a function
   i.e. has a non-NULL fndecl, and a non-zero stack depth.
   If found, write its index to *OUT_IDX and return true.
   Otherwise return false.  */

bool
diagnostic_path::get_first_event_in_a_function (unsigned *out_idx) const
{
  const unsigned num = num_events ();
  for (unsigned i = 0; i < num; i++)
    {
      if (!(get_event (i).get_fndecl () == NULL
	    && get_event (i).get_stack_depth () == 0))
	{
	  *out_idx = i;
	  return true;
	}
    }
  return false;
}

/* Return true if the events in this path involve more than one
   function, or false if it is purely intraprocedural.  */

bool
diagnostic_path::interprocedural_p () const
{
  /* Ignore leading events that are outside of any function.  */
  unsigned first_fn_event_idx;
  if (!get_first_event_in_a_function (&first_fn_event_idx))
    return false;

  const diagnostic_event &first_fn_event = get_event (first_fn_event_idx);
  tree first_fndecl = first_fn_event.get_fndecl ();
  int first_fn_stack_depth = first_fn_event.get_stack_depth ();

  const unsigned num = num_events ();
  for (unsigned i = first_fn_event_idx + 1; i < num; i++)
    {
      if (get_event (i).get_fndecl () != first_fndecl)
	return true;
      if (get_event (i).get_stack_depth () != first_fn_stack_depth)
	return true;
    }
  return false;
}

void
default_diagnostic_starter (diagnostic_context *context,
			    const diagnostic_info *diagnostic)
{
  diagnostic_report_current_module (context, diagnostic_location (diagnostic));
  pp_set_prefix (context->printer, diagnostic_build_prefix (context,
							    diagnostic));
}

void
default_diagnostic_start_span_fn (diagnostic_context *context,
				  expanded_location exploc)
{
  label_text text = context->get_location_text (exploc);
  pp_string (context->printer, text.get ());
  pp_newline (context->printer);
}

void
default_diagnostic_finalizer (diagnostic_context *context,
			      const diagnostic_info *diagnostic,
			      diagnostic_t)
{
  char *saved_prefix = pp_take_prefix (context->printer);
  pp_set_prefix (context->printer, NULL);
  pp_newline (context->printer);
  diagnostic_show_locus (context, diagnostic->richloc, diagnostic->kind);
  pp_set_prefix (context->printer, saved_prefix);
  pp_flush (context->printer);
}

/* Interface to specify diagnostic kind overrides.  Returns the
   previous setting, or DK_UNSPECIFIED if the parameters are out of
   range.  If OPTION_INDEX is zero, the new setting is for all the
   diagnostics.  */
diagnostic_t
diagnostic_option_classifier::
classify_diagnostic (const diagnostic_context *context,
		     int option_index,
		     diagnostic_t new_kind,
		     location_t where)
{
  diagnostic_t old_kind;

  if (option_index < 0
      || option_index >= m_n_opts
      || new_kind >= DK_LAST_DIAGNOSTIC_KIND)
    return DK_UNSPECIFIED;

  old_kind = m_classify_diagnostic[option_index];

  /* Handle pragmas separately, since we need to keep track of *where*
     the pragmas were.  */
  if (where != UNKNOWN_LOCATION)
    {
      int i;

      /* Record the command-line status, so we can reset it back on DK_POP. */
      if (old_kind == DK_UNSPECIFIED)
	{
	  old_kind = !context->option_enabled_p (option_index)
	    ? DK_IGNORED : DK_ANY;
	  m_classify_diagnostic[option_index] = old_kind;
	}

      for (i = m_n_classification_history - 1; i >= 0; i --)
	if (m_classification_history[i].option == option_index)
	  {
	    old_kind = m_classification_history[i].kind;
	    break;
	  }

      i = m_n_classification_history;
      m_classification_history =
	(diagnostic_classification_change_t *) xrealloc (m_classification_history, (i + 1)
							 * sizeof (diagnostic_classification_change_t));
      m_classification_history[i].location = where;
      m_classification_history[i].option = option_index;
      m_classification_history[i].kind = new_kind;
      m_n_classification_history ++;
    }
  else
    m_classify_diagnostic[option_index] = new_kind;

  return old_kind;
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
  pp_set_prefix (pp, NULL);

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
diagnostic_context::get_any_inlining_info (diagnostic_info *diagnostic)
{
  auto &ilocs = diagnostic->m_iinfo.m_ilocs;

  if (m_set_locations_cb)
    /* Retrieve the locations into which the expression about to be
       diagnosed has been inlined, including those of all the callers
       all the way down the inlining stack.  */
    m_set_locations_cb (this, diagnostic);
  else
    {
      /* When there's no callback use just the one location provided
	 by the caller of the diagnostic function.  */
      location_t loc = diagnostic_location (diagnostic);
      ilocs.safe_push (loc);
      diagnostic->m_iinfo.m_allsyslocs = in_system_header_at (loc);
    }
}

/* Update the kind of DIAGNOSTIC based on its location(s), including
   any of those in its inlining stack, relative to any
     #pragma GCC diagnostic
   directives recorded within this object.

   Return the new kind of DIAGNOSTIC if it was updated, or DK_UNSPECIFIED
   otherwise.  */

diagnostic_t
diagnostic_option_classifier::
update_effective_level_from_pragmas (diagnostic_info *diagnostic) const
{
  if (m_n_classification_history <= 0)
    return DK_UNSPECIFIED;

  /* Iterate over the locations, checking the diagnostic disposition
     for the diagnostic at each.  If it's explicitly set as opposed
     to unspecified, update the disposition for this instance of
     the diagnostic and return it.  */
  for (location_t loc: diagnostic->m_iinfo.m_ilocs)
    {
      /* FIXME: Stupid search.  Optimize later. */
      for (int i = m_n_classification_history - 1; i >= 0; i --)
	{
	  const diagnostic_classification_change_t &hist
	    = m_classification_history[i];

	  location_t pragloc = hist.location;
	  if (!linemap_location_before_p (line_table, pragloc, loc))
	    continue;

	  if (hist.kind == (int) DK_POP)
	    {
	      /* Move on to the next region.  */
	      i = hist.option;
	      continue;
	    }

	  int option = hist.option;
	  /* The option 0 is for all the diagnostics.  */
	  if (option == 0 || option == diagnostic->option_index)
	    {
	      diagnostic_t kind = hist.kind;
	      if (kind != DK_UNSPECIFIED)
		diagnostic->kind = kind;
	      return kind;
	    }
	}
    }

  return DK_UNSPECIFIED;
}

/* Generate a URL string describing CWE.  The caller is responsible for
   freeing the string.  */

char *
get_cwe_url (int cwe)
{
  return xasprintf ("https://cwe.mitre.org/data/definitions/%i.html", cwe);
}

/* If DIAGNOSTIC has a CWE identifier, print it.

   For example, if the diagnostic metadata associates it with CWE-119,
   " [CWE-119]" will be printed, suitably colorized, and with a URL of a
   description of the security issue.  */

void
diagnostic_context::print_any_cwe (const diagnostic_info &diagnostic)
{
  if (diagnostic.metadata == NULL)
    return;

  int cwe = diagnostic.metadata->get_cwe ();
  if (cwe)
    {
      pretty_printer * const pp = this->printer;
      char *saved_prefix = pp_take_prefix (pp);
      pp_string (pp, " [");
      pp_string (pp, colorize_start (pp_show_color (pp),
				     diagnostic_kind_color[diagnostic.kind]));
      if (pp->url_format != URL_FORMAT_NONE)
	{
	  char *cwe_url = get_cwe_url (cwe);
	  pp_begin_url (pp, cwe_url);
	  free (cwe_url);
	}
      pp_printf (pp, "CWE-%i", cwe);
      pp_set_prefix (pp, saved_prefix);
      if (pp->url_format != URL_FORMAT_NONE)
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
diagnostic_context::print_any_rules (const diagnostic_info &diagnostic)
{
  if (diagnostic.metadata == NULL)
    return;

  for (unsigned idx = 0; idx < diagnostic.metadata->get_num_rules (); idx++)
    {
      const diagnostic_metadata::rule &rule
	= diagnostic.metadata->get_rule (idx);
      if (char *desc = rule.make_description ())
	{
	  pretty_printer * const pp = this->printer;
	  char *saved_prefix = pp_take_prefix (pp);
	  pp_string (pp, " [");
	  pp_string (pp,
		     colorize_start (pp_show_color (pp),
				     diagnostic_kind_color[diagnostic.kind]));
	  char *url = NULL;
	  if (pp->url_format != URL_FORMAT_NONE)
	    {
	      url = rule.make_url ();
	      if (url)
		pp_begin_url (pp, url);
	    }
	  pp_string (pp, desc);
	  pp_set_prefix (pp, saved_prefix);
	  if (pp->url_format != URL_FORMAT_NONE)
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
diagnostic_context::print_option_information (const diagnostic_info &diagnostic,
					      diagnostic_t orig_diag_kind)
{
  if (char *option_text = make_option_name (diagnostic.option_index,
					    orig_diag_kind, diagnostic.kind))
    {
      char *option_url = nullptr;
      if (this->printer->url_format != URL_FORMAT_NONE)
	option_url = make_option_url (diagnostic.option_index);
      pretty_printer * const pp = this->printer;
      pp_string (pp, " [");
      pp_string (pp, colorize_start (pp_show_color (pp),
				     diagnostic_kind_color[diagnostic.kind]));
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

/* Returns whether a DIAGNOSTIC should be printed, and adjusts diagnostic->kind
   as appropriate for #pragma GCC diagnostic and -Werror=foo.  */

bool
diagnostic_context::diagnostic_enabled (diagnostic_info *diagnostic)
{
  /* Update the inlining stack for this diagnostic.  */
  get_any_inlining_info (diagnostic);

  /* Diagnostics with no option or -fpermissive are always enabled.  */
  if (!diagnostic->option_index
      || diagnostic->option_index == permissive_error_option (this))
    return true;

  /* This tests if the user provided the appropriate -Wfoo or
     -Wno-foo option.  */
  if (!option_enabled_p (diagnostic->option_index))
    return false;

  /* This tests for #pragma diagnostic changes.  */
  diagnostic_t diag_class
    = m_option_classifier.update_effective_level_from_pragmas (diagnostic);

  /* This tests if the user provided the appropriate -Werror=foo
     option.  */
  if (diag_class == DK_UNSPECIFIED
      && !option_unspecified_p (diagnostic->option_index))
    {
      const diagnostic_t new_kind
	= m_option_classifier.get_current_override (diagnostic->option_index);
      if (new_kind != DK_ANY)
	/* DK_ANY means the diagnostic is not to be ignored, but we don't want
	   to change it specifically to DK_ERROR or DK_WARNING; we want to
	   preserve whatever the caller has specified.  */
	diagnostic->kind = new_kind;
    }

  /* This allows for future extensions, like temporarily disabling
     warnings for ranges of source code.  */
  if (diagnostic->kind == DK_IGNORED)
    return false;

  return true;
}

/* Returns whether warning OPT is enabled at LOC.  */

bool
diagnostic_context::warning_enabled_at (location_t loc, int opt)
{
  if (!diagnostic_report_warnings_p (this, loc))
    return false;

  rich_location richloc (line_table, loc);
  diagnostic_info diagnostic = {};
  diagnostic.option_index = opt;
  diagnostic.richloc = &richloc;
  diagnostic.message.m_richloc = &richloc;
  diagnostic.kind = DK_WARNING;
  return diagnostic_enabled (&diagnostic);
}

/* Report a diagnostic message (an error or a warning) as specified by
   this diagnostic_context.
   front-end independent format specifiers are exactly those described
   in the documentation of output_format.
   Return true if a diagnostic was printed, false otherwise.  */

bool
diagnostic_context::report_diagnostic (diagnostic_info *diagnostic)
{
  diagnostic_t orig_diag_kind = diagnostic->kind;

  gcc_assert (m_output_format);

  /* Give preference to being able to inhibit warnings, before they
     get reclassified to something else.  */
  bool was_warning = (diagnostic->kind == DK_WARNING
		      || diagnostic->kind == DK_PEDWARN);
  if (was_warning && m_inhibit_warnings)
    return false;

  if (diagnostic->kind == DK_PEDWARN)
    {
      diagnostic->kind = pedantic_warning_kind (this);
      /* We do this to avoid giving the message for -pedantic-errors.  */
      orig_diag_kind = diagnostic->kind;
    }

  if (diagnostic->kind == DK_NOTE && m_inhibit_notes_p)
    return false;

  if (m_lock > 0)
    {
      /* If we're reporting an ICE in the middle of some other error,
	 try to flush out the previous error, then let this one
	 through.  Don't do this more than once.  */
      if ((diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
	  && m_lock == 1)
	pp_newline_and_flush (this->printer);
      else
	error_recursion ();
    }

  /* If the user requested that warnings be treated as errors, so be
     it.  Note that we do this before the next block so that
     individual warnings can be overridden back to warnings with
     -Wno-error=*.  */
  if (m_warning_as_error_requested
      && diagnostic->kind == DK_WARNING)
    diagnostic->kind = DK_ERROR;

  diagnostic->message.m_data = &diagnostic->x_data;

  /* Check to see if the diagnostic is enabled at the location and
     not disabled by #pragma GCC diagnostic anywhere along the inlining
     stack.  .  */
  if (!diagnostic_enabled (diagnostic))
    return false;

  if ((was_warning || diagnostic->kind == DK_WARNING)
      && ((!m_warn_system_headers
	   && diagnostic->m_iinfo.m_allsyslocs)
	  || m_inhibit_warnings))
    /* Bail if the warning is not to be reported because all locations in the
       inlining stack (if there is one) are in system headers.  */
    return false;

  if (diagnostic->kind != DK_NOTE && diagnostic->kind != DK_ICE)
    diagnostic_check_max_errors (this);

  m_lock++;

  if (diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
    {
      /* When not checking, ICEs are converted to fatal errors when an
	 error has already occurred.  This is counteracted by
	 abort_on_error.  */
      if (!CHECKING_P
	  && (m_diagnostic_count[DK_ERROR] > 0
	      || m_diagnostic_count[DK_SORRY] > 0)
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
			     diagnostic->message.m_format_spec,
			     diagnostic->message.m_args_ptr);
    }
  if (diagnostic->kind == DK_ERROR && orig_diag_kind == DK_WARNING)
    ++m_diagnostic_count[DK_WERROR];
  else
    ++m_diagnostic_count[diagnostic->kind];

  /* Is this the initial diagnostic within the stack of groups?  */
  if (m_diagnostic_groups.m_emission_count == 0)
    m_output_format->on_begin_group ();
  m_diagnostic_groups.m_emission_count++;

  pp_format (this->printer, &diagnostic->message, m_urlifier);
  m_output_format->on_begin_diagnostic (*diagnostic);
  pp_output_formatted_text (this->printer, m_urlifier);
  if (m_show_cwe)
    print_any_cwe (*diagnostic);
  if (m_show_rules)
    print_any_rules (*diagnostic);
  if (m_show_option_requested)
    print_option_information (*diagnostic, orig_diag_kind);
  m_output_format->on_end_diagnostic (*diagnostic, orig_diag_kind);
  switch (m_extra_output_kind)
    {
    default:
      break;
    case EXTRA_DIAGNOSTIC_OUTPUT_fixits_v1:
      print_parseable_fixits (get_file_cache (),
			      this->printer, diagnostic->richloc,
			      DIAGNOSTICS_COLUMN_UNIT_BYTE,
			      m_tabstop);
      pp_flush (this->printer);
      break;
    case EXTRA_DIAGNOSTIC_OUTPUT_fixits_v2:
      print_parseable_fixits (get_file_cache (),
			      this->printer, diagnostic->richloc,
			      DIAGNOSTICS_COLUMN_UNIT_DISPLAY,
			      m_tabstop);
      pp_flush (this->printer);
      break;
    }
  diagnostic_action_after_output (this, diagnostic->kind);
  diagnostic->x_data = NULL;

  if (m_edit_context_ptr)
    if (diagnostic->richloc->fixits_can_be_auto_applied_p ())
      m_edit_context_ptr->add_fixits (diagnostic->richloc);

  m_lock--;

  show_any_path (*diagnostic);

  return true;
}

/* Get the number of digits in the decimal representation of VALUE.  */

int
num_digits (int value)
{
  /* Perhaps simpler to use log10 for this, but doing it this way avoids
     using floating point.  */
  gcc_assert (value >= 0);

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

/* Standard error reporting routines in increasing order of severity.
   All of these take arguments like printf.  */

/* Text to be emitted verbatim to the error message stream; this
   produces no prefix and disables line-wrapping.  Use rarely.  */
void
verbatim (const char *gmsgid, ...)
{
  va_list ap;

  va_start (ap, gmsgid);
  text_info text (_(gmsgid), &ap, errno);
  pp_format_verbatim (global_dc->printer, &text);
  pp_newline_and_flush (global_dc->printer);
  va_end (ap);
}

/* Add a note with text GMSGID and with LOCATION to the diagnostic CONTEXT.  */
void
diagnostic_append_note (diagnostic_context *context,
                        location_t location,
                        const char * gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc, DK_NOTE);
  if (context->m_inhibit_notes_p)
    {
      va_end (ap);
      return;
    }
  char *saved_prefix = pp_take_prefix (context->printer);
  pp_set_prefix (context->printer,
                 diagnostic_build_prefix (context, &diagnostic));
  pp_format (context->printer, &diagnostic.message);
  pp_output_formatted_text (context->printer);
  pp_destroy_prefix (context->printer);
  pp_set_prefix (context->printer, saved_prefix);
  pp_newline (context->printer);
  diagnostic_show_locus (context, &richloc, DK_NOTE);
  va_end (ap);
}

/* Implement emit_diagnostic, inform, warning, warning_at, pedwarn,
   permerror, error, error_at, error_at, sorry, fatal_error, internal_error,
   and internal_error_no_backtrace, as documented and defined below.  */
static bool
diagnostic_impl (rich_location *richloc, const diagnostic_metadata *metadata,
		 int opt, const char *gmsgid,
		 va_list *ap, diagnostic_t kind)
{
  diagnostic_info diagnostic;
  if (kind == DK_PERMERROR)
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc,
			   permissive_error_kind (global_dc));
      diagnostic.option_index = (opt != -1 ? opt
				 : permissive_error_option (global_dc));
    }
  else
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc, kind);
      if (kind == DK_WARNING || kind == DK_PEDWARN)
	diagnostic.option_index = opt;
    }
  diagnostic.metadata = metadata;
  return global_dc->report_diagnostic (&diagnostic);
}

/* Implement inform_n, warning_n, and error_n, as documented and
   defined below.  */
static bool
diagnostic_n_impl (rich_location *richloc, const diagnostic_metadata *metadata,
		   int opt, unsigned HOST_WIDE_INT n,
		   const char *singular_gmsgid,
		   const char *plural_gmsgid,
		   va_list *ap, diagnostic_t kind)
{
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
  if (kind == DK_WARNING)
    diagnostic.option_index = opt;
  diagnostic.metadata = metadata;
  return global_dc->report_diagnostic (&diagnostic);
}

/* Wrapper around diagnostic_impl taking a variable argument list.  */

bool
emit_diagnostic (diagnostic_t kind, location_t location, int opt,
		 const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = diagnostic_impl (&richloc, NULL, opt, gmsgid, &ap, kind);
  va_end (ap);
  return ret;
}

/* As above, but for rich_location *.  */

bool
emit_diagnostic (diagnostic_t kind, rich_location *richloc, int opt,
		 const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = diagnostic_impl (richloc, NULL, opt, gmsgid, &ap, kind);
  va_end (ap);
  return ret;
}

/* Wrapper around diagnostic_impl taking a va_list parameter.  */

bool
emit_diagnostic_valist (diagnostic_t kind, location_t location, int opt,
			const char *gmsgid, va_list *ap)
{
  rich_location richloc (line_table, location);
  return diagnostic_impl (&richloc, NULL, opt, gmsgid, ap, kind);
}

/* As above, but with rich_location and metadata.  */

bool
emit_diagnostic_valist_meta (diagnostic_t kind,
			     rich_location *richloc,
			     const diagnostic_metadata *metadata,
			     int opt,
			     const char *gmsgid, va_list *ap)
{
  return diagnostic_impl (richloc, metadata, opt, gmsgid, ap, kind);
}

/* An informative note at LOCATION.  Use this for additional details on an error
   message.  */
void
inform (location_t location, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_NOTE);
  va_end (ap);
}

/* Same as "inform" above, but at RICHLOC.  */
void
inform (rich_location *richloc, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  diagnostic_impl (richloc, NULL, -1, gmsgid, &ap, DK_NOTE);
  va_end (ap);
}

/* An informative note at LOCATION.  Use this for additional details on an
   error message.  */
void
inform_n (location_t location, unsigned HOST_WIDE_INT n,
	  const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  va_list ap;
  va_start (ap, plural_gmsgid);
  auto_diagnostic_group d;
  rich_location richloc (line_table, location);
  diagnostic_n_impl (&richloc, NULL, -1, n, singular_gmsgid, plural_gmsgid,
		     &ap, DK_NOTE);
  va_end (ap);
}

/* A warning at INPUT_LOCATION.  Use this for code which is correct according
   to the relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */
bool
warning (int opt, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  bool ret = diagnostic_impl (&richloc, NULL, opt, gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* A warning at LOCATION.  Use this for code which is correct according to the
   relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */

bool
warning_at (location_t location, int opt, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = diagnostic_impl (&richloc, NULL, opt, gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* Same as "warning at" above, but using RICHLOC.  */

bool
warning_at (rich_location *richloc, int opt, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = diagnostic_impl (richloc, NULL, opt, gmsgid, &ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* Same as "warning at" above, but using METADATA.  */

bool
warning_meta (rich_location *richloc,
	      const diagnostic_metadata &metadata,
	      int opt, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret
    = diagnostic_impl (richloc, &metadata, opt, gmsgid, &ap,
		       DK_WARNING);
  va_end (ap);
  return ret;
}

/* Same as warning_n plural variant below, but using RICHLOC.  */

bool
warning_n (rich_location *richloc, int opt, unsigned HOST_WIDE_INT n,
	   const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, plural_gmsgid);
  bool ret = diagnostic_n_impl (richloc, NULL, opt, n,
				singular_gmsgid, plural_gmsgid,
				&ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* A warning at LOCATION.  Use this for code which is correct according to the
   relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */

bool
warning_n (location_t location, int opt, unsigned HOST_WIDE_INT n,
	   const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, plural_gmsgid);
  rich_location richloc (line_table, location);
  bool ret = diagnostic_n_impl (&richloc, NULL, opt, n,
				singular_gmsgid, plural_gmsgid,
				&ap, DK_WARNING);
  va_end (ap);
  return ret;
}

/* A "pedantic" warning at LOCATION: issues a warning unless
   -pedantic-errors was given on the command line, in which case it
   issues an error.  Use this for diagnostics required by the relevant
   language standard, if you have chosen not to make them errors.

   Note that these diagnostics are issued independent of the setting
   of the -Wpedantic command-line switch.  To get a warning enabled
   only with that switch, use either "if (pedantic) pedwarn
   (OPT_Wpedantic,...)" or just "pedwarn (OPT_Wpedantic,..)".  To get a
   pedwarn independently of the -Wpedantic switch use "pedwarn (0,...)".

   Returns true if the warning was printed, false if it was inhibited.  */

bool
pedwarn (location_t location, int opt, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = diagnostic_impl (&richloc, NULL, opt, gmsgid, &ap, DK_PEDWARN);
  va_end (ap);
  return ret;
}

/* Same as pedwarn above, but using RICHLOC.  */

bool
pedwarn (rich_location *richloc, int opt, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = diagnostic_impl (richloc, NULL, opt, gmsgid, &ap, DK_PEDWARN);
  va_end (ap);
  return ret;
}

/* A "permissive" error at LOCATION: issues an error unless
   -fpermissive was given on the command line, in which case it issues
   a warning.  Use this for things that really should be errors but we
   want to support legacy code.

   Returns true if the warning was printed, false if it was inhibited.  */

bool
permerror (location_t location, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* Same as "permerror" above, but at RICHLOC.  */

bool
permerror (rich_location *richloc, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = diagnostic_impl (richloc, NULL, -1, gmsgid, &ap, DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* Similar to the above, but controlled by a flag other than -fpermissive.
   As above, an error by default or a warning with -fpermissive, but this
   diagnostic can also be downgraded by -Wno-error=opt.  */

bool
permerror_opt (location_t location, int opt, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, location);
  bool ret = diagnostic_impl (&richloc, NULL, opt, gmsgid, &ap, DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* Same as "permerror" above, but at RICHLOC.  */

bool
permerror_opt (rich_location *richloc, int opt, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  bool ret = diagnostic_impl (richloc, NULL, opt, gmsgid, &ap, DK_PERMERROR);
  va_end (ap);
  return ret;
}

/* A hard error: the code is definitely ill-formed, and an object file
   will not be produced.  */
void
error (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* A hard error: the code is definitely ill-formed, and an object file
   will not be produced.  */
void
error_n (location_t location, unsigned HOST_WIDE_INT n,
	 const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, plural_gmsgid);
  rich_location richloc (line_table, location);
  diagnostic_n_impl (&richloc, NULL, -1, n, singular_gmsgid, plural_gmsgid,
		     &ap, DK_ERROR);
  va_end (ap);
}

/* Same as above, but use location LOC instead of input_location.  */
void
error_at (location_t loc, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, loc);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* Same as above, but use RICH_LOC.  */

void
error_at (rich_location *richloc, const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  diagnostic_impl (richloc, NULL, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* Same as above, but with metadata.  */

void
error_meta (rich_location *richloc, const diagnostic_metadata &metadata,
	    const char *gmsgid, ...)
{
  gcc_assert (richloc);

  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  diagnostic_impl (richloc, &metadata, -1, gmsgid, &ap, DK_ERROR);
  va_end (ap);
}

/* "Sorry, not implemented."  Use for a language feature which is
   required by the relevant specification but not implemented by GCC.
   An object file will not be produced.  */
void
sorry (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_SORRY);
  va_end (ap);
}

/* Same as above, but use location LOC instead of input_location.  */
void
sorry_at (location_t loc, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, loc);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_SORRY);
  va_end (ap);
}

/* Return true if an error or a "sorry" has been seen.  Various
   processing is disabled after errors.  */
bool
seen_error (void)
{
  return errorcount || sorrycount;
}

/* An error which is severe enough that we make no attempt to
   continue.  Do not use this for internal consistency checks; that's
   internal_error.  Use of this function should be rare.  */
void
fatal_error (location_t loc, const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, loc);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_FATAL);
  va_end (ap);

  gcc_unreachable ();
}

/* An internal consistency check has failed.  We make no attempt to
   continue.  */
void
internal_error (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_ICE);
  va_end (ap);

  gcc_unreachable ();
}

/* Like internal_error, but no backtrace will be printed.  Used when
   the internal error does not happen at the current location, but happened
   somewhere else.  */
void
internal_error_no_backtrace (const char *gmsgid, ...)
{
  auto_diagnostic_group d;
  va_list ap;
  va_start (ap, gmsgid);
  rich_location richloc (line_table, input_location);
  diagnostic_impl (&richloc, NULL, -1, gmsgid, &ap, DK_ICE_NOBT);
  va_end (ap);

  gcc_unreachable ();
}

/* Emit DIAGRAM to this context, respecting the output format.  */

void
diagnostic_context::emit_diagram (const diagnostic_diagram &diagram)
{
  if (m_diagrams.m_theme == nullptr)
    return;

  gcc_assert (m_output_format);
  m_output_format->on_diagram (diagram);
}

/* Special case error functions.  Most are implemented in terms of the
   above, or should be.  */

/* Print a diagnostic MSGID on FILE.  This is just fprintf, except it
   runs its second argument through gettext.  */
void
fnotice (FILE *file, const char *cmsgid, ...)
{
  /* If the user requested one of the machine-readable diagnostic output
     formats on stderr (e.g. -fdiagnostics-format=sarif-stderr), then
     emitting free-form text on stderr will lead to corrupt output.
     Skip the message for such cases.  */
  if (file == stderr && global_dc)
    if (const diagnostic_output_format *output_format
	  = global_dc->get_output_format ())
      if (output_format->machine_readable_stderr_p ())
	return;

  va_list ap;

  va_start (ap, cmsgid);
  vfprintf (file, _(cmsgid), ap);
  va_end (ap);
}

/* Inform the user that an error occurred while trying to report some
   other error.  This indicates catastrophic internal inconsistencies,
   so give up now.  But do try to flush out the previous error.
   This mustn't use internal_error, that will cause infinite recursion.  */

void
diagnostic_context::error_recursion ()
{
  if (m_lock < 3)
    pp_newline_and_flush (this->printer);

  fnotice (stderr,
	   "internal compiler error: error reporting routines re-entered.\n");

  /* Call diagnostic_action_after_output to get the "please submit a bug
     report" message.  */
  diagnostic_action_after_output (this, DK_ICE);

  /* Do not use gcc_unreachable here; that goes through internal_error
     and therefore would cause infinite recursion.  */
  real_abort ();
}

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
  if (global_dc->printer == NULL)
    {
      /* Print the error message.  */
      fnotice (stderr, diagnostic_kind_text[DK_ICE]);
      fnotice (stderr, "in %s, at %s:%d", function, trim_filename (file), line);
      fputc ('\n', stderr);

      /* Attempt to print a backtrace.  */
      struct backtrace_state *state
	= backtrace_create_state (NULL, 0, bt_err_callback, NULL);
      int count = 0;
      if (state != NULL)
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

/* class diagnostic_context.  */

void
diagnostic_context::begin_group ()
{
  m_diagnostic_groups.m_nesting_depth++;
}

void
diagnostic_context::end_group ()
{
  if (--m_diagnostic_groups.m_nesting_depth == 0)
    {
      /* Handle the case where we've popped the final diagnostic group.
	 If any diagnostics were emitted, give the context a chance
	 to do something.  */
      if (m_diagnostic_groups.m_emission_count > 0)
	m_output_format->on_end_group ();
      m_diagnostic_groups.m_emission_count = 0;
    }
}

/* class auto_diagnostic_group.  */

/* Constructor: "push" this group into global_dc.  */

auto_diagnostic_group::auto_diagnostic_group ()
{
  global_dc->begin_group ();
}

/* Destructor: "pop" this group from global_dc.  */

auto_diagnostic_group::~auto_diagnostic_group ()
{
  global_dc->end_group ();
}

/* class diagnostic_text_output_format : public diagnostic_output_format.  */

diagnostic_text_output_format::~diagnostic_text_output_format ()
{
  /* Some of the errors may actually have been warnings.  */
  if (m_context.diagnostic_count (DK_WERROR))
    {
      /* -Werror was given.  */
      if (m_context.warning_as_error_requested_p ())
	pp_verbatim (m_context.printer,
		     _("%s: all warnings being treated as errors"),
		     progname);
      /* At least one -Werror= was given.  */
      else
	pp_verbatim (m_context.printer,
		     _("%s: some warnings being treated as errors"),
		     progname);
      pp_newline_and_flush (m_context.printer);
    }
}

void
diagnostic_text_output_format::on_begin_diagnostic (const diagnostic_info &diagnostic)
{
  (*diagnostic_starter (&m_context)) (&m_context, &diagnostic);
}

void
diagnostic_text_output_format::on_end_diagnostic (const diagnostic_info &diagnostic,
						  diagnostic_t orig_diag_kind)
{
  (*diagnostic_finalizer (&m_context)) (&m_context, &diagnostic,
					orig_diag_kind);
}

void
diagnostic_text_output_format::on_diagram (const diagnostic_diagram &diagram)
{
  char *saved_prefix = pp_take_prefix (m_context.printer);
  pp_set_prefix (m_context.printer, NULL);
  /* Use a newline before and after and a two-space indent
     to make the diagram stand out a little from the wall of text.  */
  pp_newline (m_context.printer);
  diagram.get_canvas ().print_to_pp (m_context.printer, "  ");
  pp_newline (m_context.printer);
  pp_set_prefix (m_context.printer, saved_prefix);
  pp_flush (m_context.printer);
}

/* Set the output format for CONTEXT to FORMAT, using BASE_FILE_NAME for
   file-based output formats.  */

void
diagnostic_output_format_init (diagnostic_context *context,
			       const char *base_file_name,
			       enum diagnostics_output_format format,
			       bool json_formatting)
{
  switch (format)
    {
    default:
      gcc_unreachable ();
    case DIAGNOSTICS_OUTPUT_FORMAT_TEXT:
      /* The default; do nothing.  */
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_JSON_STDERR:
      diagnostic_output_format_init_json_stderr (context,
						 json_formatting);
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_JSON_FILE:
      diagnostic_output_format_init_json_file (context,
					       json_formatting,
					       base_file_name);
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_SARIF_STDERR:
      diagnostic_output_format_init_sarif_stderr (context,
						  json_formatting);
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_SARIF_FILE:
      diagnostic_output_format_init_sarif_file (context,
						json_formatting,
						base_file_name);
      break;
    }
}

/* Initialize this context's m_diagrams based on CHARSET.
   Specifically, make a text_art::theme object for m_diagrams.m_theme,
   (or NULL for "no diagrams").  */

void
diagnostic_context::
set_text_art_charset (enum diagnostic_text_art_charset charset)
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

/* class simple_diagnostic_path : public diagnostic_path.  */

simple_diagnostic_path::simple_diagnostic_path (pretty_printer *event_pp)
: m_event_pp (event_pp),
  m_localize_events (true)
{
  add_thread ("main");
}

/* Implementation of diagnostic_path::num_events vfunc for
   simple_diagnostic_path: simply get the number of events in the vec.  */

unsigned
simple_diagnostic_path::num_events () const
{
  return m_events.length ();
}

/* Implementation of diagnostic_path::get_event vfunc for
   simple_diagnostic_path: simply return the event in the vec.  */

const diagnostic_event &
simple_diagnostic_path::get_event (int idx) const
{
  return *m_events[idx];
}

unsigned
simple_diagnostic_path::num_threads () const
{
  return m_threads.length ();
}

const diagnostic_thread &
simple_diagnostic_path::get_thread (diagnostic_thread_id_t idx) const
{
  return *m_threads[idx];
}

diagnostic_thread_id_t
simple_diagnostic_path::add_thread (const char *name)
{
  m_threads.safe_push (new simple_diagnostic_thread (name));
  return m_threads.length () - 1;
}

/* Add an event to this path at LOC within function FNDECL at
   stack depth DEPTH.

   Use m_context's printer to format FMT, as the text of the new
   event.  Localize FMT iff m_localize_events is set.

   Return the id of the new event.  */

diagnostic_event_id_t
simple_diagnostic_path::add_event (location_t loc, tree fndecl, int depth,
				   const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  text_info ti (m_localize_events ? _(fmt) : fmt,
		&ap, 0, nullptr, &rich_loc);
  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  simple_diagnostic_event *new_event
    = new simple_diagnostic_event (loc, fndecl, depth, pp_formatted_text (pp));
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

diagnostic_event_id_t
simple_diagnostic_path::add_thread_event (diagnostic_thread_id_t thread_id,
					  location_t loc,
					  tree fndecl,
					  int depth,
					  const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  text_info ti (_(fmt), &ap, 0, nullptr, &rich_loc);

  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  simple_diagnostic_event *new_event
    = new simple_diagnostic_event (loc, fndecl, depth, pp_formatted_text (pp),
				   thread_id);
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

/* Mark the most recent event on this path (which must exist) as being
   connected to the next one to be added.  */

void
simple_diagnostic_path::connect_to_next_event ()
{
  gcc_assert (m_events.length () > 0);
  m_events[m_events.length () - 1]->connect_to_next_event ();
}

/* struct simple_diagnostic_event.  */

/* simple_diagnostic_event's ctor.  */

simple_diagnostic_event::
simple_diagnostic_event (location_t loc,
			 tree fndecl,
			 int depth,
			 const char *desc,
			 diagnostic_thread_id_t thread_id)
: m_loc (loc), m_fndecl (fndecl), m_depth (depth), m_desc (xstrdup (desc)),
  m_connected_to_next_event (false),
  m_thread_id (thread_id)
{
}

/* simple_diagnostic_event's dtor.  */

simple_diagnostic_event::~simple_diagnostic_event ()
{
  free (m_desc);
}

/* Print PATH by emitting a dummy "note" associated with it.  */

DEBUG_FUNCTION
void debug (diagnostic_path *path)
{
  rich_location richloc (line_table, UNKNOWN_LOCATION);
  richloc.set_path (path);
  inform (&richloc, "debug path");
}

/* Really call the system 'abort'.  This has to go right at the end of
   this file, so that there are no functions after it that call abort
   and get the system abort instead of our macro.  */
#undef abort
static void
real_abort (void)
{
  abort ();
}

#if CHECKING_P

namespace selftest {

/* Helper function for test_print_escaped_string.  */

static void
assert_print_escaped_string (const location &loc, const char *expected_output,
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
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
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
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
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
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
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
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
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
     diagnostic_get_location_text (..., SHOW_COLUMN)
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
  test_diagnostic_context dc;
  dc.m_show_column = show_column;
  dc.m_column_unit = column_unit;
  dc.m_column_origin = origin;

  expanded_location xloc;
  xloc.file = filename;
  xloc.line = line;
  xloc.column = column;
  xloc.data = NULL;
  xloc.sysp = false;

  label_text actual_loc_text = dc.get_location_text (xloc);
  ASSERT_STREQ (expected_loc_text, actual_loc_text.get ());
}

/* Verify that diagnostic_get_location_text works as expected.  */

static void
test_diagnostic_get_location_text ()
{
  const char *old_progname = progname;
  progname = "PROGNAME";
  assert_location_text ("PROGNAME:", NULL, 0, 0, true);
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

  maybe_line_and_column (INT_MAX, INT_MAX);
  maybe_line_and_column (INT_MIN, INT_MIN);

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
}

/* Run all of the selftests within this file.  */

void
c_diagnostic_cc_tests ()
{
  test_print_escaped_string ();
  test_print_parseable_fixits_none ();
  test_print_parseable_fixits_insert ();
  test_print_parseable_fixits_remove ();
  test_print_parseable_fixits_replace ();
  test_print_parseable_fixits_bytes_vs_display_columns ();
  test_diagnostic_get_location_text ();
  test_num_digits ();

}

} // namespace selftest

#endif /* #if CHECKING_P */

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
