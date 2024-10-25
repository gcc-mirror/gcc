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
#define INCLUDE_MEMORY
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
#include "diagnostic-format.h"
#include "diagnostic-format-sarif.h"
#include "diagnostic-format-text.h"
#include "edit-context.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "opts.h"
#include "cpplib.h"
#include "text-art/theme.h"
#include "pretty-print-urlifier.h"
#include "logical-location.h"
#include "diagnostic-buffer.h"

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
    : (isatty (fileno (pp_buffer (context->m_printer)->m_stream))
       ? get_terminal_width () - 1 : INT_MAX);

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
  m_push_list = vNULL;
  m_classification_history = vNULL;
}

void
diagnostic_option_classifier::fini ()
{
  XDELETEVEC (m_classify_diagnostic);
  m_classify_diagnostic = nullptr;
  m_classification_history.release ();
  m_push_list.release ();
}

/* Save the diagnostic_option_classifier state to F for PCH
   output.  Returns 0 on success, -1 on error.  */

int
diagnostic_option_classifier::pch_save (FILE *f)
{
  unsigned int lengths[2] = { m_classification_history.length (),
			      m_push_list.length () };
  if (fwrite (lengths, sizeof (lengths), 1, f) != 1
      || (lengths[0]
	  && fwrite (m_classification_history.address (),
		     sizeof (diagnostic_classification_change_t),
		     lengths[0], f) != lengths[0])
      || (lengths[1]
	  && fwrite (m_push_list.address (), sizeof (int),
		     lengths[1], f) != lengths[1]))
    return -1;
  return 0;
}

/* Read the diagnostic_option_classifier state from F for PCH
   read.  Returns 0 on success, -1 on error.  */

int
diagnostic_option_classifier::pch_restore (FILE *f)
{
  unsigned int lengths[2];
  if (fread (lengths, sizeof (lengths), 1, f) != 1)
    return -1;
  gcc_checking_assert (m_classification_history.is_empty ());
  gcc_checking_assert (m_push_list.is_empty ());
  m_classification_history.safe_grow (lengths[0]);
  m_push_list.safe_grow (lengths[1]);
  if ((lengths[0]
       && fread (m_classification_history.address (),
		 sizeof (diagnostic_classification_change_t),
		 lengths[0], f) != lengths[0])
      || (lengths[1]
	  && fread (m_push_list.address (), sizeof (int),
		    lengths[1], f) != lengths[1]))
    return -1;
  return 0;
}

/* Save all diagnostic classifications in a stack.  */

void
diagnostic_option_classifier::push ()
{
  m_push_list.safe_push (m_classification_history.length ());
}

/* Restore the topmost classification set off the stack.  If the stack
   is empty, revert to the state based on command line parameters.  */

void
diagnostic_option_classifier::pop (location_t where)
{
  int jump_to;

  if (!m_push_list.is_empty ())
    jump_to = m_push_list.pop ();
  else
    jump_to = 0;

  diagnostic_classification_change_t v = { where, jump_to, DK_POP };
  m_classification_history.safe_push (v);
}

/* Initialize the diagnostic message outputting machinery.  */

void
diagnostic_context::initialize (int n_opts)
{
  /* Allocate a basic pretty-printer.  Clients will replace this a
     much more elaborated pretty-printer if they wish.  */
  m_printer = XNEW (pretty_printer);
  new (m_printer) pretty_printer ();

  m_file_cache = new file_cache ();
  m_diagnostic_counters.clear ();
  m_warning_as_error_requested = false;
  m_n_opts = n_opts;
  m_option_classifier.init (n_opts);
  m_source_printing.enabled = false;
  diagnostic_set_caret_max_width (this, pp_line_cutoff (m_printer));
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
  m_text_callbacks.m_begin_diagnostic = default_diagnostic_text_starter;
  m_text_callbacks.m_start_span = default_diagnostic_start_span_fn;
  m_text_callbacks.m_end_diagnostic = default_diagnostic_text_finalizer;
  m_option_mgr = nullptr;
  m_urlifier = nullptr;
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
  m_client_data_hooks = nullptr;
  m_diagrams.m_theme = nullptr;
  m_original_argv = nullptr;
  m_diagnostic_buffer = nullptr;

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
  pp_show_color (m_printer)
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

  m_printer->set_url_format
    (determine_url_format ((diagnostic_url_rule_t) value));
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
  /* We might be handling a fatal error.
     Close any active diagnostic groups, which may trigger flushing
     the output format.  */
  while (m_diagnostic_groups.m_nesting_depth > 0)
    end_group ();

  set_diagnostic_buffer (nullptr);

  /* Clean ups.  */

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
  m_printer->~pretty_printer ();
  XDELETE (m_printer);
  m_printer = nullptr;

  if (m_edit_context_ptr)
    {
      delete m_edit_context_ptr;
      m_edit_context_ptr = nullptr;
    }

  if (m_client_data_hooks)
    {
      delete m_client_data_hooks;
      m_client_data_hooks = nullptr;
    }

  delete m_urlifier;
  m_urlifier = nullptr;

  freeargv (m_original_argv);
  m_original_argv = nullptr;
}

/* Dump state of this diagnostic_context to OUT, for debugging.  */

void
diagnostic_context::dump (FILE *out) const
{
  fprintf (out, "diagnostic_context:\n");
  m_diagnostic_counters.dump (out, 2);
  fprintf (out, "  output format:\n");
  m_output_format->dump (out, 4);
  fprintf (out, "  printer:\n");
  m_printer->dump (out, 4);
  fprintf (out, "  diagnostic buffer:\n");
  if (m_diagnostic_buffer)
    m_diagnostic_buffer->dump (out, 4);
  else
    fprintf (out, "    (none):\n");
}

/* Return true if sufficiently severe diagnostics have been seen that
   we ought to exit with a non-zero exit code.  */

bool
diagnostic_context::execution_failed_p () const
{
  /* Equivalent to (seen_error () || werrorcount), but on
     this context, rather than global_dc.  */
  return (diagnostic_count (DK_ERROR)
	  || diagnostic_count (DK_SORRY)
	  || diagnostic_count (DK_WERROR));
}

void
diagnostic_context::
set_output_format (std::unique_ptr<diagnostic_output_format> output_format)
{
  delete m_output_format;
  /* Ideally this field would be a std::unique_ptr.  */
  m_output_format = output_format.release ();
}

void
diagnostic_context::
set_client_data_hooks (std::unique_ptr<diagnostic_client_data_hooks> hooks)
{
  delete m_client_data_hooks;
  /* Ideally the field would be a std::unique_ptr here.  */
  m_client_data_hooks = hooks.release ();
}

void
diagnostic_context::set_original_argv (unique_argv original_argv)
{
  /* Ideally we'd use a unique_argv for m_original_argv, but
     diagnostic_context doesn't yet have a ctor/dtor pair.  */

  // Ensure any old value is freed
  freeargv (m_original_argv);

  // Take ownership of the new value
  m_original_argv = original_argv.release ();
}

void
diagnostic_context::
set_option_manager (std::unique_ptr<diagnostic_option_manager> mgr,
		    unsigned lang_mask)
{
  delete m_option_mgr;
  m_option_mgr = mgr.release ();
  m_lang_mask = lang_mask;
}

void
diagnostic_context::set_urlifier (std::unique_ptr<urlifier> urlifier)
{
  delete m_urlifier;
  /* Ideally the field would be a std::unique_ptr here.  */
  m_urlifier = urlifier.release ();
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
  diagnostic->option_id = 0;
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

diagnostic_column_policy::
diagnostic_column_policy (const diagnostic_context &dc)
: m_file_cache (dc.get_file_cache ()),
  m_column_unit (dc.m_column_unit),
  m_column_origin (dc.m_column_origin),
  m_tabstop (dc.m_tabstop)
{
}

/* Given an expanded_location, convert the column (which is in 1-based bytes)
   to the requested units and origin.  Return -1 if the column is
   invalid (<= 0).  */
int
diagnostic_column_policy::converted_column (expanded_location s) const
{
  int one_based_col = convert_column_unit (m_file_cache,
					   m_column_unit, m_tabstop, s);
  if (one_based_col <= 0)
    return -1;
  return one_based_col + (m_column_origin - 1);
}

/* Return a string describing a location e.g. "foo.c:42:10".  */

label_text
diagnostic_column_policy::get_location_text (const expanded_location &s,
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

  const char *line_col = maybe_line_and_column (line, col);
  return label_text::take (build_message_string ("%s%s%s:%s", locus_cs, file,
						 line_col, locus_ce));
}

diagnostic_location_print_policy::
diagnostic_location_print_policy (const diagnostic_context &dc)
: m_column_policy (dc),
  m_show_column (dc.m_show_column)
{
}

diagnostic_location_print_policy::
diagnostic_location_print_policy (const diagnostic_text_output_format &text_output)
:
  m_column_policy (text_output.get_context ()),
  m_show_column (text_output.get_context ().m_show_column)
{
}

static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
  "must-not-happen"
};

/* Get unlocalized string describing KIND.  */

const char *
get_diagnostic_kind_text (diagnostic_t kind)
{
  return diagnostic_kind_text[kind];
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

  int count = (diagnostic_count (DK_ERROR)
	       + diagnostic_count (DK_SORRY)
	       + diagnostic_count (DK_WERROR));

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
	/* Attempt to ensure that any outputs are flushed e.g. that .sarif
	   files are written out.
	   Only do it once.  */
	static bool finishing_due_to_ice = false;
	if (!finishing_due_to_ice)
	  {
	    finishing_due_to_ice = true;
	    finish ();
	  }

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

/* class logical_location.  */

/* Return true iff this is a function or method.  */

bool
logical_location::function_p () const
{
  switch (get_kind ())
    {
    default:
      gcc_unreachable ();
    case LOGICAL_LOCATION_KIND_UNKNOWN:
    case LOGICAL_LOCATION_KIND_MODULE:
    case LOGICAL_LOCATION_KIND_NAMESPACE:
    case LOGICAL_LOCATION_KIND_TYPE:
    case LOGICAL_LOCATION_KIND_RETURN_TYPE:
    case LOGICAL_LOCATION_KIND_PARAMETER:
    case LOGICAL_LOCATION_KIND_VARIABLE:
      return false;

    case LOGICAL_LOCATION_KIND_FUNCTION:
    case LOGICAL_LOCATION_KIND_MEMBER:
      return true;
    }
}

void
default_diagnostic_start_span_fn (const diagnostic_location_print_policy &loc_policy,
				  pretty_printer *pp,
				  expanded_location exploc)
{
  const diagnostic_column_policy &column_policy
    = loc_policy.get_column_policy ();
  label_text text
    = column_policy.get_location_text (exploc,
				       loc_policy.show_column_p (),
				       pp_show_color (pp));
  pp_string (pp, text.get ());
  pp_newline (pp);
}

/* Interface to specify diagnostic kind overrides.  Returns the
   previous setting, or DK_UNSPECIFIED if the parameters are out of
   range.  If OPTION_ID is zero, the new setting is for all the
   diagnostics.  */
diagnostic_t
diagnostic_option_classifier::
classify_diagnostic (const diagnostic_context *context,
		     diagnostic_option_id option_id,
		     diagnostic_t new_kind,
		     location_t where)
{
  diagnostic_t old_kind;

  if (option_id.m_idx < 0
      || option_id.m_idx >= m_n_opts
      || new_kind >= DK_LAST_DIAGNOSTIC_KIND)
    return DK_UNSPECIFIED;

  old_kind = m_classify_diagnostic[option_id.m_idx];

  /* Handle pragmas separately, since we need to keep track of *where*
     the pragmas were.  */
  if (where != UNKNOWN_LOCATION)
    {
      unsigned i;

      /* Record the command-line status, so we can reset it back on DK_POP. */
      if (old_kind == DK_UNSPECIFIED)
	{
	  old_kind = (!context->option_enabled_p (option_id)
		      ? DK_IGNORED : DK_ANY);
	  m_classify_diagnostic[option_id.m_idx] = old_kind;
	}

      diagnostic_classification_change_t *p;
      FOR_EACH_VEC_ELT_REVERSE (m_classification_history, i, p)
	if (p->option == option_id.m_idx)
	  {
	    old_kind = p->kind;
	    break;
	  }

      diagnostic_classification_change_t v
	= { where, option_id.m_idx, new_kind };
      m_classification_history.safe_push (v);
    }
  else
    m_classify_diagnostic[option_id.m_idx] = new_kind;

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
  if (m_classification_history.is_empty ())
    return DK_UNSPECIFIED;

  /* Iterate over the locations, checking the diagnostic disposition
     for the diagnostic at each.  If it's explicitly set as opposed
     to unspecified, update the disposition for this instance of
     the diagnostic and return it.  */
  for (location_t loc: diagnostic->m_iinfo.m_ilocs)
    {
      /* FIXME: Stupid search.  Optimize later. */
      unsigned int i;
      diagnostic_classification_change_t *p;
      FOR_EACH_VEC_ELT_REVERSE (m_classification_history, i, p)
	{
	  location_t pragloc = p->location;
	  if (!linemap_location_before_p (line_table, pragloc, loc))
	    continue;

	  if (p->kind == (int) DK_POP)
	    {
	      /* Move on to the next region.  */
	      i = p->option;
	      continue;
	    }

	  diagnostic_option_id option = p->option;
	  /* The option 0 is for all the diagnostics.  */
	  if (option == 0 || option == diagnostic->option_id)
	    {
	      diagnostic_t kind = p->kind;
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

/* Returns whether a DIAGNOSTIC should be printed, and adjusts diagnostic->kind
   as appropriate for #pragma GCC diagnostic and -Werror=foo.  */

bool
diagnostic_context::diagnostic_enabled (diagnostic_info *diagnostic)
{
  /* Update the inlining stack for this diagnostic.  */
  get_any_inlining_info (diagnostic);

  /* Diagnostics with no option or -fpermissive are always enabled.  */
  if (!diagnostic->option_id.m_idx
      || diagnostic->option_id == m_opt_permissive)
    return true;

  /* This tests if the user provided the appropriate -Wfoo or
     -Wno-foo option.  */
  if (!option_enabled_p (diagnostic->option_id))
    return false;

  /* This tests for #pragma diagnostic changes.  */
  diagnostic_t diag_class
    = m_option_classifier.update_effective_level_from_pragmas (diagnostic);

  /* This tests if the user provided the appropriate -Werror=foo
     option.  */
  if (diag_class == DK_UNSPECIFIED
      && !option_unspecified_p (diagnostic->option_id))
    {
      const diagnostic_t new_kind
	= m_option_classifier.get_current_override (diagnostic->option_id);
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

/* Returns whether warning OPTION_ID is enabled at LOC.  */

bool
diagnostic_context::warning_enabled_at (location_t loc,
					diagnostic_option_id option_id)
{
  if (!diagnostic_report_warnings_p (this, loc))
    return false;

  rich_location richloc (line_table, loc);
  diagnostic_info diagnostic = {};
  diagnostic.option_id = option_id;
  diagnostic.richloc = &richloc;
  diagnostic.message.m_richloc = &richloc;
  diagnostic.kind = DK_WARNING;
  return diagnostic_enabled (&diagnostic);
}

/* Emit a diagnostic within a diagnostic group on this context.  */

bool
diagnostic_context::
emit_diagnostic_with_group (diagnostic_t kind,
			    rich_location &richloc,
			    const diagnostic_metadata *metadata,
			    diagnostic_option_id option_id,
			    const char *gmsgid, ...)
{
  begin_group ();

  va_list ap;
  va_start (ap, gmsgid);
  bool ret = emit_diagnostic_with_group_va (kind, richloc, metadata, option_id,
					    gmsgid, &ap);
  va_end (ap);

  end_group ();

  return ret;
}

/* As above, but taking a va_list *.  */

bool
diagnostic_context::
emit_diagnostic_with_group_va (diagnostic_t kind,
			       rich_location &richloc,
			       const diagnostic_metadata *metadata,
			       diagnostic_option_id option_id,
			       const char *gmsgid, va_list *ap)
{
  begin_group ();

  bool ret = diagnostic_impl (&richloc, metadata, option_id,
			      gmsgid, ap, kind);

  end_group ();

  return ret;
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

  /* Every call to report_diagnostic should be within a
     begin_group/end_group pair so that output formats can reliably
     flush diagnostics with on_end_group when the topmost group is ended.  */
  gcc_assert (m_diagnostic_groups.m_nesting_depth > 0);

  /* Give preference to being able to inhibit warnings, before they
     get reclassified to something else.  */
  bool was_warning = (diagnostic->kind == DK_WARNING
		      || diagnostic->kind == DK_PEDWARN);
  if (was_warning && m_inhibit_warnings)
    return false;

  if (m_adjust_diagnostic_info)
    m_adjust_diagnostic_info (this, diagnostic);

  if (diagnostic->kind == DK_PEDWARN)
    {
      diagnostic->kind = m_pedantic_errors ? DK_ERROR : DK_WARNING;

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
	pp_newline_and_flush (m_printer);
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
    check_max_errors (false);

  m_lock++;

  if (diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
    {
      /* When not checking, ICEs are converted to fatal errors when an
	 error has already occurred.  This is counteracted by
	 abort_on_error.  */
      if (!CHECKING_P
	  && (diagnostic_count (DK_ERROR) > 0
	      || diagnostic_count (DK_SORRY) > 0)
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

  /* Increment the counter for the appropriate diagnostic kind, either
     within this context, or within the diagnostic_buffer.  */
  {
    const diagnostic_t kind_for_count =
      ((diagnostic->kind == DK_ERROR && orig_diag_kind == DK_WARNING)
       ? DK_WERROR
       : diagnostic->kind);
    diagnostic_counters &counters
      = (m_diagnostic_buffer
	 ? m_diagnostic_buffer->m_diagnostic_counters
	 : m_diagnostic_counters);
    ++counters.m_count_for_kind[kind_for_count];
  }

  /* Is this the initial diagnostic within the stack of groups?  */
  if (m_diagnostic_groups.m_emission_count == 0)
    m_output_format->on_begin_group ();
  m_diagnostic_groups.m_emission_count++;

  /* Run phases 1 and 2 of formatting the message.
     In particular, some format codes may have side-effects here which need to
     happen before sending the diagnostic to the output format.

     For example, Fortran's %C and %L formatting codes populate the
     rich_location.  */
  pp_format (m_printer, &diagnostic->message);

  /* Call vfunc in the output format.  This is responsible for
     phase 3 of formatting, and for printing the result.  */
  m_output_format->on_report_diagnostic (*diagnostic, orig_diag_kind);

  switch (m_extra_output_kind)
    {
    default:
      break;
    case EXTRA_DIAGNOSTIC_OUTPUT_fixits_v1:
      print_parseable_fixits (get_file_cache (),
			      m_printer, diagnostic->richloc,
			      DIAGNOSTICS_COLUMN_UNIT_BYTE,
			      m_tabstop);
      pp_flush (m_printer);
      break;
    case EXTRA_DIAGNOSTIC_OUTPUT_fixits_v2:
      print_parseable_fixits (get_file_cache (),
			      m_printer, diagnostic->richloc,
			      DIAGNOSTICS_COLUMN_UNIT_DISPLAY,
			      m_tabstop);
      pp_flush (m_printer);
      break;
    }
  if (m_diagnostic_buffer == nullptr
      || diagnostic->kind == DK_ICE
      || diagnostic->kind == DK_ICE_NOBT)
    action_after_output (diagnostic->kind);
  diagnostic->x_data = NULL;

  if (m_edit_context_ptr)
    if (diagnostic->richloc->fixits_can_be_auto_applied_p ())
      if (!m_diagnostic_buffer)
	m_edit_context_ptr->add_fixits (diagnostic->richloc);

  m_lock--;

  if (!m_diagnostic_buffer)
    m_output_format->after_diagnostic (*diagnostic);

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

/* Implement emit_diagnostic, inform, warning, warning_at, pedwarn,
   permerror, error, error_at, error_at, sorry, fatal_error, internal_error,
   and internal_error_no_backtrace, as documented and defined below.  */
bool
diagnostic_context::diagnostic_impl (rich_location *richloc,
				     const diagnostic_metadata *metadata,
				     diagnostic_option_id option_id,
				     const char *gmsgid,
				     va_list *ap, diagnostic_t kind)
{
  diagnostic_info diagnostic;
  if (kind == DK_PERMERROR)
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc,
			   m_permissive ? DK_WARNING : DK_ERROR);
      diagnostic.option_id = (option_id.m_idx != -1 ? option_id : m_opt_permissive);
    }
  else
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc, kind);
      if (kind == DK_WARNING || kind == DK_PEDWARN)
	diagnostic.option_id = option_id;
    }
  diagnostic.metadata = metadata;
  return report_diagnostic (&diagnostic);
}

/* Implement inform_n, warning_n, and error_n, as documented and
   defined below.  */
bool
diagnostic_context::diagnostic_n_impl (rich_location *richloc,
				       const diagnostic_metadata *metadata,
				       diagnostic_option_id option_id,
				       unsigned HOST_WIDE_INT n,
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
    diagnostic.option_id = option_id;
  diagnostic.metadata = metadata;
  return report_diagnostic (&diagnostic);
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

/* Inform the user that an error occurred while trying to report some
   other error.  This indicates catastrophic internal inconsistencies,
   so give up now.  But do try to flush out the previous error.
   This mustn't use internal_error, that will cause infinite recursion.  */

void
diagnostic_context::error_recursion ()
{
  if (m_lock < 3)
    pp_newline_and_flush (m_printer);

  fnotice (stderr,
	   "internal compiler error: error reporting routines re-entered.\n");

  /* Call action_after_output to get the "please submit a bug report"
     message.  */
  action_after_output (DK_ICE);

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
  if (global_dc->m_printer == nullptr)
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

void
diagnostic_output_format::dump (FILE *, int) const
{
  /* No-op for now.  */
}

/* Set the output format for CONTEXT to FORMAT, using BASE_FILE_NAME for
   file-based output formats.  */

void
diagnostic_output_format_init (diagnostic_context &context,
			       const char *main_input_filename_,
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
						  line_table,
						  main_input_filename_,
						  json_formatting,
						  sarif_version::v2_1_0);
      break;

    case DIAGNOSTICS_OUTPUT_FORMAT_SARIF_FILE:
      diagnostic_output_format_init_sarif_file (context,
						line_table,
						main_input_filename_,
						json_formatting,
						sarif_version::v2_1_0,
						base_file_name);
      break;
    case DIAGNOSTICS_OUTPUT_FORMAT_SARIF_FILE_2_2_PRERELEASE:
      diagnostic_output_format_init_sarif_file
	(context,
	 line_table,
	 main_input_filename_,
	 json_formatting,
	 sarif_version::v2_2_prerelease_2024_08_08,
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

/* If BUFFER is non-null, use BUFFER as the active diagnostic_buffer on
   this context.  BUFFER is borrowed.

   If BUFFER is null, stop any buffering on this context until the next call
   to this function.  */

void
diagnostic_context::set_diagnostic_buffer (diagnostic_buffer *buffer)
{
  /* We don't allow changing buffering within a diagnostic group
     (to simplify handling of buffered diagnostics within the
     diagnostic_format implementations).  */
  gcc_assert (m_diagnostic_groups.m_nesting_depth == 0);

  m_diagnostic_buffer = buffer;

  gcc_assert (m_output_format);
  if (buffer)
    {
      buffer->ensure_per_format_buffer ();
      gcc_assert (buffer->m_per_format_buffer);
      m_output_format->set_buffer (buffer->m_per_format_buffer.get ());
    }
  else
    m_output_format->set_buffer (nullptr);
}

/* Clear BUFFER without flushing it.  */

void
diagnostic_context::clear_diagnostic_buffer (diagnostic_buffer &buffer)
{
  if (buffer.m_per_format_buffer)
    buffer.m_per_format_buffer->clear ();
  buffer.m_diagnostic_counters.clear ();

  /* We need to reset last_location, otherwise we may skip caret lines
     when we actually give a diagnostic.  */
  m_last_location = UNKNOWN_LOCATION;
}

/* Flush the diagnostics in BUFFER to this context, clearing BUFFER.  */

void
diagnostic_context::flush_diagnostic_buffer (diagnostic_buffer &buffer)
{
  bool had_errors
    = (buffer.m_diagnostic_counters.m_count_for_kind[DK_ERROR] > 0
       || buffer.m_diagnostic_counters.m_count_for_kind[DK_WERROR] > 0);
  if (buffer.m_per_format_buffer)
    buffer.m_per_format_buffer->flush ();
  buffer.m_diagnostic_counters.move_to (m_diagnostic_counters);

  action_after_output (had_errors ? DK_ERROR : DK_WARNING);
  check_max_errors (true);
}

/* struct diagnostic_counters.  */

diagnostic_counters::diagnostic_counters ()
{
  clear ();
}

void
diagnostic_counters::dump (FILE *out, int indent) const
{
  fprintf (out, "%*scounts:\n", indent, "");
  bool none = true;
  for (int i = 0; i < DK_LAST_DIAGNOSTIC_KIND; i++)
    if (m_count_for_kind[i] > 0)
      {
	fprintf (out, "%*s%s%i\n",
		 indent + 2, "",
		 get_diagnostic_kind_text (static_cast<diagnostic_t> (i)),
		 m_count_for_kind[i]);
	none = false;
      }
  if (none)
    fprintf (out, "%*s(none)\n", indent + 2, "");
}

void
diagnostic_counters::move_to (diagnostic_counters &dest)
{
  for (int i = 0; i < DK_LAST_DIAGNOSTIC_KIND; i++)
    dest.m_count_for_kind[i] += m_count_for_kind[i];
  clear ();
}

void
diagnostic_counters::clear ()
{
  memset (&m_count_for_kind, 0, sizeof m_count_for_kind);
}

/* class diagnostic_buffer.  */

diagnostic_buffer::diagnostic_buffer (diagnostic_context &ctxt)
: m_ctxt (ctxt)
{
}

void
diagnostic_buffer::dump (FILE *out, int indent) const
{
  fprintf (out, "%*sm_per_format_buffer:\n", indent, "");
  m_diagnostic_counters.dump (out, indent + 2);
  if (m_per_format_buffer)
    m_per_format_buffer->dump (out, indent + 2);
  else
    fprintf (out, "%*s(none)\n", indent + 2, "");
}

bool
diagnostic_buffer::empty_p () const
{
  if (m_per_format_buffer)
    return m_per_format_buffer->empty_p ();
  else
    return true;
}

void
diagnostic_buffer::move_to (diagnostic_buffer &dest)
{
  ensure_per_format_buffer ();
  dest.ensure_per_format_buffer ();
  m_per_format_buffer->move_to (*dest.m_per_format_buffer);
  m_diagnostic_counters.move_to (dest.m_diagnostic_counters);
}

/* Lazily get output format to create its own kind of buffer.  */

void
diagnostic_buffer::ensure_per_format_buffer ()
{
  if (!m_per_format_buffer)
    {
      gcc_assert (m_ctxt.get_output_format ());
      m_per_format_buffer
	= m_ctxt.get_output_format ()->make_per_format_buffer ();
    }
  gcc_assert (m_per_format_buffer);
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
     diagnostic_column_policy::get_location_text (..., SHOW_COLUMN, ...)
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
  dc.m_column_unit = column_unit;
  dc.m_column_origin = origin;

  expanded_location xloc;
  xloc.file = filename;
  xloc.line = line;
  xloc.column = column;
  xloc.data = NULL;
  xloc.sysp = false;

  diagnostic_column_policy column_policy (dc);
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
  test_get_location_text ();
  test_num_digits ();
}

} // namespace selftest

#endif /* #if CHECKING_P */

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
