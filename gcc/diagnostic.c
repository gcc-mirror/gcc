/* Language-independent diagnostic subroutines for the GNU Compiler Collection
   Copyright (C) 1999-2020 Free Software Foundation, Inc.
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
#include "edit-context.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "opts.h"
#include "cpplib.h"

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
  ((DC)->pedantic_errors ? DK_ERROR : DK_WARNING)
#define permissive_error_kind(DC) ((DC)->permissive ? DK_WARNING : DK_ERROR)
#define permissive_error_option(DC) ((DC)->opt_permissive)

/* Prototypes.  */
static bool diagnostic_impl (rich_location *, const diagnostic_metadata *,
			     int, const char *,
			     va_list *, diagnostic_t) ATTRIBUTE_GCC_DIAG(4,0);
static bool diagnostic_n_impl (rich_location *, const diagnostic_metadata *,
			       int, unsigned HOST_WIDE_INT,
			       const char *, const char *, va_list *,
			       diagnostic_t) ATTRIBUTE_GCC_DIAG(6,0);

static void error_recursion (diagnostic_context *) ATTRIBUTE_NORETURN;
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

  context->caret_max_width = value;
}

/* Default implementation of final_cb.  */

static void
default_diagnostic_final_cb (diagnostic_context *context)
{
  /* Some of the errors may actually have been warnings.  */
  if (diagnostic_kind_count (context, DK_WERROR))
    {
      /* -Werror was given.  */
      if (context->warning_as_error_requested)
	pp_verbatim (context->printer,
		     _("%s: all warnings being treated as errors"),
		     progname);
      /* At least one -Werror= was given.  */
      else
	pp_verbatim (context->printer,
		     _("%s: some warnings being treated as errors"),
		     progname);
      pp_newline_and_flush (context->printer);
    }
}

/* Initialize the diagnostic message outputting machinery.  */
void
diagnostic_initialize (diagnostic_context *context, int n_opts)
{
  int i;

  /* Allocate a basic pretty-printer.  Clients will replace this a
     much more elaborated pretty-printer if they wish.  */
  context->printer = XNEW (pretty_printer);
  new (context->printer) pretty_printer ();

  memset (context->diagnostic_count, 0, sizeof context->diagnostic_count);
  context->warning_as_error_requested = false;
  context->n_opts = n_opts;
  context->classify_diagnostic = XNEWVEC (diagnostic_t, n_opts);
  for (i = 0; i < n_opts; i++)
    context->classify_diagnostic[i] = DK_UNSPECIFIED;
  context->show_caret = false;
  diagnostic_set_caret_max_width (context, pp_line_cutoff (context->printer));
  for (i = 0; i < rich_location::STATICALLY_ALLOCATED_RANGES; i++)
    context->caret_chars[i] = '^';
  context->show_cwe = false;
  context->path_format = DPF_NONE;
  context->show_path_depths = false;
  context->show_option_requested = false;
  context->abort_on_error = false;
  context->show_column = false;
  context->pedantic_errors = false;
  context->permissive = false;
  context->opt_permissive = 0;
  context->fatal_errors = false;
  context->dc_inhibit_warnings = false;
  context->dc_warn_system_headers = false;
  context->max_errors = 0;
  context->internal_error = NULL;
  diagnostic_starter (context) = default_diagnostic_starter;
  context->start_span = default_diagnostic_start_span_fn;
  diagnostic_finalizer (context) = default_diagnostic_finalizer;
  context->option_enabled = NULL;
  context->option_state = NULL;
  context->option_name = NULL;
  context->get_option_url = NULL;
  context->last_location = UNKNOWN_LOCATION;
  context->last_module = 0;
  context->x_data = NULL;
  context->lock = 0;
  context->inhibit_notes_p = false;
  context->colorize_source_p = false;
  context->show_labels_p = false;
  context->show_line_numbers_p = false;
  context->min_margin_width = 0;
  context->show_ruler_p = false;
  context->parseable_fixits_p = false;
  context->column_unit = DIAGNOSTICS_COLUMN_UNIT_DISPLAY;
  context->column_origin = 1;
  context->tabstop = 8;
  context->edit_context_ptr = NULL;
  context->diagnostic_group_nesting_depth = 0;
  context->diagnostic_group_emission_count = 0;
  context->begin_group_cb = NULL;
  context->end_group_cb = NULL;
  context->final_cb = default_diagnostic_final_cb;
}

/* Maybe initialize the color support. We require clients to do this
   explicitly, since most clients don't want color.  When called
   without a VALUE, it initializes with DIAGNOSTICS_COLOR_DEFAULT.  */

void
diagnostic_color_init (diagnostic_context *context, int value /*= -1 */)
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
  pp_show_color (context->printer)
    = colorize_init ((diagnostic_color_rule_t) value);
}

/* Initialize URL support within CONTEXT based on VALUE, handling "auto".  */

void
diagnostic_urls_init (diagnostic_context *context, int value /*= -1 */)
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

  context->printer->url_format
    = determine_url_format ((diagnostic_url_rule_t) value);
}

/* Do any cleaning up required after the last diagnostic is emitted.  */

void
diagnostic_finish (diagnostic_context *context)
{
  if (context->final_cb)
    context->final_cb (context);

  diagnostic_file_cache_fini ();

  XDELETEVEC (context->classify_diagnostic);
  context->classify_diagnostic = NULL;

  /* diagnostic_initialize allocates context->printer using XNEW
     and placement-new.  */
  context->printer->~pretty_printer ();
  XDELETE (context->printer);
  context->printer = NULL;

  if (context->edit_context_ptr)
    {
      delete context->edit_context_ptr;
      context->edit_context_ptr = NULL;
    }
}

/* Initialize DIAGNOSTIC, where the message MSG has already been
   translated.  */
void
diagnostic_set_info_translated (diagnostic_info *diagnostic, const char *msg,
				va_list *args, rich_location *richloc,
				diagnostic_t kind)
{
  gcc_assert (richloc);
  diagnostic->message.err_no = errno;
  diagnostic->message.args_ptr = args;
  diagnostic->message.format_spec = msg;
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
   to the requested units and origin.  Return -1 if the column is
   invalid (<= 0).  */
int
diagnostic_converted_column (diagnostic_context *context, expanded_location s)
{
  if (s.column <= 0)
    return -1;

  int one_based_col;
  switch (context->column_unit)
    {
    case DIAGNOSTICS_COLUMN_UNIT_DISPLAY:
      one_based_col = location_compute_display_column (s, context->tabstop);
      break;

    case DIAGNOSTICS_COLUMN_UNIT_BYTE:
      one_based_col = s.column;
      break;

    default:
      gcc_unreachable ();
    }

  return one_based_col + (context->column_origin - 1);
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

/* Return a malloc'd string describing a location e.g. "foo.c:42:10".
   The caller is responsible for freeing the memory.  */

static char *
diagnostic_get_location_text (diagnostic_context *context,
			      expanded_location s)
{
  pretty_printer *pp = context->printer;
  const char *locus_cs = colorize_start (pp_show_color (pp), "locus");
  const char *locus_ce = colorize_stop (pp_show_color (pp));
  const char *file = s.file ? s.file : progname;
  int line = 0;
  int col = -1;
  if (strcmp (file, N_("<built-in>")))
    {
      line = s.line;
      if (context->show_column)
	col = diagnostic_converted_column (context, s);
    }

  const char *line_col = maybe_line_and_column (line, col);
  return build_message_string ("%s%s%s:%s", locus_cs, file,
			       line_col, locus_ce);
}

/* Return a malloc'd string describing a location and the severity of the
   diagnostic, e.g. "foo.c:42:10: error: ".  The caller is responsible for
   freeing the memory.  */
char *
diagnostic_build_prefix (diagnostic_context *context,
			 const diagnostic_info *diagnostic)
{
  static const char *const diagnostic_kind_text[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (T),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
    "must-not-happen"
  };
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

  expanded_location s = diagnostic_expand_location (diagnostic);
  char *location_text = diagnostic_get_location_text (context, s);

  char *result = build_message_string ("%s %s%s%s", location_text,
				       text_cs, text, text_ce);
  free (location_text);
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

  /* Skip functions in diagnostic.c.  */
  if (*pcount == 0
      && filename != NULL
      && strcmp (lbasename (filename), "diagnostic.c") == 0)
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
   with a message.  CONTEXT is the context to check, and FLUSH
   indicates whether a diagnostic_finish call is needed.  */

void
diagnostic_check_max_errors (diagnostic_context *context, bool flush)
{
  if (!context->max_errors)
    return;

  int count = (diagnostic_kind_count (context, DK_ERROR)
	       + diagnostic_kind_count (context, DK_SORRY)
	       + diagnostic_kind_count (context, DK_WERROR));

  if (count >= context->max_errors)
    {
      fnotice (stderr,
	       "compilation terminated due to -fmax-errors=%u.\n",
	       context->max_errors);
      if (flush)
	diagnostic_finish (context);
      exit (FATAL_EXIT_CODE);
    }
}

/* Take any action which is expected to happen after the diagnostic
   is written out.  This function does not always return.  */
void
diagnostic_action_after_output (diagnostic_context *context,
				diagnostic_t diag_kind)
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
      if (context->abort_on_error)
	real_abort ();
      if (context->fatal_errors)
	{
	  fnotice (stderr, "compilation terminated due to -Wfatal-errors.\n");
	  diagnostic_finish (context);
	  exit (FATAL_EXIT_CODE);
	}
      break;

    case DK_ICE:
    case DK_ICE_NOBT:
      {
	struct backtrace_state *state = NULL;
	if (diag_kind == DK_ICE)
	  state = backtrace_create_state (NULL, 0, bt_err_callback, NULL);
	int count = 0;
	if (state != NULL)
	  backtrace_full (state, 2, bt_callback, bt_err_callback,
			  (void *) &count);

	if (context->abort_on_error)
	  real_abort ();

	fnotice (stderr, "Please submit a full bug report,\n"
		 "with preprocessed source if appropriate.\n");
	if (count > 0)
	  fnotice (stderr,
		   ("Please include the complete backtrace "
		    "with any bug report.\n"));
	fnotice (stderr, "See %s for instructions.\n", bug_report_url);

	exit (ICE_EXIT_CODE);
      }

    case DK_FATAL:
      if (context->abort_on_error)
	real_abort ();
      diagnostic_finish (context);
      fnotice (stderr, "compilation terminated.\n");
      exit (FATAL_EXIT_CODE);

    default:
      gcc_unreachable ();
    }
}

/* True if the last module or file in which a diagnostic was reported is
   different from the current one.  */

static bool
last_module_changed_p (diagnostic_context *context,
		       const line_map_ordinary *map)
{
  return context->last_module != map;
}

/* Remember the current module or file as being the last one in which we
   report a diagnostic.  */

static void
set_last_module (diagnostic_context *context, const line_map_ordinary *map)
{
  context->last_module = map;
}

void
diagnostic_report_current_module (diagnostic_context *context, location_t where)
{
  const line_map_ordinary *map = NULL;

  if (pp_needs_newline (context->printer))
    {
      pp_newline (context->printer);
      pp_needs_newline (context->printer) = false;
    }

  if (where <= BUILTINS_LOCATION)
    return;

  linemap_resolve_location (line_table, where,
			    LRK_MACRO_DEFINITION_LOCATION,
			    &map);

  if (map && last_module_changed_p (context, map))
    {
      set_last_module (context, map);
      if (! MAIN_FILE_P (map))
	{
	  bool first = true;
	  expanded_location s = {};
	  do
	    {
	      where = linemap_included_from (map);
	      map = linemap_included_from_linemap (line_table, map);
	      s.file = LINEMAP_FILE (map);
	      s.line = SOURCE_LINE (map, where);
	      int col = -1;
	      if (first && context->show_column)
		{
		  s.column = SOURCE_COLUMN (map, where);
		  col = diagnostic_converted_column (context, s);
		}
	      const char *line_col = maybe_line_and_column (s.line, col);
	      static const char *const msgs[] =
		{
		 N_("In file included from"),
		 N_("                 from"),
		};
	      unsigned index = !first;
	      pp_verbatim (context->printer, "%s%s %r%s%s%R",
			   first ? "" : ",\n", _(msgs[index]),
			   "locus", s.file, line_col);
	      first = false;
	    }
	  while (! MAIN_FILE_P (map));
	  pp_verbatim (context->printer, ":");
	  pp_newline (context->printer);
	}
    }
}

/* If DIAGNOSTIC has a diagnostic_path and CONTEXT supports printing paths,
   print the path.  */

void
diagnostic_show_any_path (diagnostic_context *context,
			  diagnostic_info *diagnostic)
{
  const diagnostic_path *path = diagnostic->richloc->get_path ();
  if (!path)
    return;

  if (context->print_path)
    context->print_path (context, path);
}

/* Return true if the events in this path involve more than one
   function, or false if it is purely intraprocedural.  */

bool
diagnostic_path::interprocedural_p () const
{
  const unsigned num = num_events ();
  for (unsigned i = 0; i < num; i++)
    {
      if (get_event (i).get_fndecl () != get_event (0).get_fndecl ())
	return true;
      if (get_event (i).get_stack_depth () != get_event (0).get_stack_depth ())
	return true;
    }
  return false;
}

void
default_diagnostic_starter (diagnostic_context *context,
			    diagnostic_info *diagnostic)
{
  diagnostic_report_current_module (context, diagnostic_location (diagnostic));
  pp_set_prefix (context->printer, diagnostic_build_prefix (context,
							    diagnostic));
}

void
default_diagnostic_start_span_fn (diagnostic_context *context,
				  expanded_location exploc)
{
  char *text = diagnostic_get_location_text (context, exploc);
  pp_string (context->printer, text);
  free (text);
  pp_newline (context->printer);
}

void
default_diagnostic_finalizer (diagnostic_context *context,
			      diagnostic_info *diagnostic,
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
diagnostic_classify_diagnostic (diagnostic_context *context,
				int option_index,
				diagnostic_t new_kind,
				location_t where)
{
  diagnostic_t old_kind;

  if (option_index < 0
      || option_index >= context->n_opts
      || new_kind >= DK_LAST_DIAGNOSTIC_KIND)
    return DK_UNSPECIFIED;

  old_kind = context->classify_diagnostic[option_index];

  /* Handle pragmas separately, since we need to keep track of *where*
     the pragmas were.  */
  if (where != UNKNOWN_LOCATION)
    {
      int i;

      /* Record the command-line status, so we can reset it back on DK_POP. */
      if (old_kind == DK_UNSPECIFIED)
	{
	  old_kind = !context->option_enabled (option_index,
					       context->lang_mask,
					       context->option_state)
	    ? DK_IGNORED : (context->warning_as_error_requested
			    ? DK_ERROR : DK_WARNING);
	  context->classify_diagnostic[option_index] = old_kind;
	}

      for (i = context->n_classification_history - 1; i >= 0; i --)
	if (context->classification_history[i].option == option_index)
	  {
	    old_kind = context->classification_history[i].kind;
	    break;
	  }

      i = context->n_classification_history;
      context->classification_history =
	(diagnostic_classification_change_t *) xrealloc (context->classification_history, (i + 1)
							 * sizeof (diagnostic_classification_change_t));
      context->classification_history[i].location = where;
      context->classification_history[i].option = option_index;
      context->classification_history[i].kind = new_kind;
      context->n_classification_history ++;
    }
  else
    context->classify_diagnostic[option_index] = new_kind;

  return old_kind;
}

/* Save all diagnostic classifications in a stack.  */
void
diagnostic_push_diagnostics (diagnostic_context *context, location_t where ATTRIBUTE_UNUSED)
{
  context->push_list = (int *) xrealloc (context->push_list, (context->n_push + 1) * sizeof (int));
  context->push_list[context->n_push ++] = context->n_classification_history;
}

/* Restore the topmost classification set off the stack.  If the stack
   is empty, revert to the state based on command line parameters.  */
void
diagnostic_pop_diagnostics (diagnostic_context *context, location_t where)
{
  int jump_to;
  int i;

  if (context->n_push)
    jump_to = context->push_list [-- context->n_push];
  else
    jump_to = 0;

  i = context->n_classification_history;
  context->classification_history =
    (diagnostic_classification_change_t *) xrealloc (context->classification_history, (i + 1)
						     * sizeof (diagnostic_classification_change_t));
  context->classification_history[i].location = where;
  context->classification_history[i].option = jump_to;
  context->classification_history[i].kind = DK_POP;
  context->n_classification_history ++;
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

/* Implementation of -fdiagnostics-parseable-fixits.  Print a
   machine-parseable version of all fixits in RICHLOC to PP.  */

static void
print_parseable_fixits (pretty_printer *pp, rich_location *richloc)
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
      pp_printf (pp, ":{%i:%i-%i:%i}:",
		 start_exploc.line, start_exploc.column,
		 next_exploc.line, next_exploc.column);
      print_escaped_string (pp, hint->get_string ());
      pp_newline (pp);
    }

  pp_set_prefix (pp, saved_prefix);
}

/* Update the diag_class of DIAGNOSTIC based on its location
   relative to any
     #pragma GCC diagnostic
   directives recorded within CONTEXT.

   Return the new diag_class of DIAGNOSTIC if it was updated, or
   DK_UNSPECIFIED otherwise.  */

static diagnostic_t
update_effective_level_from_pragmas (diagnostic_context *context,
				     diagnostic_info *diagnostic)
{
  diagnostic_t diag_class = DK_UNSPECIFIED;

  if (context->n_classification_history > 0)
    {
      location_t location = diagnostic_location (diagnostic);

      /* FIXME: Stupid search.  Optimize later. */
      for (int i = context->n_classification_history - 1; i >= 0; i --)
	{
	  if (linemap_location_before_p
	      (line_table,
	       context->classification_history[i].location,
	       location))
	    {
	      if (context->classification_history[i].kind == (int) DK_POP)
		{
		  i = context->classification_history[i].option;
		  continue;
		}
	      int option = context->classification_history[i].option;
	      /* The option 0 is for all the diagnostics.  */
	      if (option == 0 || option == diagnostic->option_index)
		{
		  diag_class = context->classification_history[i].kind;
		  if (diag_class != DK_UNSPECIFIED)
		    diagnostic->kind = diag_class;
		  break;
		}
	    }
	}
    }

  return diag_class;
}

/* Generate a URL string describing CWE.  The caller is responsible for
   freeing the string.  */

static char *
get_cwe_url (int cwe)
{
  return xasprintf ("https://cwe.mitre.org/data/definitions/%i.html", cwe);
}

/* If DIAGNOSTIC has a CWE identifier, print it.

   For example, if the diagnostic metadata associates it with CWE-119,
   " [CWE-119]" will be printed, suitably colorized, and with a URL of a
   description of the security issue.  */

static void
print_any_cwe (diagnostic_context *context,
		    const diagnostic_info *diagnostic)
{
  if (diagnostic->metadata == NULL)
    return;

  int cwe = diagnostic->metadata->get_cwe ();
  if (cwe)
    {
      pretty_printer *pp = context->printer;
      char *saved_prefix = pp_take_prefix (context->printer);
      pp_string (pp, " [");
      pp_string (pp, colorize_start (pp_show_color (pp),
				     diagnostic_kind_color[diagnostic->kind]));
      if (pp->url_format != URL_FORMAT_NONE)
	{
	  char *cwe_url = get_cwe_url (cwe);
	  pp_begin_url (pp, cwe_url);
	  free (cwe_url);
	}
      pp_printf (pp, "CWE-%i", cwe);
      pp_set_prefix (context->printer, saved_prefix);
      if (pp->url_format != URL_FORMAT_NONE)
	pp_end_url (pp);
      pp_string (pp, colorize_stop (pp_show_color (pp)));
      pp_character (pp, ']');
    }
}

/* Print any metadata about the option used to control DIAGNOSTIC to CONTEXT's
   printer, e.g. " [-Werror=uninitialized]".
   Subroutine of diagnostic_report_diagnostic.  */

static void
print_option_information (diagnostic_context *context,
			  const diagnostic_info *diagnostic,
			  diagnostic_t orig_diag_kind)
{
  char *option_text;

  option_text = context->option_name (context, diagnostic->option_index,
				      orig_diag_kind, diagnostic->kind);

  if (option_text)
    {
      char *option_url = NULL;
      if (context->get_option_url
	  && context->printer->url_format != URL_FORMAT_NONE)
	option_url = context->get_option_url (context,
					      diagnostic->option_index);
      pretty_printer *pp = context->printer;
      pp_string (pp, " [");
      pp_string (pp, colorize_start (pp_show_color (pp),
				     diagnostic_kind_color[diagnostic->kind]));
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

/* Report a diagnostic message (an error or a warning) as specified by
   DC.  This function is *the* subroutine in terms of which front-ends
   should implement their specific diagnostic handling modules.  The
   front-end independent format specifiers are exactly those described
   in the documentation of output_format.
   Return true if a diagnostic was printed, false otherwise.  */

bool
diagnostic_report_diagnostic (diagnostic_context *context,
			      diagnostic_info *diagnostic)
{
  location_t location = diagnostic_location (diagnostic);
  diagnostic_t orig_diag_kind = diagnostic->kind;

  /* Give preference to being able to inhibit warnings, before they
     get reclassified to something else.  */
  if ((diagnostic->kind == DK_WARNING || diagnostic->kind == DK_PEDWARN)
      && !diagnostic_report_warnings_p (context, location))
    return false;

  if (diagnostic->kind == DK_PEDWARN)
    {
      diagnostic->kind = pedantic_warning_kind (context);
      /* We do this to avoid giving the message for -pedantic-errors.  */
      orig_diag_kind = diagnostic->kind;
    }
 
  if (diagnostic->kind == DK_NOTE && context->inhibit_notes_p)
    return false;

  if (context->lock > 0)
    {
      /* If we're reporting an ICE in the middle of some other error,
	 try to flush out the previous error, then let this one
	 through.  Don't do this more than once.  */
      if ((diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
	  && context->lock == 1)
	pp_newline_and_flush (context->printer);
      else
	error_recursion (context);
    }

  /* If the user requested that warnings be treated as errors, so be
     it.  Note that we do this before the next block so that
     individual warnings can be overridden back to warnings with
     -Wno-error=*.  */
  if (context->warning_as_error_requested
      && diagnostic->kind == DK_WARNING)
    diagnostic->kind = DK_ERROR;

  if (diagnostic->option_index
      && diagnostic->option_index != permissive_error_option (context))
    {
      /* This tests if the user provided the appropriate -Wfoo or
	 -Wno-foo option.  */
      if (! context->option_enabled (diagnostic->option_index,
				     context->lang_mask,
				     context->option_state))
	return false;

      /* This tests for #pragma diagnostic changes.  */
      diagnostic_t diag_class
	= update_effective_level_from_pragmas (context, diagnostic);

      /* This tests if the user provided the appropriate -Werror=foo
	 option.  */
      if (diag_class == DK_UNSPECIFIED
	  && (context->classify_diagnostic[diagnostic->option_index]
	      != DK_UNSPECIFIED))
	diagnostic->kind
	  = context->classify_diagnostic[diagnostic->option_index];

      /* This allows for future extensions, like temporarily disabling
	 warnings for ranges of source code.  */
      if (diagnostic->kind == DK_IGNORED)
	return false;
    }

  if (diagnostic->kind != DK_NOTE)
    diagnostic_check_max_errors (context);

  context->lock++;

  if (diagnostic->kind == DK_ICE || diagnostic->kind == DK_ICE_NOBT)
    {
      /* When not checking, ICEs are converted to fatal errors when an
	 error has already occurred.  This is counteracted by
	 abort_on_error.  */
      if (!CHECKING_P
	  && (diagnostic_kind_count (context, DK_ERROR) > 0
	      || diagnostic_kind_count (context, DK_SORRY) > 0)
	  && !context->abort_on_error)
	{
	  expanded_location s 
	    = expand_location (diagnostic_location (diagnostic));
	  fnotice (stderr, "%s:%d: confused by earlier errors, bailing out\n",
		   s.file, s.line);
	  exit (ICE_EXIT_CODE);
	}
      if (context->internal_error)
	(*context->internal_error) (context,
				    diagnostic->message.format_spec,
				    diagnostic->message.args_ptr);
    }
  if (diagnostic->kind == DK_ERROR && orig_diag_kind == DK_WARNING)
    ++diagnostic_kind_count (context, DK_WERROR);
  else
    ++diagnostic_kind_count (context, diagnostic->kind);

  /* Is this the initial diagnostic within the stack of groups?  */
  if (context->diagnostic_group_emission_count == 0)
    {
      if (context->begin_group_cb)
	context->begin_group_cb (context);
    }
  context->diagnostic_group_emission_count++;

  diagnostic->message.x_data = &diagnostic->x_data;
  diagnostic->x_data = NULL;
  pp_format (context->printer, &diagnostic->message);
  (*diagnostic_starter (context)) (context, diagnostic);
  pp_output_formatted_text (context->printer);
  if (context->show_cwe)
    print_any_cwe (context, diagnostic);
  if (context->show_option_requested)
    print_option_information (context, diagnostic, orig_diag_kind);
  (*diagnostic_finalizer (context)) (context, diagnostic, orig_diag_kind);
  if (context->parseable_fixits_p)
    {
      print_parseable_fixits (context->printer, diagnostic->richloc);
      pp_flush (context->printer);
    }
  diagnostic_action_after_output (context, diagnostic->kind);
  diagnostic->x_data = NULL;

  if (context->edit_context_ptr)
    if (diagnostic->richloc->fixits_can_be_auto_applied_p ())
      context->edit_context_ptr->add_fixits (diagnostic->richloc);

  context->lock--;

  diagnostic_show_any_path (context, diagnostic);

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
   is used by fancy_abort() to print `Internal compiler error in expr.c'
   instead of `Internal compiler error in ../../GCC/gcc/expr.c'.  */

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
  text_info text;
  va_list ap;

  va_start (ap, gmsgid);
  text.err_no = errno;
  text.args_ptr = &ap;
  text.format_spec = _(gmsgid);
  text.x_data = NULL;
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
  if (context->inhibit_notes_p)
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
      diagnostic.option_index = permissive_error_option (global_dc);
    }
  else
    {
      diagnostic_set_info (&diagnostic, gmsgid, ap, richloc, kind);
      if (kind == DK_WARNING || kind == DK_PEDWARN)
	diagnostic.option_index = opt;
    }
  diagnostic.metadata = metadata;
  return diagnostic_report_diagnostic (global_dc, &diagnostic);
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
  return diagnostic_report_diagnostic (global_dc, &diagnostic);
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
   continue.  Note that unless there is debugging value to be had from
   a more specific message, or some other good reason, you should use
   abort () instead of calling this function directly.  */
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

/* Special case error functions.  Most are implemented in terms of the
   above, or should be.  */

/* Print a diagnostic MSGID on FILE.  This is just fprintf, except it
   runs its second argument through gettext.  */
void
fnotice (FILE *file, const char *cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);
  vfprintf (file, _(cmsgid), ap);
  va_end (ap);
}

/* Inform the user that an error occurred while trying to report some
   other error.  This indicates catastrophic internal inconsistencies,
   so give up now.  But do try to flush out the previous error.
   This mustn't use internal_error, that will cause infinite recursion.  */

static void
error_recursion (diagnostic_context *context)
{
  if (context->lock < 3)
    pp_newline_and_flush (context->printer);

  fnotice (stderr,
	   "Internal compiler error: Error reporting routines re-entered.\n");

  /* Call diagnostic_action_after_output to get the "please submit a bug
     report" message.  */
  diagnostic_action_after_output (context, DK_ICE);

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
  internal_error ("in %s, at %s:%d", function, trim_filename (file), line);
}

/* class auto_diagnostic_group.  */

/* Constructor: "push" this group into global_dc.  */

auto_diagnostic_group::auto_diagnostic_group ()
{
  global_dc->diagnostic_group_nesting_depth++;
}

/* Destructor: "pop" this group from global_dc.  */

auto_diagnostic_group::~auto_diagnostic_group ()
{
  if (--global_dc->diagnostic_group_nesting_depth == 0)
    {
      /* Handle the case where we've popped the final diagnostic group.
	 If any diagnostics were emitted, give the context a chance
	 to do something.  */
      if (global_dc->diagnostic_group_emission_count > 0)
	{
	  if (global_dc->end_group_cb)
	    global_dc->end_group_cb (global_dc);
	}
      global_dc->diagnostic_group_emission_count = 0;
    }
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

/* Add an event to this path at LOC within function FNDECL at
   stack depth DEPTH.

   Use m_context's printer to format FMT, as the text of the new
   event.

   Return the id of the new event.  */

diagnostic_event_id_t
simple_diagnostic_path::add_event (location_t loc, tree fndecl, int depth,
				   const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  text_info ti;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  ti.format_spec = _(fmt);
  ti.args_ptr = &ap;
  ti.err_no = 0;
  ti.x_data = NULL;
  ti.m_richloc = &rich_loc;

  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  simple_diagnostic_event *new_event
    = new simple_diagnostic_event (loc, fndecl, depth, pp_formatted_text (pp));
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

/* struct simple_diagnostic_event.  */

/* simple_diagnostic_event's ctor.  */

simple_diagnostic_event::simple_diagnostic_event (location_t loc,
						  tree fndecl,
						  int depth,
						  const char *desc)
: m_loc (loc), m_fndecl (fndecl), m_depth (depth), m_desc (xstrdup (desc))
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
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  print_parseable_fixits (&pp, &richloc);
  ASSERT_STREQ ("", pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits does the right thing if there
   is an insertion fixit hint.  */

static void
test_print_parseable_fixits_insert ()
{
  pretty_printer pp;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  linemap_add (line_table, LC_ENTER, false, "test.c", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
  location_t where = linemap_position_for_column (line_table, 10);
  richloc.add_fixit_insert_before (where, "added content");

  print_parseable_fixits (&pp, &richloc);
  ASSERT_STREQ ("fix-it:\"test.c\":{5:10-5:10}:\"added content\"\n",
		pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits does the right thing if there
   is an removal fixit hint.  */

static void
test_print_parseable_fixits_remove ()
{
  pretty_printer pp;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  linemap_add (line_table, LC_ENTER, false, "test.c", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
  source_range where;
  where.m_start = linemap_position_for_column (line_table, 10);
  where.m_finish = linemap_position_for_column (line_table, 20);
  richloc.add_fixit_remove (where);

  print_parseable_fixits (&pp, &richloc);
  ASSERT_STREQ ("fix-it:\"test.c\":{5:10-5:21}:\"\"\n",
		pp_formatted_text (&pp));
}

/* Verify that print_parseable_fixits does the right thing if there
   is an replacement fixit hint.  */

static void
test_print_parseable_fixits_replace ()
{
  pretty_printer pp;
  rich_location richloc (line_table, UNKNOWN_LOCATION);

  linemap_add (line_table, LC_ENTER, false, "test.c", 0);
  linemap_line_start (line_table, 5, 100);
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);
  source_range where;
  where.m_start = linemap_position_for_column (line_table, 10);
  where.m_finish = linemap_position_for_column (line_table, 20);
  richloc.add_fixit_replace (where, "replacement");

  print_parseable_fixits (&pp, &richloc);
  ASSERT_STREQ ("fix-it:\"test.c\":{5:10-5:21}:\"replacement\"\n",
		pp_formatted_text (&pp));
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
  dc.show_column = show_column;
  dc.column_unit = column_unit;
  dc.column_origin = origin;

  expanded_location xloc;
  xloc.file = filename;
  xloc.line = line;
  xloc.column = column;
  xloc.data = NULL;
  xloc.sysp = false;

  char *actual_loc_text = diagnostic_get_location_text (&dc, xloc);
  ASSERT_STREQ (expected_loc_text, actual_loc_text);
  free (actual_loc_text);
}

/* Verify that diagnostic_get_location_text works as expected.  */

static void
test_diagnostic_get_location_text ()
{
  const char *old_progname = progname;
  progname = "PROGNAME";
  assert_location_text ("PROGNAME:", NULL, 0, 0, true);
  assert_location_text ("<built-in>:", "<built-in>", 42, 10, true);
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
    const int display_width = cpp_display_width (content, line_bytes,
						 def_tabstop);
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
diagnostic_c_tests ()
{
  test_print_escaped_string ();
  test_print_parseable_fixits_none ();
  test_print_parseable_fixits_insert ();
  test_print_parseable_fixits_remove ();
  test_print_parseable_fixits_replace ();
  test_diagnostic_get_location_text ();
  test_num_digits ();

}

} // namespace selftest

#endif /* #if CHECKING_P */

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
