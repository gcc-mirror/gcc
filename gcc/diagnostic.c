/* Language-independent diagnostic subroutines for the GNU Compiler Collection
   Copyright (C) 1999-2013 Free Software Foundation, Inc.
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
#include "input.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "diagnostic-color.h"

#include <new>                     // For placement new.

#define pedantic_warning_kind(DC)			\
  ((DC)->pedantic_errors ? DK_ERROR : DK_WARNING)
#define permissive_error_kind(DC) ((DC)->permissive ? DK_WARNING : DK_ERROR)
#define permissive_error_option(DC) ((DC)->opt_permissive)

/* Prototypes.  */
static char *build_message_string (const char *, ...) ATTRIBUTE_PRINTF_1;

static void error_recursion (diagnostic_context *) ATTRIBUTE_NORETURN;

static void diagnostic_action_after_output (diagnostic_context *,
					    diagnostic_info *);
static void real_abort (void) ATTRIBUTE_NORETURN;

/* Name of program invoked, sans directories.  */

const char *progname;

/* A diagnostic_context surrogate for stderr.  */
static diagnostic_context global_diagnostic_context;
diagnostic_context *global_dc = &global_diagnostic_context;

/* Return a malloc'd string containing MSG formatted a la printf.  The
   caller is responsible for freeing the memory.  */
static char *
build_message_string (const char *msg, ...)
{
  char *str;
  va_list ap;

  va_start (ap, msg);
  vasprintf (&str, msg, ap);
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
   value is not set to a positive integer, then return INT_MAX.  */
static int
getenv_columns (void)
{
  const char * s = getenv ("COLUMNS");
  if (s != NULL) {
    int n = atoi (s);
    if (n > 0)
      return n;
  }
  return INT_MAX;
}

/* Set caret_max_width to value.  */
void
diagnostic_set_caret_max_width (diagnostic_context *context, int value)
{
  /* One minus to account for the leading empty space.  */
  value = value ? value - 1 
    : (isatty (fileno (pp_buffer (context->printer)->stream))
       ? getenv_columns () - 1: INT_MAX);
  
  if (value <= 0) 
    value = INT_MAX;

  context->caret_max_width = value;
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
  context->some_warnings_are_errors = false;
  context->warning_as_error_requested = false;
  context->n_opts = n_opts;
  context->classify_diagnostic = XNEWVEC (diagnostic_t, n_opts);
  for (i = 0; i < n_opts; i++)
    context->classify_diagnostic[i] = DK_UNSPECIFIED;
  context->show_caret = false;
  diagnostic_set_caret_max_width (context, pp_line_cutoff (context->printer));
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
  diagnostic_finalizer (context) = default_diagnostic_finalizer;
  context->option_enabled = NULL;
  context->option_state = NULL;
  context->option_name = NULL;
  context->last_location = UNKNOWN_LOCATION;
  context->last_module = 0;
  context->x_data = NULL;
  context->lock = 0;
  context->inhibit_notes_p = false;
}

/* Do any cleaning up required after the last diagnostic is emitted.  */

void
diagnostic_finish (diagnostic_context *context)
{
  /* Some of the errors may actually have been warnings.  */
  if (context->some_warnings_are_errors)
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

/* Initialize DIAGNOSTIC, where the message MSG has already been
   translated.  */
void
diagnostic_set_info_translated (diagnostic_info *diagnostic, const char *msg,
				va_list *args, location_t location,
				diagnostic_t kind)
{
  diagnostic->message.err_no = errno;
  diagnostic->message.args_ptr = args;
  diagnostic->message.format_spec = msg;
  diagnostic->location = location;
  diagnostic->override_column = 0;
  diagnostic->kind = kind;
  diagnostic->option_index = 0;
}

/* Initialize DIAGNOSTIC, where the message GMSGID has not yet been
   translated.  */
void
diagnostic_set_info (diagnostic_info *diagnostic, const char *gmsgid,
		     va_list *args, location_t location,
		     diagnostic_t kind)
{
  diagnostic_set_info_translated (diagnostic, _(gmsgid), args, location, kind);
}

/* Return a malloc'd string describing a location.  The caller is
   responsible for freeing the memory.  */
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
  static const char *const diagnostic_kind_color[] = {
#define DEFINE_DIAGNOSTIC_KIND(K, T, C) (C),
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
    NULL
  };
  const char *text = _(diagnostic_kind_text[diagnostic->kind]);
  const char *text_cs = "", *text_ce = "";
  const char *locus_cs, *locus_ce;
  pretty_printer *pp = context->printer;

  if (diagnostic_kind_color[diagnostic->kind])
    {
      text_cs = colorize_start (pp_show_color (pp),
				diagnostic_kind_color[diagnostic->kind]);
      text_ce = colorize_stop (pp_show_color (pp));
    }
  locus_cs = colorize_start (pp_show_color (pp), "locus");
  locus_ce = colorize_stop (pp_show_color (pp));

  expanded_location s = expand_location_to_spelling_point (diagnostic->location);
  if (diagnostic->override_column)
    s.column = diagnostic->override_column;
  gcc_assert (diagnostic->kind < DK_LAST_DIAGNOSTIC_KIND);

  return
    (s.file == NULL
     ? build_message_string ("%s%s:%s %s%s%s", locus_cs, progname, locus_ce,
			     text_cs, text, text_ce)
     : !strcmp (s.file, N_("<built-in>"))
     ? build_message_string ("%s%s:%s %s%s%s", locus_cs, s.file, locus_ce,
			     text_cs, text, text_ce)
     : context->show_column
     ? build_message_string ("%s%s:%d:%d:%s %s%s%s", locus_cs, s.file, s.line,
			     s.column, locus_ce, text_cs, text, text_ce)
     : build_message_string ("%s%s:%d:%s %s%s%s", locus_cs, s.file, s.line, locus_ce,
			     text_cs, text, text_ce));
}

/* If LINE is longer than MAX_WIDTH, and COLUMN is not smaller than
   MAX_WIDTH by some margin, then adjust the start of the line such
   that the COLUMN is smaller than MAX_WIDTH minus the margin.  The
   margin is either 10 characters or the difference between the column
   and the length of the line, whatever is smaller.  The length of
   LINE is given by LINE_WIDTH.  */
static const char *
adjust_line (const char *line, int line_width,
	     int max_width, int *column_p)
{
  int right_margin = 10;
  int column = *column_p;

  right_margin = MIN (line_width - column, right_margin);
  right_margin = max_width - right_margin;
  if (line_width >= max_width && column > right_margin)
    {
      line += column - right_margin;
      *column_p = right_margin;
    }
  return line;
}

/* Print the physical source line corresponding to the location of
   this diagnostics, and a caret indicating the precise column.  */
void
diagnostic_show_locus (diagnostic_context * context,
		       const diagnostic_info *diagnostic)
{
  const char *line;
  int line_width;
  char *buffer;
  expanded_location s;
  int max_width;
  const char *saved_prefix;
  const char *caret_cs, *caret_ce;

  if (!context->show_caret
      || diagnostic->location <= BUILTINS_LOCATION
      || diagnostic->location == context->last_location)
    return;

  context->last_location = diagnostic->location;
  s = expand_location_to_spelling_point (diagnostic->location);
  line = location_get_source_line (s, &line_width);
  if (line == NULL)
    return;

  max_width = context->caret_max_width;
  line = adjust_line (line, line_width, max_width, &(s.column));

  pp_newline (context->printer);
  saved_prefix = pp_get_prefix (context->printer);
  pp_set_prefix (context->printer, NULL);
  pp_space (context->printer);
  while (max_width > 0 && line_width > 0)
    {
      char c = *line == '\t' ? ' ' : *line;
      if (c == '\0')
	c = ' ';
      pp_character (context->printer, c);
      max_width--;
      line_width--;
      line++;
    }
  pp_newline (context->printer);
  caret_cs = colorize_start (pp_show_color (context->printer), "caret");
  caret_ce = colorize_stop (pp_show_color (context->printer));

  /* pp_printf does not implement %*c.  */
  size_t len = s.column + 3 + strlen (caret_cs) + strlen (caret_ce);
  buffer = XALLOCAVEC (char, len);
  snprintf (buffer, len, "%s %*c%s", caret_cs, s.column, '^', caret_ce);
  pp_string (context->printer, buffer);
  pp_set_prefix (context->printer, saved_prefix);
}

/* Functions at which to stop the backtrace print.  It's not
   particularly helpful to print the callers of these functions.  */

static const char * const bt_stop[] =
{
  "main",
  "toplev_main",
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

/* Take any action which is expected to happen after the diagnostic
   is written out.  This function does not always return.  */
static void
diagnostic_action_after_output (diagnostic_context *context,
				diagnostic_info *diagnostic)
{
  switch (diagnostic->kind)
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
      if (context->max_errors != 0
	  && ((unsigned) (diagnostic_kind_count (context, DK_ERROR)
			  + diagnostic_kind_count (context, DK_SORRY))
	      >= context->max_errors))
	{
	  fnotice (stderr,
		   "compilation terminated due to -fmax-errors=%u.\n",
		   context->max_errors);
	  diagnostic_finish (context);
	  exit (FATAL_EXIT_CODE);
	}
      break;

    case DK_ICE:
      {
	struct backtrace_state *state =
	  backtrace_create_state (NULL, 0, bt_err_callback, NULL);
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

void
diagnostic_report_current_module (diagnostic_context *context, location_t where)
{
  const struct line_map *map = NULL;

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

  if (map && diagnostic_last_module_changed (context, map))
    {
      diagnostic_set_last_module (context, map);
      if (! MAIN_FILE_P (map))
	{
	  map = INCLUDED_FROM (line_table, map);
	  if (context->show_column)
	    pp_verbatim (context->printer,
			 "In file included from %r%s:%d:%d%R", "locus",
			 LINEMAP_FILE (map),
			 LAST_SOURCE_LINE (map), LAST_SOURCE_COLUMN (map));
	  else
	    pp_verbatim (context->printer,
			 "In file included from %r%s:%d%R", "locus",
			 LINEMAP_FILE (map), LAST_SOURCE_LINE (map));
	  while (! MAIN_FILE_P (map))
	    {
	      map = INCLUDED_FROM (line_table, map);
	      pp_verbatim (context->printer,
			   ",\n                 from %r%s:%d%R", "locus",
			   LINEMAP_FILE (map), LAST_SOURCE_LINE (map));
	    }
	  pp_verbatim (context->printer, ":");
	  pp_newline (context->printer);
	}
    }
}

void
default_diagnostic_starter (diagnostic_context *context,
			    diagnostic_info *diagnostic)
{
  diagnostic_report_current_module (context, diagnostic->location);
  pp_set_prefix (context->printer, diagnostic_build_prefix (context,
							    diagnostic));
}

void
default_diagnostic_finalizer (diagnostic_context *context ATTRIBUTE_UNUSED,
			      diagnostic_info *diagnostic ATTRIBUTE_UNUSED)
{
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
  location_t location = diagnostic->location;
  diagnostic_t orig_diag_kind = diagnostic->kind;
  const char *saved_format_spec;

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
      if (diagnostic->kind == DK_ICE && context->lock == 1)
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
    {
      diagnostic->kind = DK_ERROR;
    }

  if (diagnostic->option_index
      && diagnostic->option_index != permissive_error_option (context))
    {
      diagnostic_t diag_class = DK_UNSPECIFIED;

      /* This tests if the user provided the appropriate -Wfoo or
	 -Wno-foo option.  */
      if (! context->option_enabled (diagnostic->option_index,
				     context->option_state))
	return false;

      /* This tests for #pragma diagnostic changes.  */
      if (context->n_classification_history > 0)
	{
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
      /* This tests if the user provided the appropriate -Werror=foo
	 option.  */
      if (diag_class == DK_UNSPECIFIED
	  && context->classify_diagnostic[diagnostic->option_index] != DK_UNSPECIFIED)
	{
	  diagnostic->kind = context->classify_diagnostic[diagnostic->option_index];
	}
      /* This allows for future extensions, like temporarily disabling
	 warnings for ranges of source code.  */
      if (diagnostic->kind == DK_IGNORED)
	return false;
    }

  if (orig_diag_kind == DK_WARNING && diagnostic->kind == DK_ERROR)
    context->some_warnings_are_errors = true;

  context->lock++;

  if (diagnostic->kind == DK_ICE)
    {
#ifndef ENABLE_CHECKING
      /* When not checking, ICEs are converted to fatal errors when an
	 error has already occurred.  This is counteracted by
	 abort_on_error.  */
      if ((diagnostic_kind_count (context, DK_ERROR) > 0
	   || diagnostic_kind_count (context, DK_SORRY) > 0)
	  && !context->abort_on_error)
	{
	  expanded_location s = expand_location (diagnostic->location);
	  fnotice (stderr, "%s:%d: confused by earlier errors, bailing out\n",
		   s.file, s.line);
	  exit (ICE_EXIT_CODE);
	}
#endif
      if (context->internal_error)
	(*context->internal_error) (context,
				    diagnostic->message.format_spec,
				    diagnostic->message.args_ptr);
    }
  if (diagnostic->kind == DK_ERROR && orig_diag_kind == DK_WARNING)
    ++diagnostic_kind_count (context, DK_WERROR);
  else
    ++diagnostic_kind_count (context, diagnostic->kind);

  saved_format_spec = diagnostic->message.format_spec;
  if (context->show_option_requested)
    {
      char *option_text;

      option_text = context->option_name (context, diagnostic->option_index,
					  orig_diag_kind, diagnostic->kind);

      if (option_text)
	{
	  diagnostic->message.format_spec
	    = ACONCAT ((diagnostic->message.format_spec,
			" ", 
			"[", option_text, "]",
			NULL));
	  free (option_text);
	}
    }
  diagnostic->message.locus = &diagnostic->location;
  diagnostic->message.x_data = &diagnostic->x_data;
  diagnostic->x_data = NULL;
  pp_format (context->printer, &diagnostic->message);
  (*diagnostic_starter (context)) (context, diagnostic);
  pp_output_formatted_text (context->printer);
  diagnostic_show_locus (context, diagnostic);
  (*diagnostic_finalizer (context)) (context, diagnostic);
  pp_destroy_prefix (context->printer);
  pp_newline_and_flush (context->printer);
  diagnostic_action_after_output (context, diagnostic);
  diagnostic->message.format_spec = saved_format_spec;
  diagnostic->x_data = NULL;

  context->lock--;

  return true;
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
  text.locus = NULL;
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
  const char *saved_prefix;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location, DK_NOTE);
  if (context->inhibit_notes_p)
    {
      va_end (ap);
      return;
    }
  saved_prefix = pp_get_prefix (context->printer);
  pp_set_prefix (context->printer,
                 diagnostic_build_prefix (context, &diagnostic));
  pp_newline (context->printer);
  pp_format (context->printer, &diagnostic.message);
  pp_output_formatted_text (context->printer);
  pp_destroy_prefix (context->printer);
  pp_set_prefix (context->printer, saved_prefix);
  diagnostic_show_locus (context, &diagnostic);
  va_end (ap);
}

bool
emit_diagnostic (diagnostic_t kind, location_t location, int opt,
		 const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool ret;

  va_start (ap, gmsgid);
  if (kind == DK_PERMERROR)
    {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, location,
			   permissive_error_kind (global_dc));
      diagnostic.option_index = permissive_error_option (global_dc);
    }
  else {
      diagnostic_set_info (&diagnostic, gmsgid, &ap, location, kind);
      if (kind == DK_WARNING || kind == DK_PEDWARN)
	diagnostic.option_index = opt;
  }

  ret = report_diagnostic (&diagnostic);
  va_end (ap);
  return ret;
}

/* An informative note at LOCATION.  Use this for additional details on an error
   message.  */
void
inform (location_t location, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location, DK_NOTE);
  report_diagnostic (&diagnostic);
  va_end (ap);
}

/* An informative note at LOCATION.  Use this for additional details on an
   error message.  */
void
inform_n (location_t location, int n, const char *singular_gmsgid,
          const char *plural_gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, plural_gmsgid);
  diagnostic_set_info_translated (&diagnostic,
                                  ngettext (singular_gmsgid, plural_gmsgid, n),
                                  &ap, location, DK_NOTE);
  report_diagnostic (&diagnostic);
  va_end (ap);
}

/* A warning at INPUT_LOCATION.  Use this for code which is correct according
   to the relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */
bool
warning (int opt, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool ret;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, input_location, DK_WARNING);
  diagnostic.option_index = opt;

  ret = report_diagnostic (&diagnostic);
  va_end (ap);
  return ret;
}

/* A warning at LOCATION.  Use this for code which is correct according to the
   relevant language specification but is likely to be buggy anyway.
   Returns true if the warning was printed, false if it was inhibited.  */

bool
warning_at (location_t location, int opt, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool ret;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location, DK_WARNING);
  diagnostic.option_index = opt;
  ret = report_diagnostic (&diagnostic);
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
  diagnostic_info diagnostic;
  va_list ap;
  bool ret;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location,  DK_PEDWARN);
  diagnostic.option_index = opt;
  ret = report_diagnostic (&diagnostic);
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
  diagnostic_info diagnostic;
  va_list ap;
  bool ret;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location,
                       permissive_error_kind (global_dc));
  diagnostic.option_index = permissive_error_option (global_dc);
  ret = report_diagnostic (&diagnostic);
  va_end (ap);
  return ret;
}

/* A hard error: the code is definitely ill-formed, and an object file
   will not be produced.  */
void
error (const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, input_location, DK_ERROR);
  report_diagnostic (&diagnostic);
  va_end (ap);
}

/* A hard error: the code is definitely ill-formed, and an object file
   will not be produced.  */
void
error_n (location_t location, int n, const char *singular_gmsgid,
         const char *plural_gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, plural_gmsgid);
  diagnostic_set_info_translated (&diagnostic,
                                  ngettext (singular_gmsgid, plural_gmsgid, n),
                                  &ap, location, DK_ERROR);
  report_diagnostic (&diagnostic);
  va_end (ap);
}

/* Same as ebove, but use location LOC instead of input_location.  */
void
error_at (location_t loc, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, loc, DK_ERROR);
  report_diagnostic (&diagnostic);
  va_end (ap);
}

/* "Sorry, not implemented."  Use for a language feature which is
   required by the relevant specification but not implemented by GCC.
   An object file will not be produced.  */
void
sorry (const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, input_location, DK_SORRY);
  report_diagnostic (&diagnostic);
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
fatal_error (const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, input_location, DK_FATAL);
  report_diagnostic (&diagnostic);
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
  diagnostic_info diagnostic;
  va_list ap;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, input_location, DK_ICE);
  report_diagnostic (&diagnostic);
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
  diagnostic_info diagnostic;

  if (context->lock < 3)
    pp_newline_and_flush (context->printer);

  fnotice (stderr,
	   "Internal compiler error: Error reporting routines re-entered.\n");

  /* Call diagnostic_action_after_output to get the "please submit a bug
     report" message.  It only looks at the kind field of diagnostic_info.  */
  diagnostic.kind = DK_ICE;
  diagnostic_action_after_output (context, &diagnostic);

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

/* Really call the system 'abort'.  This has to go right at the end of
   this file, so that there are no functions after it that call abort
   and get the system abort instead of our macro.  */
#undef abort
static void
real_abort (void)
{
  abort ();
}
