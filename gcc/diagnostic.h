/* Various declarations for language-independent diagnostics subroutines.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef __GCC_DIAGNOSTIC_H__
#define __GCC_DIAGNOSTIC_H__

#include "obstack.h"

/*  Forward declarations.  */
typedef struct output_buffer output_buffer;
typedef struct diagnostic_context diagnostic_context;
typedef void (*diagnostic_starter_fn) PARAMS ((output_buffer *,
                                               diagnostic_context *));
typedef diagnostic_starter_fn diagnostic_finalizer_fn;

#define DIAGNOSTICS_SHOW_PREFIX_ONCE       0x0
#define DIAGNOSTICS_SHOW_PREFIX_NEVER      0x1
#define DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE 0x2

/* The type of front-end specific hook that formats trees into an
   output_buffer.  A language specific printer returns a truth value if
   everything goes well. */
typedef int (*printer_fn) PARAMS ((output_buffer *));

/* This data structure encapsulates an output_buffer's state.  */
typedef struct
{
  /* The prefix for each new line.   */
  const char *prefix;
  /* The real upper bound of number of characters per line, taking into
     account the case of a very very looong prefix.  */  
  int maximum_length;
  /* The ideal upper bound of number of characters per line, as suggested
     by front-end. */  
  int ideal_maximum_length;

  /* Indentation count.  */
  int indent_skip;

  /* Nonzero if current PREFIX was emitted at least once.  */
  int emitted_prefix_p;

  /* Nonzero means one should emit a newline before outputing anything.  */
  int need_newline_p;

  /* Tells how often current PREFIX should be emitted:
     o DIAGNOSTICS_SHOW_PREFIX_NEVER: never - not yet supported;
     o DIAGNOSTICS_SHOW_PREFIX_ONCE: emit current PREFIX only once;
     o DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE: emit current PREFIX each time
       a physical line is started.  */
  int prefixing_rule;
  /* The current char to output.  Updated by front-end (*format_map) when
     it is called to report front-end printer for a specified format.  */  
  const char *cursor;
  /* A pointer to the variable argument-list for formatting.  */  
  va_list *format_args;
} output_state;

/* The output buffer datatype.  This is best seen as an abstract datatype.  */
struct output_buffer
{
  /* Internal data.  These fields should not be accessed directly by
     front-ends.  */

  /* The obstack where the text is built up.  */  
  struct obstack obstack;
  /* The amount of characters output so far.  */  
  int line_length;
  /* The current state of the buffer.  */
  output_state state;
};

#define output_buffer_text_cursor(BUFFER) (BUFFER)->state.cursor
#define output_buffer_format_args(BUFFER) *((BUFFER)->state.format_args)
#define output_needs_newline(BUFFER) (BUFFER)->state.need_newline_p
#define output_buffer_state(BUFFER) (BUFFER)->state
#define output_indentation(BUFFER) (BUFFER)->state.indent_skip
#define output_message_text(BUFFER) \
   ((const char *) obstack_base (&(BUFFER)->obstack))

/* This data structure bundles altogether any information relevant to
   the context of a diagnostic message.  */
struct diagnostic_context
{
  /* The diagnostic message to output.  */
  const char *message;

  /* A pointer to a variable list of the arguments necessary for the
     purpose of message formatting.  */
  va_list *args_ptr;

  /* The name of the source file involved in the diiagnostic.  */     
  const char *file;

  /* The line-location in the source file.  */
  int line;

  /* Is this message a warning?  */
  int warn;

  /* This function is called before any message is printed out.  It is
     responsible for preparing message prefix and such.  For example, it
     might say:
     In file included from "/usr/local/include/curses.h:5:
                      from "/home/gdr/src/nifty_printer.h:56:
                      ...
  */
  void (*begin_diagnostic) PARAMS ((output_buffer *, diagnostic_context *));

  /* This function is called after the diagnostic message is printed.   */
  void (*end_diagnostic) PARAMS ((output_buffer *, diagnostic_context *));

  /* Hook for front-end extensions.  */
  void *x_data;
};

#define diagnostic_message(DC) (DC)->message
#define diagnostic_argument_list(DC) (DC)->args_ptr
#define diagnostic_file_location(DC) (DC)->file
#define diagnostic_line_location(DC) (DC)->line
#define diagnostic_is_warning(DC) (DC)->warn
#define diagnostic_starter(DC) (DC)->begin_diagnostic
#define diagnostic_finalizer(DC) (DC)->end_diagnostic
#define diagnostic_auxiliary_data(DC) (DC)->x_data

/* If non-NULL, this function formats data in the BUFFER. When called,
   output_buffer_text_cursor (BUFFER) points to a format code.  LANG_PRINTER
   should call output_add_string (and related functions) to add data to
   the BUFFER.  LANG_PRINTER can read arguments from
   output_buffer_format_args (BUFFER) using VA_ARG.  If the BUFFER needs
   additional characters from the format string, it should advance
   the output_buffer_text_cursor (BUFFER) as it goes.  When LANG_PRINTER
   returns, output_buffer_text_cursor (BUFFER) should point to the last
   character processed.  */

extern printer_fn lang_printer;

extern diagnostic_starter_fn lang_diagnostic_starter;
extern diagnostic_finalizer_fn lang_diagnostic_finalizer;

extern int diagnostic_message_length_per_line;

/* This output buffer is used by front-ends that directly output
   diagnostic messages without going through `error', `warning',
   and similar functions.  In general, such usage should be
   avoided.  This global buffer will go away, once all such usage
   has been removed.  */
extern output_buffer *diagnostic_buffer;

/* Prototypes */
extern void set_diagnostic_context	PARAMS ((diagnostic_context *,
						 const char *, va_list *,
						 const char *, int, int));
extern void set_internal_error_function	PARAMS ((void (*)
						 PARAMS ((const char *,
							  va_list *))));
extern void report_diagnostic		PARAMS ((diagnostic_context *));
extern void initialize_diagnostics	PARAMS ((void));
extern void reshape_diagnostic_buffer	PARAMS ((void));
extern void default_initialize_buffer	PARAMS ((output_buffer *));
extern void init_output_buffer		PARAMS ((output_buffer *,
						 const char *, int));
extern void flush_diagnostic_buffer	PARAMS ((void));
extern void output_clear		PARAMS ((output_buffer *));
extern const char *output_get_prefix	PARAMS ((const output_buffer *));
extern const char *output_last_position PARAMS ((const output_buffer *));
extern void output_set_prefix		PARAMS ((output_buffer *,
						 const char *));
extern void output_destroy_prefix	PARAMS ((output_buffer *));
extern void output_set_maximum_length	PARAMS ((output_buffer *, int));
extern void output_emit_prefix		PARAMS ((output_buffer *));
extern void output_add_newline		PARAMS ((output_buffer *));
extern void output_add_space		PARAMS ((output_buffer *));
extern int output_space_left		PARAMS ((const output_buffer *));
extern void output_append		PARAMS ((output_buffer *, const char *,
						 const char *));
extern void output_add_character	PARAMS ((output_buffer *, int));
extern void output_decimal		PARAMS ((output_buffer *, int));
extern void output_add_string		PARAMS ((output_buffer *,
						 const char *));
extern const char *output_finalize_message PARAMS ((output_buffer *));
extern void output_clear_message_text	PARAMS ((output_buffer *));
extern void output_printf		PARAMS ((output_buffer *, const char *,
						 ...)) ATTRIBUTE_PRINTF_2;
extern int output_is_line_wrapping	PARAMS ((output_buffer *));
extern void set_message_prefixing_rule	PARAMS ((int));
extern void output_verbatim		PARAMS ((output_buffer *, const char *,
						 ...)) ATTRIBUTE_PRINTF_2;
extern void verbatim			PARAMS ((const char *, ...))
     ATTRIBUTE_PRINTF_1;
extern char *context_as_prefix		PARAMS ((const char *, int, int));
extern char *file_name_as_prefix	PARAMS ((const char *));
extern int error_module_changed         PARAMS ((void));
extern void record_last_error_module	PARAMS ((void));
extern int error_function_changed	PARAMS ((void));
extern void record_last_error_function	PARAMS ((void));
extern void report_problematic_module	PARAMS ((output_buffer *));     

#endif /* __GCC_DIAGNOSTIC_H__ */
