/* Various declarations for language-independent diagnostics subroutines.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_DIAGNOSTIC_H
#define GCC_DIAGNOSTIC_H

#include "obstack.h"

/*  Forward declarations.  */
typedef struct output_buffer output_buffer;
typedef struct diagnostic_context diagnostic_context;
typedef void (*diagnostic_starter_fn) PARAMS ((output_buffer *,
                                               diagnostic_context *));
typedef diagnostic_starter_fn diagnostic_finalizer_fn;

typedef enum
{
#define DEFINE_DIAGNOSTIC_KIND(K, M) K,  
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
  DK_LAST_DIAGNOSTIC_KIND
} diagnostic_t;

#define pedantic_error_kind() (flag_pedantic_errors ? DK_ERROR : DK_WARNING)

/* How often diagnostics are prefixed by their locations:
   o DIAGNOSTICS_SHOW_PREFIX_NEVER: never - not yet supported;
   o DIAGNOSTICS_SHOW_PREFIX_ONCE: emit only once;
   o DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE: emit each time a physical
   line is started.  */
typedef enum
{
  DIAGNOSTICS_SHOW_PREFIX_ONCE       = 0x0,
  DIAGNOSTICS_SHOW_PREFIX_NEVER      = 0x1,
  DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE = 0x2
} diagnostic_prefixing_rule_t;

/* The type of front-end specific hook that formats trees into an
   output_buffer.  A language specific printer returns a truth value if
   everything goes well.  */
typedef int (*printer_fn) PARAMS ((output_buffer *));

/* This data structure encapsulates an output_buffer's state.  */
typedef struct
{
  /* The prefix for each new line.  */
  const char *prefix;

  /* The real upper bound of number of characters per line, taking into
     account the case of a very very looong prefix.  */  
  int maximum_length;

  /* The ideal upper bound of number of characters per line, as suggested
     by front-end.  */  
  int ideal_maximum_length;

  /* Indentation count.  */
  int indent_skip;

  /* Nonzero if current PREFIX was emitted at least once.  */
  bool emitted_prefix_p;

  /* Nonzero means one should emit a newline before outputing anything.  */
  bool need_newline_p;

  /* Current prefixing rule.  */
  diagnostic_prefixing_rule_t prefixing_rule;

  /* The current char to output.  Updated by front-end (*format_map) when
     it is called to report front-end printer for a specified format.  */  
  const char *cursor;

  /* A pointer to the variable argument-list for formatting.  */  
  va_list *format_args;

  /* The number of times we have issued diagnostics.  */
  int diagnostic_count[DK_LAST_DIAGNOSTIC_KIND];
} output_state;

/* The output buffer datatype.  This is best seen as an abstract datatype.  */
struct output_buffer
{
  /* Internal data.  These fields should not be accessed directly by
     front-ends.  */

  /* The current state of the buffer.  */
  output_state state;

  /* Where to output formatted text.  */
  FILE* stream;

  /* The obstack where the text is built up.  */  
  struct obstack obstack;

  /* The amount of characters output so far.  */  
  int line_length;

  /* This must be large enough to hold any printed integer or
     floating-point value.  */
  char digit_buffer[128];

/* If non-NULL, this function formats data in the BUFFER. When called,
   output_buffer_text_cursor (BUFFER) points to a format code.
   FORMAT_DECODER should call output_add_string (and related functions)
   to add data to the BUFFER.  FORMAT_DECODER can read arguments from
   output_buffer_format_args (BUFFER) using VA_ARG.  If the BUFFER needs
   additional characters from the format string, it should advance
   the output_buffer_text_cursor (BUFFER) as it goes.  When FORMAT_DECODER
   returns, output_buffer_text_cursor (BUFFER) should point to the last
   character processed.  */

  printer_fn format_decoder;
};

/* Current state of the diagnostic_context' output_buffer.  This macro
   accepts both `diagnostic_context *' and `output_buffer *'.  */
#define output_buffer_state(BUFFER) ((output_buffer *)(BUFFER))->state

/* The stream attached to the output_buffer, where the formatted
   diagnostics will ultimately go.  Works only on `output_buffer *'.  */
#define output_buffer_attached_stream(BUFFER) (BUFFER)->stream

/* This points to the beginning of the rest of the diagnostic message
   to be formatted.  Accepts only `output_buffer *'s.  */
#define output_buffer_text_cursor(BUFFER) (BUFFER)->state.cursor

/* The rest of the `variable argument list' not yet processed.
   This macro works on both `output_state *' and `output_buffer *'.  */
#define output_buffer_format_args(BUFFER) \
   *(((output_state *)(BUFFER))->format_args)

/* In line-wrapping mode, whether we should start a new line.  */
#define output_needs_newline(BUFFER) (BUFFER)->state.need_newline_p

/* The amount of whitespace to be emitted when starting a new line.  */
#define output_indentation(BUFFER) (BUFFER)->state.indent_skip

/* A pointer to the formatted diagonstic message.  */
#define output_message_text(BUFFER) \
   ((const char *) obstack_base (&(BUFFER)->obstack))

/* This data structure bundles altogether any information relevant to
   the context of a diagnostic message.  */
struct diagnostic_context
{
  /* Where most of the diagnostic formatting work is done.  In Object
     Oriented terms, we'll say that diagnostic_context is a sub-class of
     output_buffer.  */
  output_buffer buffer;

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

  /* This function is called after the diagnostic message is printed.  */
  void (*end_diagnostic) PARAMS ((output_buffer *, diagnostic_context *));

  /* Hook for front-end extensions.  */
  void *x_data;
};

/* The diagnostic message being formatted.  */
#define diagnostic_message(DC) (DC)->message

/* A pointer to the variable argument list used in a call
   to a diagonstic routine.  */   
#define diagnostic_argument_list(DC) (DC)->args_ptr

/* The program file to which the diagnostic is referring to.  */
#define diagnostic_file_location(DC) (DC)->file

/* The program source line referred to in the diagnostic message.  */
#define diagnostic_line_location(DC) (DC)->line

/* Tell whether the diagnostic message is to be treated as a warning.  */
#define diagnostic_is_warning(DC) (DC)->warn

/* Client supplied function to announce a diagnostic.  */
#define diagnostic_starter(DC) (DC)->begin_diagnostic

/* Client supplied function called after a diagnostic message is
   displayed.  */
#define diagnostic_finalizer(DC) (DC)->end_diagnostic

/* Extension hook for client.  */
#define diagnostic_auxiliary_data(DC) (DC)->x_data

/* Client supplied function used to decode formats.  Can operate on both
 `output_buffer *' and `diagnostic_context *'.  */
#define diagnostic_format_decoder(DC) ((output_buffer *)(DC))->format_decoder

/* Prefixing rule used in formatting a diagnostic message.  Accepts both
   `output_buffer *' and `diagnostic_context *'.  */
#define diagnostic_prefixing_rule(DC) \
   ((output_buffer *)(DC))->state.prefixing_rule

/* Maximum characters per line in automatic line wrapping mode.
   Zero means don't wrap lines.  */
#define diagnostic_line_cutoff(DC) \
   ((output_buffer *)(DC))->state.ideal_maximum_length

/* This diagnostic context is used by front-ends that directly output
   diagnostic messages without going through `error', `warning',
   and similar functions.  */
extern diagnostic_context *global_dc;

/* This will be removed shortly.  */
extern output_buffer *diagnostic_buffer;

/* The total count of a KIND of diagnostics meitted so far.  */
#define diagnostic_kind_count(DC, DK) \
   ((output_buffer *)(DC))->state.diagnostic_count[(int) (DK)]

/* The number of errors that have been issued so far.  Ideally, these
   would take an output_buffer as an argument.  */
#define errorcount diagnostic_kind_count (global_dc, DK_ERROR)
/* Similarly, but for warnings.  */
#define warningcount diagnostic_kind_count (global_dc, DK_WARNING)
/* Similarly, but for sorrys.  */
#define sorrycount diagnostic_kind_count (global_dc, DK_SORRY)

/* Returns non-zero if warnings should be emitted.  */
#define diagnostic_report_warnings_p()			\
  (!inhibit_warnings					\
   && !(in_system_header && !warn_system_headers))


/* Prototypes */
extern void set_diagnostic_context	PARAMS ((diagnostic_context *,
						 const char *, va_list *,
						 const char *, int, int));
extern void set_internal_error_function	PARAMS ((void (*)
						 PARAMS ((const char *,
							  va_list *))));
extern void report_diagnostic		PARAMS ((diagnostic_context *));
extern void diagnostic_initialize	PARAMS ((diagnostic_context *));
extern void init_output_buffer		PARAMS ((output_buffer *,
						 const char *, int));
extern void flush_diagnostic_buffer	PARAMS ((void));
extern void output_clear		PARAMS ((output_buffer *));
extern const char *output_get_prefix	PARAMS ((const output_buffer *));
extern const char *output_last_position PARAMS ((const output_buffer *));
extern void output_set_prefix		PARAMS ((output_buffer *,
						 const char *));
extern void output_destroy_prefix	PARAMS ((output_buffer *));
extern void output_set_maximum_length   PARAMS ((output_buffer *, int));
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

/* Called by report_error_function to print out function name.
 * Default may be overridden by language front-ends.  */
extern void (*print_error_function) PARAMS ((diagnostic_context *,
                                             const char *));

extern void default_print_error_function PARAMS ((diagnostic_context *,
                                                  const char *));

#endif /* ! GCC_DIAGNOSTIC_H */
