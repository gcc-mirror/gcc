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
#include "location.h"

/* The type of a text to be formatted according a format specification
   along with a list of things.  */
typedef struct
{
  const char *format_spec;
  va_list *args_ptr;
} text_info;

/* Contants used to discreminate diagnostics.  */
typedef enum
{
#define DEFINE_DIAGNOSTIC_KIND(K, M) K,  
#include "diagnostic.def"
#undef DEFINE_DIAGNOSTIC_KIND
  DK_LAST_DIAGNOSTIC_KIND
} diagnostic_t;

/* A diagnostic is described by the MESSAGE to send, the FILE and LINE of
   its context and its KIND (ice, error, warning, note, ...)  See complete
   list in diagnostic.def.  */
typedef struct
{
  text_info message;
  location_t location;
  /* The kind of diagnostic it is about.  */
  diagnostic_t kind;
} diagnostic_info;

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
} output_state;

/* The type of a hook that formats client-specific data (trees mostly) into
   an output_buffer.  A client-supplied formatter returns true if everything
   goes well.  */
typedef struct output_buffer output_buffer;
typedef bool (*printer_fn) PARAMS ((output_buffer *, text_info *));

/* The output buffer datatype.  This is best seen as an abstract datatype
   whose fields should not be accessed directly by clients.  */
struct output_buffer
{
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

  /* If non-NULL, this function formats a TEXT into the BUFFER. When called,
     TEXT->format_spec points to a format code.  FORMAT_DECODER should call
     output_add_string (and related functions) to add data to the BUFFER.
     FORMAT_DECODER can read arguments from *TEXT->args_pts using VA_ARG.
     If the BUFFER needs additional characters from the format string, it
     should advance the TEXT->format_spec as it goes.  When FORMAT_DECODER
     returns, TEXT->format_spec should point to the last character processed.
  */
  printer_fn format_decoder;
} ;

#define output_prefix(BUFFER) (BUFFER)->state.prefix

/* The stream attached to the output_buffer, where the formatted
   diagnostics will ultimately go.  Works only on `output_buffer *'.  */
#define output_buffer_attached_stream(BUFFER) (BUFFER)->stream

/* In line-wrapping mode, whether we should start a new line.  */
#define output_needs_newline(BUFFER) (BUFFER)->state.need_newline_p

/* The amount of whitespace to be emitted when starting a new line.  */
#define output_indentation(BUFFER) (BUFFER)->state.indent_skip

/* A pointer to the formatted diagnostic message.  */
#define output_message_text(BUFFER) \
   ((const char *) obstack_base (&(BUFFER)->obstack))

/* Client supplied function used to decode formats.  */
#define output_format_decoder(BUFFER)     (BUFFER)->format_decoder

/* Prefixing rule used in formatting a diagnostic message.  */
#define output_prefixing_rule(BUFFER)  (BUFFER)->state.prefixing_rule

/* Maximum characters per line in automatic line wrapping mode.
   Zero means don't wrap lines.  */
#define output_line_cutoff(BUFFER)  (BUFFER)->state.ideal_maximum_length

/* True if BUFFER is in line-wrapping mode.  */
#define output_is_line_wrapping(BUFFER) (output_line_cutoff (BUFFER) > 0)

#define output_formatted_scalar(BUFFER, FORMAT, INTEGER)	\
  do								\
    {								\
      sprintf ((BUFFER)->digit_buffer, FORMAT, INTEGER);	\
      output_add_string (BUFFER, (BUFFER)->digit_buffer);	\
    }								\
  while (0)

/*  Forward declarations.  */
typedef struct diagnostic_context diagnostic_context;
typedef void (*diagnostic_starter_fn) PARAMS ((diagnostic_context *,
                                               diagnostic_info *));
typedef diagnostic_starter_fn diagnostic_finalizer_fn;

/* This data structure bundles altogether any information relevant to
   the context of a diagnostic message.  */
struct diagnostic_context
{
  /* Where most of the diagnostic formatting work is done.  In Object
     Oriented terms, we'll say that diagnostic_context is a sub-class of
     output_buffer.  */
  output_buffer buffer;

  /* The number of times we have issued diagnostics.  */
  int diagnostic_count[DK_LAST_DIAGNOSTIC_KIND];

  /* True if we should display the "warnings are being tread as error"
     message, usually displayed once per compiler run.  */
  bool warnings_are_errors_message;

  /* This function is called before any message is printed out.  It is
     responsible for preparing message prefix and such.  For example, it
     might say:
     In file included from "/usr/local/include/curses.h:5:
                      from "/home/gdr/src/nifty_printer.h:56:
                      ...
  */
  diagnostic_starter_fn begin_diagnostic;

  /* This function is called after the diagnostic message is printed.  */
  diagnostic_finalizer_fn end_diagnostic;

  /* Client hook to report an internal error.  */
  void (*internal_error) PARAMS ((const char *, va_list *));

  /* Function of last diagnostic message; more generally, function such that
     if next diagnostic message is in it then we don't have to mention the
     function name.  */
  tree last_function;

  /* Used to detect when input_file_stack has changed since last described.  */
  int last_module;

  int lock;
  
  /* Hook for front-end extensions.  */
  void *x_data;
};

/* Client supplied function to announce a diagnostic.  */
#define diagnostic_starter(DC) (DC)->begin_diagnostic

/* Client supplied function called after a diagnostic message is
   displayed.  */
#define diagnostic_finalizer(DC) (DC)->end_diagnostic

/* Extension hook for client.  */
#define diagnostic_auxiliary_data(DC) (DC)->x_data

/* Same as output_format_decoder.  Works on 'diagnostic_context *'.  */
#define diagnostic_format_decoder(DC) output_format_decoder (&(DC)->buffer)

/* Same as output_prefixing_rule.  Works on 'diagnostic_context *'.  */
#define diagnostic_prefixing_rule(DC) output_prefixing_rule (&(DC)->buffer)

/* Maximum characters per line in automatic line wrapping mode.
   Zero means don't wrap lines.  */
#define diagnostic_line_cutoff(DC) output_line_cutoff (&(DC)->buffer)

/* True if the last function in which a diagnostic was reported is
   different from the current one.  */
#define diagnostic_last_function_changed(DC) \
  ((DC)->last_function != current_function_decl)

/* Remember the current function as being the last one in which we report
   a diagnostic.  */
#define diagnostic_set_last_function(DC) \
  (DC)->last_function = current_function_decl

/* True if the last module or file in which a diagnostic was reported is
   different from the current one.  */
#define diagnostic_last_module_changed(DC) \
  ((DC)->last_module != input_file_stack_tick)

/* Remember the current module or file as being the last one in which we
   report a diagnostic.  */
#define diagnostic_set_last_module(DC) \
  (DC)->last_module = input_file_stack_tick

/* This diagnostic_context is used by front-ends that directly output
   diagnostic messages without going through `error', `warning',
   and similar functions.  */
extern diagnostic_context *global_dc;

/* The total count of a KIND of diagnostics meitted so far.  */
#define diagnostic_kind_count(DC, DK) (DC)->diagnostic_count[(int) (DK)]

/* The number of errors that have been issued so far.  Ideally, these
   would take a diagnostic_context as an argument.  */
#define errorcount diagnostic_kind_count (global_dc, DK_ERROR)
/* Similarly, but for warnings.  */
#define warningcount diagnostic_kind_count (global_dc, DK_WARNING)
/* Similarly, but for sorrys.  */
#define sorrycount diagnostic_kind_count (global_dc, DK_SORRY)

/* Returns nonzero if warnings should be emitted.  */
#define diagnostic_report_warnings_p()			\
  (!inhibit_warnings					\
   && !(in_system_header && !warn_system_headers))

#define report_diagnostic(D) diagnostic_report_diagnostic (global_dc, D)

/* Dignostic related functions.  */
extern void diagnostic_initialize	PARAMS ((diagnostic_context *));
extern void diagnostic_report_current_module PARAMS ((diagnostic_context *));
extern void diagnostic_report_current_function PARAMS ((diagnostic_context *));
extern void diagnostic_flush_buffer	PARAMS ((diagnostic_context *));
extern bool diagnostic_count_diagnostic PARAMS ((diagnostic_context *,
                                                 diagnostic_t));
extern void diagnostic_report_diagnostic PARAMS ((diagnostic_context *,
                                                 diagnostic_info *));
extern void diagnostic_set_info         PARAMS ((diagnostic_info *,
                                                 const char *, va_list *,
                                                 const char *, int,
                                                 diagnostic_t));
extern char *diagnostic_build_prefix    PARAMS ((diagnostic_info *));

/* Pure text formatting support functions.  */
extern void init_output_buffer		PARAMS ((output_buffer *,
						 const char *, int));
extern void output_clear		PARAMS ((output_buffer *));
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
extern void output_add_identifier	PARAMS ((output_buffer *, tree));
extern const char *output_finalize_message PARAMS ((output_buffer *));
extern void output_clear_message_text	PARAMS ((output_buffer *));
extern void output_printf		PARAMS ((output_buffer *, const char *,
						 ...)) ATTRIBUTE_PRINTF_2;
extern void output_verbatim		PARAMS ((output_buffer *, const char *,
						 ...));
extern void verbatim			PARAMS ((const char *, ...));
extern char *file_name_as_prefix	PARAMS ((const char *));
extern void inform                      PARAMS ((const char *, ...));

#endif /* ! GCC_DIAGNOSTIC_H */
