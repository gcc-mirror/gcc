/* Various declarations for language-independent diagnostics subroutines.
   Copyright (C) 2000 Free Software Foundation, Inc.
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

#define DIAGNOSTICS_SHOW_PREFIX_ONCE       0x0
#define DIAGNOSTICS_SHOW_PREFIX_NEVER      0x1
#define DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE 0x2

/* The type of front-end specific hook that formats trees into an
   output_buffer.  A language specific printer returns a truth value if
   everything goes well. */
typedef int (*printer_fn) PARAMS ((output_buffer *));

/* This data structure encapulates an output_buffer's state.  */
typedef struct
{
  /* The prefix for each new line.   */
  const char *prefix;
  /* The real upper bound of number of characters per line, taking into
     accompt the case of a very very looong prefix.  */  
  int maximum_length;
  /* The ideal upper bound of number of characters per line, as suggested
     by front-end. */  
  int ideal_maximum_length;
  /* Nonzero if current PREFIX was emitted at least once.  */
  int emitted_prefix_p;
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

extern int diagnostic_message_length_per_line;

/* This output buffer is used by front-ends that directly output
   diagnostic messages without going through `error', `warning',
   and simillar functions.  In general, such usage should be
   avoided.  This global buffer will go away, once all such usage
   has been removed.  */
extern output_buffer *diagnostic_buffer;

/* Prototypes */
void report_diagnostic          PARAMS ((const char *, va_list *,
                                         const char *, int, int));
void initialize_diagnostics     PARAMS ((void));
void reshape_diagnostic_buffer  PARAMS ((void));
void default_initialize_buffer  PARAMS ((output_buffer *));
void init_output_buffer		PARAMS ((output_buffer *, const char *, int));
void output_clear		PARAMS ((output_buffer *));
const char *output_get_prefix	PARAMS ((const output_buffer *));
void output_set_prefix		PARAMS ((output_buffer *, const char *));
void output_destroy_prefix      PARAMS ((output_buffer *));
void output_set_maximum_length  PARAMS ((output_buffer *, int));
void output_emit_prefix		PARAMS ((output_buffer *));
void output_add_newline		PARAMS ((output_buffer *));
void output_add_space		PARAMS ((output_buffer *));
int output_space_left		PARAMS ((const output_buffer *));
void output_append		PARAMS ((output_buffer *, const char *,
                                         const char *));
void output_add_character	PARAMS ((output_buffer *, int));
void output_decimal		PARAMS ((output_buffer *, int));
void output_add_string		PARAMS ((output_buffer *, const char *));
const char *output_finish	PARAMS ((output_buffer *));
void output_printf		PARAMS ((output_buffer *, const char *,
                                         ...)) ATTRIBUTE_PRINTF_2;
int output_is_line_wrapping	PARAMS ((output_buffer *));
void set_message_prefixing_rule PARAMS ((int));
void output_verbatim            PARAMS ((output_buffer *, const char *, ...))
     ATTRIBUTE_PRINTF_2;
void verbatim PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;

#endif /* __GCC_DIAGNOSTIC_H__ */
