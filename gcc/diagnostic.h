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

/* The type of front-end specific hook that formats trees into an
   output_buffer.  */
typedef void (*printer_fn) PARAMS ((output_buffer *));

/* The output buffer datatype.  This is best seen as an abstract datatype.  */
struct output_buffer
{
  /* Internal data.  These fields should not be accessed directly by
     front-ends.  */

  /* The obstack where the text is built up.  */  
  struct obstack obstack;
  /* The prefix for each new line.   */
  char *prefix;
  /* The amount of characters output so far.  */  
  int line_length;
  /* The real upper bound of number of characters per line, taking into
     accompt the case of a very very looong prefix.  */  
  int maximum_length;
  /* The ideal upper bound of number of characters per line, as suggested
     by front-end. */  
  int ideal_maximum_length;

  /* Public fields.  These are used by front-ends to extract formats and
     arguments from the variable argument-list passed to output_format.  */

  /* The current char to output.  Updated by front-end (*format_map) when
     it is called to report front-end printer for a specified format.  */  
  const char *cursor;
  /* Variable argument-list for formatting.  */  
  va_list format_args;
};

/* If non-NULL, this function formats data in the BUFFER.
   BUFFER->CURSOR points to a format code.  LANG_PRINTER should
   call output_add_string (and related functions) to add data to
   the BUFFER.  LANG_PRINTER can read arguments from
   BUFFER->FORMAT_ARGS using VA_ARG.  If the BUFFER needs
   additional characters from the format string, it should advance
   the BUFFER->CURSOR as it goes.  When LANG_PRINTER returns,
   BUFFER->CURSOR should point to the last character processed.  */

extern printer_fn lang_printer;

/* Prototypes */
void init_output_buffer		PARAMS ((output_buffer *, char *, int));
void output_clear		PARAMS ((output_buffer *));
char *output_get_prefix		PARAMS ((const output_buffer *));
void output_set_prefix		PARAMS ((output_buffer *, char *));
void output_set_maximum_length  PARAMS ((output_buffer *, int));
void output_emit_prefix		PARAMS ((output_buffer *));
void output_add_newline		PARAMS ((output_buffer *));
void output_add_space		PARAMS ((output_buffer *));
int output_space_left		PARAMS ((const output_buffer *));
void output_append		PARAMS ((output_buffer *, const char *,
                                         const char *));
void output_add_character	PARAMS ((output_buffer *, int));
void output_add_integer		PARAMS ((output_buffer *, HOST_WIDE_INT));
void output_add_string		PARAMS ((output_buffer *, const char *));
const char *output_finish	PARAMS ((output_buffer *));
void output_flush_on		PARAMS ((output_buffer *, FILE *));
void output_printf		PARAMS ((output_buffer *, const char *,
                                         ...)) ATTRIBUTE_PRINTF_2;
void output_format		PARAMS ((output_buffer *, const char *));
int output_is_line_wrapping	PARAMS ((output_buffer *));

#endif /* __GCC_DIAGNOSTIC_H__ */
