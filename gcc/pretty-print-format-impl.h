/* Implementation detail of pp_format.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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

#ifndef GCC_PRETTY_PRINT_FORMAT_IMPL_H
#define GCC_PRETTY_PRINT_FORMAT_IMPL_H

#include "pretty-print.h"

/* The chunk_info data structure forms a stack of the results from the
   first phase of formatting (pp_format) which have not yet been
   output (pp_output_formatted_text).  A stack is necessary because
   the diagnostic starter may decide to generate its own output by way
   of the formatter.  */
class chunk_info
{
  friend class pretty_printer;
  friend class pp_markup::context;

public:
  const char * const *get_args () const { return m_args; }
  quoting_info *get_quoting_info () const { return m_quotes; }

  void append_formatted_chunk (const char *content);

  void pop_from_output_buffer (output_buffer &buf);

private:
  void on_begin_quote (const output_buffer &buf,
		       unsigned chunk_idx,
		       const urlifier *urlifier);

  void on_end_quote (pretty_printer *pp,
		     output_buffer &buf,
		     unsigned chunk_idx,
		     const urlifier *urlifier);

  /* Pointer to previous chunk on the stack.  */
  chunk_info *m_prev;

  /* Array of chunks to output.  Each chunk is a NUL-terminated string.
     In the first phase of formatting, even-numbered chunks are
     to be output verbatim, odd-numbered chunks are format specifiers.
     The second phase replaces all odd-numbered chunks with formatted
     text, and the third phase simply emits all the chunks in sequence
     with appropriate line-wrapping.  */
  const char *m_args[PP_NL_ARGMAX * 2];

  /* If non-null, information on quoted text runs within the chunks
     for use by a urlifier.  */
  quoting_info *m_quotes;
};

#endif /* GCC_PRETTY_PRINT_FORMAT_IMPL_H */
