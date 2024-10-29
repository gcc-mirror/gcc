/* Support for buffering diagnostics before flushing them to output format.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_DIAGNOSTIC_BUFFER_H
#define GCC_DIAGNOSTIC_BUFFER_H

#include "diagnostic.h"

class diagnostic_per_format_buffer;
class diagnostic_output_format;
  class diagnostic_text_output_format;

/* Class representing a buffer of zero or more diagnostics that
   have been reported to a diagnostic_context, but which haven't
   yet been flushed.

   A diagnostic_buffer can be:

   * flushed to the diagnostic_context, which issues
   the diagnostics within the buffer to the output format
   and checks for limits such as -fmax-errors=, or

   * moved to another diagnostic_buffer, which moves the diagnostics
   within the first buffer to the other buffer, appending them after any
   existing diagnostics within the destination buffer, emptying the
   source buffer, or

   * cleared, which discards any diagnostics within the buffer
   without issuing them to the output format.

   Since a buffer needs to contain output-format-specific data,
   it's not possible to change the output format of the
   diagnostic_context once any buffers are non-empty.

   To simplify implementing output formats, it's not possible
   to change buffering on a diagnostic_context whilst within a
   diagnostic group.  */

class diagnostic_buffer
{
 public:
  friend class diagnostic_context;

  diagnostic_buffer (diagnostic_context &ctxt);
  ~diagnostic_buffer ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  int diagnostic_count (diagnostic_t kind) const
  {
    return m_diagnostic_counters.get_count (kind);
  }

  bool empty_p () const;

  void move_to (diagnostic_buffer &dest);

 private:
  void ensure_per_format_buffers ();

  diagnostic_context &m_ctxt;
  auto_vec<diagnostic_per_format_buffer *> *m_per_format_buffers;

  /* The number of buffered diagnostics of each kind.  */
  diagnostic_counters m_diagnostic_counters;
};

/* Implementation detail of diagnostic_buffer.

   Abstract base class describing how to represent zero of more
   buffered diagnostics for a particular diagnostic_output_format
   (e.g. text vs SARIF).

   Each diagnostic_output_format subclass should implement its own
   subclass for handling diagnostic_buffer.  */

class diagnostic_per_format_buffer
{
public:
  virtual ~diagnostic_per_format_buffer () {}

  virtual void dump (FILE *out, int indent) const = 0;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  virtual bool empty_p () const = 0;
  virtual void move_to (diagnostic_per_format_buffer &dest) = 0;
  virtual void clear () = 0;
  virtual void flush () = 0;
};

#endif /* ! GCC_DIAGNOSTIC_BUFFER_H */
