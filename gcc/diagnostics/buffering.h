/* Support for buffering diagnostics before flushing them to output sinks.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTICS_BUFFERING_H
#define GCC_DIAGNOSTICS_BUFFERING_H

#include "diagnostics/counters.h"

namespace diagnostics {

class per_sink_buffer;
class sink;
  class text_sink;

/* Class representing a buffer of zero or more diagnostics that
   have been reported to a diagnostics::context, but which haven't
   yet been flushed.

   A diagnostics::buffer can be:

   * flushed to the diagnostics::context, which issues
   the diagnostics within the buffer to the output sinks
   and checks for limits such as -fmax-errors=, or

   * moved to another diagnostics::buffer, which moves the diagnostics
   within the first buffer to the other buffer, appending them after any
   existing diagnostics within the destination buffer, emptying the
   source buffer, or

   * cleared, which discards any diagnostics within the buffer
   without issuing them to the output sinks.

   Since a buffer needs to contain sink-specific data,
   it's not possible to change the output sink(s) of the
   diagnostics::context once any buffers are non-empty.

   To simplify implementing output sinks, it's not possible
   to change buffering on a diagnostics::context whilst within a
   diagnostic group.  */

class buffer
{
 public:
  friend class context;

  buffer (context &ctxt);
  ~buffer ();

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  int diagnostic_count (enum kind kind) const
  {
    return m_diagnostic_counters.get_count (kind);
  }

  bool empty_p () const;

  void move_to (buffer &dest);

 private:
  void ensure_per_sink_buffers ();

  context &m_ctxt;
  auto_vec<per_sink_buffer *> *m_per_sink_buffers;

  /* The number of buffered diagnostics of each kind.  */
  counters m_diagnostic_counters;
};

/* Implementation detail of diagnostics::buffer.

   Abstract base class describing how to represent zero of more
   buffered diagnostics for a particular diagnostics::sink
   (e.g. text vs SARIF).

   Each diagnostics::sink subclass should implement its own
   subclass for handling diagnostics::buffer.  */

class per_sink_buffer
{
public:
  virtual ~per_sink_buffer () {}

  virtual void dump (FILE *out, int indent) const = 0;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  virtual bool empty_p () const = 0;
  virtual void move_to (per_sink_buffer &dest) = 0;
  virtual void clear () = 0;
  virtual void flush () = 0;
};

} // namespace diagnostics

#endif /* ! GCC_DIAGNOSTICS_BUFFERING_H */
