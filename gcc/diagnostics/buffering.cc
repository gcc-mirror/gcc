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

#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostics/buffering.h"
#include "diagnostics/sink.h"
#include "diagnostics/dumping.h"

namespace diagnostics {

/* Methods fns of diagnostics::context relating to buffering.  */

/* If BUFFER_ is non-null, use BUFFER as the active diagnostics::buffer on
   this context.  BUFFER is borrowed.

   If BUFFER_ is null, stop any buffering on this context until the next call
   to this function.  */

void
context::set_diagnostic_buffer (buffer *buffer_)
{
  /* Early reject of no-op (to avoid recursively crashing when handling an
     ICE when inside a nested diagnostics; see PR diagnostics/121876).  */
  if (buffer_ == m_diagnostic_buffer)
    return;

  /* We don't allow changing buffering within a diagnostic group
     (to simplify handling of buffered diagnostics within the
     diagnostic_format implementations).  */
  gcc_assert (m_diagnostic_groups.m_group_nesting_depth == 0);

  /* Likewise, for simplicity, we only allow changing buffers
     at nesting level 0.  */
  gcc_assert (m_diagnostic_groups.m_diagnostic_nesting_level == 0);

  m_diagnostic_buffer = buffer_;

  if (buffer_)
    {
      buffer_->ensure_per_sink_buffers ();
      gcc_assert (buffer_->m_per_sink_buffers);
      gcc_assert (buffer_->m_per_sink_buffers->length ()
		  == m_sinks.length ());
      for (unsigned idx = 0; idx < m_sinks.length (); ++idx)
	{
	  auto sink_ = m_sinks[idx];
	  auto per_sink_buffer = (*buffer_->m_per_sink_buffers)[idx];
	  sink_->set_buffer (per_sink_buffer);
	}
    }
  else
    for (auto sink_ : m_sinks)
      sink_->set_buffer (nullptr);
}

/* Clear BUFFER_ without flushing it.  */

void
context::clear_diagnostic_buffer (buffer &buffer_)
{
  if (buffer_.m_per_sink_buffers)
    for (auto per_sink_buffer_ : *buffer_.m_per_sink_buffers)
      per_sink_buffer_->clear ();

  buffer_.m_diagnostic_counters.clear ();

  /* We need to reset last_location, otherwise we may skip caret lines
     when we actually give a diagnostic.  */
  m_last_location = UNKNOWN_LOCATION;
}

/* Flush the diagnostics in BUFFER_ to this context, clearing BUFFER_.  */

void
context::flush_diagnostic_buffer (buffer &buffer_)
{
  bool had_errors
    = (buffer_.diagnostic_count (kind::error) > 0
       || buffer_.diagnostic_count (kind::werror) > 0);
  if (buffer_.m_per_sink_buffers)
    for (auto per_sink_buffer_ : *buffer_.m_per_sink_buffers)
      per_sink_buffer_->flush ();
  buffer_.m_diagnostic_counters.move_to (m_diagnostic_counters);

  action_after_output (had_errors ? kind::error : kind::warning);
  check_max_errors (true);
}

/* class diagnostics::buffer.  */

buffer::buffer (context &ctxt)
: m_ctxt (ctxt),
  m_per_sink_buffers (nullptr)
{
}

buffer::~buffer ()
{
  if (m_per_sink_buffers)
    {
      for (auto iter : *m_per_sink_buffers)
	delete iter;
      delete m_per_sink_buffers;
    }
}

void
buffer::dump (FILE *out, int indent) const
{
  m_diagnostic_counters.dump (out, indent + 2);
  dumping::emit_heading (out, indent, "m_per_sink_buffers");
  if (m_per_sink_buffers)
    for (auto per_sink_buffer_ : *m_per_sink_buffers)
      per_sink_buffer_->dump (out, indent + 2);
  else
    dumping::emit_none (out, indent + 2);
}

bool
buffer::empty_p () const
{
  if (m_per_sink_buffers)
    for (auto per_sink_buffer_ : *m_per_sink_buffers)
      /* Query initial buffer.  */
      return per_sink_buffer_->empty_p ();
  return true;
}

void
buffer::move_to (buffer &dest)
{
  /* Bail if there's nothing to move.  */
  if (!m_per_sink_buffers)
    return;

  m_diagnostic_counters.move_to (dest.m_diagnostic_counters);

  if (!dest.m_per_sink_buffers)
    {
      /* Optimization for the "move to empty" case:
	 simply move the vec to the dest.  */
      dest.m_per_sink_buffers = m_per_sink_buffers;
      m_per_sink_buffers = nullptr;
      return;
    }

  dest.ensure_per_sink_buffers ();
  gcc_assert (m_per_sink_buffers);
  gcc_assert (m_per_sink_buffers->length ()
	      == m_ctxt.m_sinks.length ());
  gcc_assert (dest.m_per_sink_buffers);
  gcc_assert (dest.m_per_sink_buffers->length ()
	      == m_ctxt.m_sinks.length ());
  for (unsigned idx = 0; idx < m_ctxt.m_sinks.length (); ++idx)
    {
      auto per_sink_buffer_src = (*m_per_sink_buffers)[idx];
      auto per_sink_buffer_dest = (*dest.m_per_sink_buffers)[idx];
      per_sink_buffer_src->move_to (*per_sink_buffer_dest);
    }
}

/* Lazily get the output formats to create their own kind of buffers.
   We can't change the output sinks on a context once this has been called
   on any diagnostics::buffer instances for that context, since there's no
   way to update all diagnostics::buffer instances for that context.  */

void
buffer::ensure_per_sink_buffers ()
{
  if (!m_per_sink_buffers)
    {
      m_per_sink_buffers = new auto_vec<per_sink_buffer *> ();
      for (unsigned idx = 0; idx < m_ctxt.m_sinks.length (); ++idx)
	{
	  auto sink_ = m_ctxt.m_sinks[idx];
	  auto per_sink_buffer = sink_->make_per_sink_buffer ();
	  m_per_sink_buffers->safe_push (per_sink_buffer.release ());
	}
    }
  gcc_assert (m_per_sink_buffers);
  gcc_assert (m_per_sink_buffers->length ()
	      == m_ctxt.m_sinks.length ());
}

} // namespace diagnostics
