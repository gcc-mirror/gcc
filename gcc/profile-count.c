/* Profile counter container type.
   Copyright (C) 2017 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "profile-count.h"
#include "options.h"
#include "tree.h"
#include "basic-block.h"
#include "cfg.h"
#include "function.h"
#include "gimple.h"
#include "data-streamer.h"
#include "cgraph.h"

void
profile_count::dump (FILE *f) const
{
  if (!initialized_p ())
    fprintf (f, "uninitialized");
  else
    fprintf (f, "%" PRId64, m_val);
}

void
profile_count::debug () const
{
  dump (stderr);
}

bool
profile_count::differs_from_p (profile_count other) const
{
  if (!initialized_p () || !other.initialized_p ())
    return false;
  if (m_val - other.m_val < 100 && other.m_val - m_val < 100)
    return false;
  if (!other.m_val)
    return true;
  int64_t ratio = m_val * 100 / other.m_val;
  return ratio < 99 || ratio > 101;
}

profile_count
profile_count::stream_in (struct lto_input_block *ib)
{
  profile_count ret;
  ret.m_val = streamer_read_gcov_count (ib);
  return ret;
}

void
profile_count::stream_out (struct output_block *ob)
{
  streamer_write_gcov_count (ob, m_val);
}

void
profile_count::stream_out (struct lto_output_stream *ob)
{
  streamer_write_gcov_count_stream (ob, m_val);
}
