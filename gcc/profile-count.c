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
#include "wide-int.h"

/* Dump THIS to F.  */

void
profile_count::dump (FILE *f) const
{
  if (!initialized_p ())
    fprintf (f, "uninitialized");
  else
    {
      fprintf (f, "%" PRId64, m_val);
      if (m_quality == profile_adjusted)
	fprintf (f, " (adjusted)");
      else if (m_quality == profile_afdo)
	fprintf (f, " (auto FDO)");
      else if (m_quality == profile_guessed)
	fprintf (f, " (guessed)");
    }
}

/* Dump THIS to stderr.  */

void
profile_count::debug () const
{
  dump (stderr);
  fprintf (stderr, "\n");
}

/* Return true if THIS differs from OTHER; tolerate small diferences.  */

bool
profile_count::differs_from_p (profile_count other) const
{
  if (!initialized_p () || !other.initialized_p ())
    return false;
  if ((uint64_t)m_val - (uint64_t)other.m_val < 100
      || (uint64_t)other.m_val - (uint64_t)m_val < 100)
    return false;
  if (!other.m_val)
    return true;
  int64_t ratio = (int64_t)m_val * 100 / other.m_val;
  return ratio < 99 || ratio > 101;
}

/* Stream THIS from IB.  */

profile_count
profile_count::stream_in (struct lto_input_block *ib)
{
  profile_count ret;
  ret.m_val = streamer_read_gcov_count (ib);
  ret.m_quality = (profile_quality) streamer_read_uhwi (ib);
  return ret;
}

/* Stream THIS to OB.  */

void
profile_count::stream_out (struct output_block *ob)
{
  streamer_write_gcov_count (ob, m_val);
  streamer_write_uhwi (ob, m_quality);
}

/* Stream THIS to OB.  */

void
profile_count::stream_out (struct lto_output_stream *ob)
{
  streamer_write_gcov_count_stream (ob, m_val);
  streamer_write_uhwi_stream (ob, m_quality);
}

/* Dump THIS to F.  */

void
profile_probability::dump (FILE *f) const
{
  if (!initialized_p ())
    fprintf (f, "uninitialized");
  else
    {
      /* Make difference between 0.00 as a roundoff error and actual 0.
	 Similarly for 1.  */
      if (m_val == 0)
        fprintf (f, "never");
      else if (m_val == max_probability)
        fprintf (f, "always");
      else
        fprintf (f, "%3.1f%%", (double)m_val * 100 / max_probability);
      if (m_quality == profile_adjusted)
	fprintf (f, " (adjusted)");
      else if (m_quality == profile_afdo)
	fprintf (f, " (auto FDO)");
      else if (m_quality == profile_guessed)
	fprintf (f, " (guessed)");
    }
}

/* Dump THIS to stderr.  */

void
profile_probability::debug () const
{
  dump (stderr);
  fprintf (stderr, "\n");
}

/* Return true if THIS differs from OTHER; tolerate small diferences.  */

bool
profile_probability::differs_from_p (profile_probability other) const
{
  if (!initialized_p () || !other.initialized_p ())
    return false;
  if ((uint64_t)m_val - (uint64_t)other.m_val < max_probability / 1000
      || (uint64_t)other.m_val - (uint64_t)max_probability < 1000)
    return false;
  if (!other.m_val)
    return true;
  int64_t ratio = (int64_t)m_val * 100 / other.m_val;
  return ratio < 99 || ratio > 101;
}

/* Return true if THIS differs significantly from OTHER.  */

bool
profile_probability::differs_lot_from_p (profile_probability other) const
{
  if (!initialized_p () || !other.initialized_p ())
    return false;
  uint32_t d = m_val > other.m_val ? m_val - other.m_val : other.m_val - m_val;
  return d > max_probability / 2;
}

/* Stream THIS from IB.  */

profile_probability
profile_probability::stream_in (struct lto_input_block *ib)
{
  profile_probability ret;
  ret.m_val = streamer_read_uhwi (ib);
  ret.m_quality = (profile_quality) streamer_read_uhwi (ib);
  return ret;
}

/* Stream THIS to OB.  */

void
profile_probability::stream_out (struct output_block *ob)
{
  streamer_write_uhwi (ob, m_val);
  streamer_write_uhwi (ob, m_quality);
}

/* Stream THIS to OB.  */

void
profile_probability::stream_out (struct lto_output_stream *ob)
{
  streamer_write_uhwi_stream (ob, m_val);
  streamer_write_uhwi_stream (ob, m_quality);
}

/* Compute RES=(a*b + c/2)/c capping and return false if overflow happened.  */

bool
slow_safe_scale_64bit (uint64_t a, uint64_t b, uint64_t c, uint64_t *res)
{
  FIXED_WIDE_INT (128) tmp = a;
  bool overflow;
  tmp = wi::udiv_floor (wi::umul (tmp, b, &overflow) + (c / 2), c);
  gcc_checking_assert (!overflow);
  if (wi::fits_uhwi_p (tmp))
    {
      *res = tmp.to_uhwi ();
      return true;
    }
  *res = (uint64_t) -1;
  return false;
}
