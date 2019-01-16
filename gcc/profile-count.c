/* Profile counter container type.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.
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
#include "function.h"
#include "cfg.h"
#include "gimple.h"
#include "data-streamer.h"
#include "cgraph.h"
#include "wide-int.h"
#include "sreal.h"

/* Get a string describing QUALITY.  */

const char *
profile_quality_as_string (enum profile_quality quality)
{
  switch (quality)
    {
    default:
      gcc_unreachable ();
    case profile_uninitialized:
      return "uninitialized";
    case profile_guessed_local:
      return "guessed_local";
    case profile_guessed_global0:
      return "guessed_global0";
    case profile_guessed_global0adjusted:
      return "guessed_global0adjusted";
    case profile_guessed:
      return "guessed";
    case profile_afdo:
      return "afdo";
    case profile_adjusted:
      return "adjusted";
    case profile_precise:
      return "precise";
    }
}

/* Dump THIS to F.  */

void
profile_count::dump (FILE *f) const
{
  if (!initialized_p ())
    fprintf (f, "uninitialized");
  else
    {
      fprintf (f, "%" PRId64, m_val);
      if (m_quality == profile_guessed_local)
	fprintf (f, " (estimated locally)");
      else if (m_quality == profile_guessed_global0)
	fprintf (f, " (estimated locally, globally 0)");
      else if (m_quality == profile_guessed_global0adjusted)
	fprintf (f, " (estimated locally, globally 0 adjusted)");
      else if (m_quality == profile_adjusted)
	fprintf (f, " (adjusted)");
      else if (m_quality == profile_afdo)
	fprintf (f, " (auto FDO)");
      else if (m_quality == profile_guessed)
	fprintf (f, " (guessed)");
      else if (m_quality == profile_precise)
	fprintf (f, " (precise)");
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
  gcc_checking_assert (compatible_p (other));
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
  wi::overflow_type overflow;
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

/* Return count as frequency within FUN scaled in range 0 to REG_FREQ_MAX
   Used for legacy code and should not be used anymore.  */

int
profile_count::to_frequency (struct function *fun) const
{
  if (!initialized_p ())
    return BB_FREQ_MAX;
  if (*this == profile_count::zero ())
    return 0;
  gcc_assert (REG_BR_PROB_BASE == BB_FREQ_MAX
	      && fun->cfg->count_max.initialized_p ());
  profile_probability prob = probability_in (fun->cfg->count_max);
  if (!prob.initialized_p ())
    return REG_BR_PROB_BASE;
  return prob.to_reg_br_prob_base ();
}

/* Return count as frequency within FUN scaled in range 0 to CGRAPH_FREQ_MAX
   where CGRAPH_FREQ_BASE means that count equals to entry block count.
   Used for legacy code and should not be used anymore.  */

int
profile_count::to_cgraph_frequency (profile_count entry_bb_count) const
{
  if (!initialized_p () || !entry_bb_count.initialized_p ())
    return CGRAPH_FREQ_BASE;
  if (*this == profile_count::zero ())
    return 0;
  gcc_checking_assert (entry_bb_count.initialized_p ());
  uint64_t scale;
  if (!safe_scale_64bit (!entry_bb_count.m_val ? m_val + 1 : m_val,
			 CGRAPH_FREQ_BASE, MAX (1, entry_bb_count.m_val), &scale))
    return CGRAPH_FREQ_MAX;
  return MIN (scale, CGRAPH_FREQ_MAX);
}

/* Return THIS/IN as sreal value.  */

sreal
profile_count::to_sreal_scale (profile_count in, bool *known) const
{
  if (!initialized_p () || !in.initialized_p ())
    {
      if (known)
	*known = false;
      return 1;
    }
  if (known)
    *known = true;
  if (*this == profile_count::zero ())
    return 0;

  if (!in.m_val)
    {
      if (!m_val)
	return 1;
      return m_val * 4;
    }
  return (sreal)m_val / (sreal)in.m_val;
}

/* We want to scale profile across function boundary from NUM to DEN.
   Take care of the side case when DEN is zeros.  We still want to behave
   sanely here which means
     - scale to profile_count::zero () if NUM is profile_count::zero
     - do not affect anything if NUM == DEN
     - preserve counter value but adjust quality in other cases.  */

void
profile_count::adjust_for_ipa_scaling (profile_count *num,
				       profile_count *den)
{
  /* Scaling is no-op if NUM and DEN are the same.  */
  if (*num == *den)
    return;
  /* Scaling to zero is always zero.  */
  if (*num == profile_count::zero ())
    return;
  /* If den is non-zero we are safe.  */
  if (den->force_nonzero () == *den)
    return;
  /* Force both to non-zero so we do not push profiles to 0 when
     both num == 0 and den == 0.  */
  *den = den->force_nonzero ();
  *num = num->force_nonzero ();
}

/* THIS is a count of bb which is known to be executed IPA times.
   Combine this information into bb counter.  This means returning IPA
   if it is nonzero, not changing anything if IPA is uninitialized
   and if IPA is zero, turning THIS into corresponding local profile with
   global0.  */
profile_count
profile_count::combine_with_ipa_count (profile_count ipa)
{
  ipa = ipa.ipa ();
  if (ipa.nonzero_p ())
    return ipa;
  if (!ipa.initialized_p () || *this == profile_count::zero ())
    return *this;
  if (ipa == profile_count::zero ())
    return this->global0 ();
  return this->global0adjusted ();
}

/* The profiling runtime uses gcov_type, which is usually 64bit integer.
   Conversions back and forth are used to read the coverage and get it
   into internal representation.  */
profile_count
profile_count::from_gcov_type (gcov_type v)
  {
    profile_count ret;
    gcc_checking_assert (v >= 0);
    if (dump_file && v >= (gcov_type)max_count)
      fprintf (dump_file,
	       "Capping gcov count %" PRId64 " to max_count %" PRId64 "\n",
	       (int64_t) v, (int64_t) max_count);
    ret.m_val = MIN (v, (gcov_type)max_count);
    ret.m_quality = profile_precise;
    return ret;
  }


/* COUNT1 times event happens with *THIS probability, COUNT2 times OTHER
   happens with COUNT2 probablity. Return probablity that either *THIS or
   OTHER happens.  */

profile_probability
profile_probability::combine_with_count (profile_count count1,
					 profile_probability other,
					 profile_count count2) const
{
  /* If probabilities are same, we are done.
     If counts are nonzero we can distribute accordingly. In remaining
     cases just avreage the values and hope for the best.  */
  if (*this == other || count1 == count2
      || (count2 == profile_count::zero ()
	  && !(count1 == profile_count::zero ())))
    return *this;
  if (count1 == profile_count::zero () && !(count2 == profile_count::zero ()))
    return other;
  else if (count1.nonzero_p () || count2.nonzero_p ())
    return *this * count1.probability_in (count1 + count2)
	   + other * count2.probability_in (count1 + count2);
  else
    return *this * profile_probability::even ()
	   + other * profile_probability::even ();
}
