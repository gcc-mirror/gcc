/* Profile counter container type.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

/* Names from profile_quality enum values.  */

const char *profile_quality_names[] =
{
  "uninitialized",
  "guessed_local",
  "guessed_global0",
  "guessed_global0adjusted",
  "guessed",
  "afdo",
  "adjusted",
  "precise"
};

/* Get a string describing QUALITY.  */

const char *
profile_quality_as_string (enum profile_quality quality)
{
  return profile_quality_names[quality];
}

/* Parse VALUE as profile quality and return true when a valid QUALITY.  */

bool
parse_profile_quality (const char *value, profile_quality *quality)
{
  for (unsigned i = 0; i < ARRAY_SIZE (profile_quality_names); i++)
    if (strcmp (profile_quality_names[i], value) == 0)
      {
	*quality = (profile_quality)i;
	return true;
      }

  return false;
}

/* Display names from profile_quality enum values.  */

const char *profile_quality_display_names[] =
{
  NULL,
  "estimated locally",
  "estimated locally, globally 0",
  "estimated locally, globally 0 adjusted",
  "guessed",
  "auto FDO",
  "adjusted",
  "precise"
};

/* Dump THIS to BUFFER.  */

void
profile_count::dump (char *buffer) const
{
  if (!initialized_p ())
    sprintf (buffer, "uninitialized");
  else
    sprintf (buffer, "%" PRId64 " (%s)", m_val,
	     profile_quality_display_names[m_quality]);
}

/* Dump THIS to F.  */

void
profile_count::dump (FILE *f) const
{
  char buffer[64];
  dump (buffer);
  fputs (buffer, f);
}

/* Dump THIS to stderr.  */

void
profile_count::debug () const
{
  dump (stderr);
  fprintf (stderr, "\n");
}

/* Return true if THIS differs from OTHER; tolerate small differences.  */

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
profile_count::stream_in (class lto_input_block *ib)
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


/* Output THIS to BUFFER.  */

void
profile_probability::dump (char *buffer) const
{
  if (!initialized_p ())
    sprintf (buffer, "uninitialized");
  else
    {
      /* Make difference between 0.00 as a roundoff error and actual 0.
	 Similarly for 1.  */
      if (m_val == 0)
	buffer += sprintf (buffer, "never");
      else if (m_val == max_probability)
	buffer += sprintf (buffer, "always");
      else
	buffer += sprintf (buffer, "%3.1f%%", (double)m_val * 100 / max_probability);

      if (m_quality == ADJUSTED)
	sprintf (buffer, " (adjusted)");
      else if (m_quality == AFDO)
	sprintf (buffer, " (auto FDO)");
      else if (m_quality == GUESSED)
	sprintf (buffer, " (guessed)");
    }
}

/* Dump THIS to F.  */

void
profile_probability::dump (FILE *f) const
{
  char buffer[64];
  dump (buffer);
  fputs (buffer, f);
}

/* Dump THIS to stderr.  */

void
profile_probability::debug () const
{
  dump (stderr);
  fprintf (stderr, "\n");
}

/* Return true if THIS differs from OTHER; tolerate small differences.  */

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
profile_probability::stream_in (class lto_input_block *ib)
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
  if (*this == zero ())
    return 0;
  STATIC_ASSERT (REG_BR_PROB_BASE == BB_FREQ_MAX);
  gcc_assert (fun->cfg->count_max.initialized_p ());
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
  if (*this == zero ())
    return 0;
  gcc_checking_assert (entry_bb_count.initialized_p ());
  uint64_t scale;
  gcc_checking_assert (compatible_p (entry_bb_count));
  if (!safe_scale_64bit (!entry_bb_count.m_val ? m_val + 1 : m_val,
			 CGRAPH_FREQ_BASE, MAX (1, entry_bb_count.m_val), &scale))
    return CGRAPH_FREQ_MAX;
  return MIN (scale, CGRAPH_FREQ_MAX);
}

/* Return THIS/IN as sreal value.  */

sreal
profile_count::to_sreal_scale (profile_count in, bool *known) const
{
  if (*this == zero ()
      && !(in == zero ()))
  {
    if (known)
      *known = true;
    return 0;
  }
  if (!initialized_p () || !in.initialized_p ())
    {
      if (known)
	*known = false;
      return 1;
    }
  if (known)
    *known = true;
  if (*this == in)
    return 1;
  gcc_checking_assert (compatible_p (in));
  if (m_val == in.m_val)
    return 1;
  if (!in.m_val)
    return m_val * 4;
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
  if (*num == zero ())
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
  if (!initialized_p ())
    return *this;
  ipa = ipa.ipa ();
  if (ipa.nonzero_p ())
    return ipa;
  if (!ipa.initialized_p () || *this == zero ())
    return *this;
  if (ipa == zero ())
    return this->global0 ();
  return this->global0adjusted ();
}

/* Sae as profile_count::combine_with_ipa_count but within function with count
   IPA2.  */
profile_count
profile_count::combine_with_ipa_count_within (profile_count ipa,
					      profile_count ipa2)
{
  profile_count ret;
  if (!initialized_p ())
    return *this;
  if (ipa2.ipa () == ipa2 && ipa.initialized_p ())
    ret = ipa;
  else
    ret = combine_with_ipa_count (ipa);
  gcc_checking_assert (ret.compatible_p (ipa2));
  return ret;
}

/* The profiling runtime uses gcov_type, which is usually 64bit integer.
   Conversions back and forth are used to read the coverage and get it
   into internal representation.  */

profile_count
profile_count::from_gcov_type (gcov_type v, profile_quality quality)
  {
    profile_count ret;
    gcc_checking_assert (v >= 0);
    if (dump_file && v >= (gcov_type)max_count)
      fprintf (dump_file,
	       "Capping gcov count %" PRId64 " to max_count %" PRId64 "\n",
	       (int64_t) v, (int64_t) max_count);
    ret.m_val = MIN (v, (gcov_type)max_count);
    ret.m_quality = quality;
    return ret;
  }

/* COUNT1 times event happens with *THIS probability, COUNT2 times OTHER
   happens with COUNT2 probability.  Return probability that either *THIS or
   OTHER happens.  */

profile_probability
profile_probability::combine_with_count (profile_count count1,
					 profile_probability other,
					 profile_count count2) const
{
  /* If probabilities are same, we are done.
     If counts are nonzero we can distribute accordingly. In remaining
     cases just average the values and hope for the best.  */
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
    return *this * even () + other * even ();
}

/* Return probability as sreal in range [0, 1].  */

sreal
profile_probability::to_sreal () const
{
  gcc_checking_assert (initialized_p ());
  return ((sreal)m_val) >> (n_bits - 2);
}
