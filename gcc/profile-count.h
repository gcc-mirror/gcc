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

#ifndef GCC_PROFILE_COUNT_H
#define GCC_PROFILE_COUNT_H

/* Quality of the proflie count.  Because gengtype does not support enums
   inside of clases, this is in global namespace.  */
enum profile_count_quality {
  /* Profile is based on static branch prediction heuristics.  It may or may
     not reflect the reality.  */
  count_guessed = 0,
  /* Profile was determined by autofdo.  */
  count_afdo = 1,
  /* Profile was originally based on feedback but it was adjusted 
     by code duplicating optimization.  It may not precisely reflect the
     particular code path.  */
  count_adjusted = 2,
  /* Profile was read from profile feedback or determined by accurate static
     method.  */
  count_read = 3
};

/* The base value for branch probability notes and edge probabilities.  */
#define REG_BR_PROB_BASE  10000

#define RDIV(X,Y) (((X) + (Y) / 2) / (Y))

/* Main data type to hold profile counters in GCC.  In most cases profile
   counts originate from profile feedback. They are 64bit integers
   representing number of executions during the train run.
   As the profile is maintained during the compilation, many adjustments are
   made.  Not all transformations can be made precisely, most importantly
   when code is being duplicated.  It also may happen that part of CFG has
   profile counts known while other do not - for example when LTO optimizing
   partly profiled program or when profile was lost due to COMDAT merging.

   For this reason profile_count tracks more information than
   just unsigned integer and it is also ready for profile mismatches.
   The API of this data type represent operations that are natural
   on profile counts - sum, difference and operation with scales and
   probabilities.  All operations are safe by never getting negative counts
   and they do end up in uninitialized scale if any of the parameters is
   uninitialized.

   All comparsions that are three state and handling of probabilities.  Thus
   a < b is not equal to !(a >= b).

   The following pre-defined counts are available:

   profile_count::zero ()  for code that is known to execute zero times at
      runtime (this can be detected statically i.e. for paths leading to
      abort ();
   profile_count::one () for code that is known to execute once (such as
      main () function
   profile_count::uninitialized ()  for unknown execution count.

 */

class GTY(()) profile_count
{
  /* Use 62bit to hold basic block counters.  Should be at least
     64bit.  Although a counter cannot be negative, we use a signed
     type to hold various extra stages.  */

  static const int n_bits = 62;
  static const uint64_t max_count = ((uint64_t) 1 << n_bits) - 2;
  static const uint64_t uninitialized_count = ((uint64_t) 1 << n_bits) - 1;

  uint64_t m_val : n_bits;
  enum profile_count_quality m_quality : 2;

  /* Assume numbers smaller than this to multiply.  This is set to make
     testsuite pass, in future we may implement precise multiplication in higer
     rangers.  */
  static const int64_t max_safe_multiplier = 131072;
public:

  /* Used for counters which are expected to be never executed.  */
  static profile_count zero ()
    {
      return from_gcov_type (0);
    }
  static profile_count one ()
    {
      return from_gcov_type (1);
    }
  /* Value of counters which has not been initialized. Either because
     initialization did not happen yet or because profile is unknown.  */
  static profile_count uninitialized ()
    {
      profile_count c;
      c.m_val = uninitialized_count;
      c.m_quality = count_guessed;
      return c;
    }

  /* The profiling runtime uses gcov_type, which is usually 64bit integer.
     Conversions back and forth are used to read the coverage and get it
     into internal representation.  */
  static profile_count from_gcov_type (gcov_type v)
    {
      profile_count ret;
      gcc_checking_assert (v >= 0 && (uint64_t) v <= max_count);
      ret.m_val = v;
      ret.m_quality = count_read;
      return ret;
    }

  /* Conversion to gcov_type is lossy.  */
  gcov_type to_gcov_type () const
    {
      gcc_checking_assert (initialized_p ());
      return m_val;
    }

  /* Return true if value has been initialized.  */
  bool initialized_p () const
    {
      return m_val != uninitialized_count;
    }
  /* Return true if value can be trusted.  */
  bool reliable_p () const
    {
      return initialized_p ();
    }

  /* Basic operations.  */
  bool operator== (const profile_count &other) const
    {
      return m_val == other.m_val && m_quality == other.m_quality;
    }
  profile_count operator+ (const profile_count &other) const
    {
      if (other == profile_count::zero ())
	return *this;
      if (*this == profile_count::zero ())
	return other;
      if (!initialized_p () || !other.initialized_p ())
	return profile_count::uninitialized ();

      profile_count ret;
      ret.m_val = m_val + other.m_val;
      ret.m_quality = MIN (m_quality, other.m_quality);
      return ret;
    }
  profile_count &operator+= (const profile_count &other)
    {
      if (other == profile_count::zero ())
	return *this;
      if (*this == profile_count::zero ())
	{
	  *this = other;
	  return *this;
	}
      if (!initialized_p () || !other.initialized_p ())
	return *this = profile_count::uninitialized ();
      else
	{
	  m_val += other.m_val;
          m_quality = MIN (m_quality, other.m_quality);
	}
      return *this;
    }
  profile_count operator- (const profile_count &other) const
    {
      if (*this == profile_count::zero () || other == profile_count::zero ())
	return *this;
      if (!initialized_p () || !other.initialized_p ())
	return profile_count::uninitialized ();
      profile_count ret;
      ret.m_val = m_val >= other.m_val ? m_val - other.m_val : 0;
      ret.m_quality = MIN (m_quality, other.m_quality);
      return ret;
    }
  profile_count &operator-= (const profile_count &other)
    {
      if (*this == profile_count::zero () || other == profile_count::zero ())
	return *this;
      if (!initialized_p () || !other.initialized_p ())
	return *this = profile_count::uninitialized ();
      else
	{
	  m_val = m_val >= other.m_val ? m_val - other.m_val: 0;
          m_quality = MIN (m_quality, other.m_quality);
	}
      return *this;
    }

  /* Return false if profile_count is bogus.  */
  bool verify () const
    {
      return m_val != uninitialized_count || m_quality == count_guessed;
    }

  /* Comparsions are three-state and conservative.  False is returned if
     the inequality can not be decided.  */
  bool operator< (const profile_count &other) const
    {
      return initialized_p () && other.initialized_p () && m_val < other.m_val;
    }
  bool operator> (const profile_count &other) const
    {
      return initialized_p () && other.initialized_p () && m_val > other.m_val;
    }
  bool operator< (const gcov_type other) const
    {
      gcc_checking_assert (other >= 0);
      return initialized_p () && m_val < (uint64_t) other;
    }
  bool operator> (const gcov_type other) const
    {
      gcc_checking_assert (other >= 0);
      return initialized_p () && m_val > (uint64_t) other;
    }

  bool operator<= (const profile_count &other) const
    {
      return initialized_p () && other.initialized_p () && m_val <= other.m_val;
    }
  bool operator>= (const profile_count &other) const
    {
      return initialized_p () && m_val >= other.m_val;
    }
  bool operator<= (const gcov_type other) const
    {
      gcc_checking_assert (other >= 0);
      return initialized_p () && m_val <= (uint64_t) other;
    }
  bool operator>= (const gcov_type other) const
    {
      gcc_checking_assert (other >= 0);
      return initialized_p () && m_val >= (uint64_t) other;
    }

  /* PROB is a probability in scale 0...REG_BR_PROB_BASE.  Scale counter
     accordingly.  */
  profile_count apply_probability (int prob) const
    {
      gcc_checking_assert (prob >= 0 && prob <= REG_BR_PROB_BASE);
      if (m_val == 0)
	return *this;
      if (!initialized_p ())
	return profile_count::uninitialized ();
      profile_count ret;
      ret.m_val = RDIV (m_val * prob, REG_BR_PROB_BASE);
      ret.m_quality = MIN (m_quality, count_adjusted);
      return ret;
    }
  /* Return *THIS * NUM / DEN.  */
  profile_count apply_scale (int64_t num, int64_t den) const
    {
      if (m_val == 0)
	return *this;
      if (!initialized_p ())
	return profile_count::uninitialized ();
      profile_count ret;
      gcc_checking_assert (num >= 0 && den > 0);
      /* FIXME: shrink wrapping violates this sanity check.  */
      gcc_checking_assert ((num <= REG_BR_PROB_BASE
			    || den <= REG_BR_PROB_BASE) || 1);
      ret.m_val = RDIV (m_val * num, den);
      ret.m_quality = MIN (m_quality, count_adjusted);
      return ret;
    }
  profile_count apply_scale (profile_count num, profile_count den) const
    {
      if (m_val == 0)
	return *this;
      if (num.m_val == 0)
	return num;
      if (!initialized_p () || !num.initialized_p () || !den.initialized_p ())
	return profile_count::uninitialized ();
      gcc_checking_assert (den > 0);
      if (num == den)
	return *this;

      profile_count ret;
      /* Take care for overflows!  */
      if (num.m_val < max_safe_multiplier || m_val < max_safe_multiplier)
        ret.m_val = RDIV (m_val * num.m_val, den.m_val);
      else
        ret.m_val = RDIV (m_val * RDIV (num.m_val * max_safe_multiplier,
					den.m_val), max_safe_multiplier);
      ret.m_quality = MIN (m_quality, count_adjusted);
      return ret;
    }

  /* Return probability of event with counter THIS within event with counter
     OVERALL.  */
  int probability_in (profile_count overall)
    {
      if (!m_val)
	return 0;
      if (!initialized_p () || !overall.initialized_p ())
	return REG_BR_PROB_BASE / 2;
      if (overall < *this)
	return REG_BR_PROB_BASE;
      if (!overall.m_val)
	return REG_BR_PROB_BASE / 2;
      return RDIV (m_val * REG_BR_PROB_BASE, overall.m_val);
    }

  /* Output THIS to F.  */
  void dump (FILE *f) const;

  /* Print THIS to stderr.  */
  void debug () const;

  /* Return true if THIS is known to differ significantly from OTHER.  */
  bool differs_from_p (profile_count other) const;

  /* LTO streaming support.  */
  static profile_count stream_in (struct lto_input_block *);
  void stream_out (struct output_block *);
  void stream_out (struct lto_output_stream *);
};
#endif
