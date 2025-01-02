/* Subsets of a finite interval over Z.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* A class that represents the union of finitely many intervals.
   The domain over which the intervals are defined is a finite integer
   interval [m_min, m_max], usually the range of some [u]intN_t.
   Supported operations are:
   - Complement w.r.t. the domain (invert)
   - Union (union_)
   - Intersection (intersect)
   - Difference / Setminus (minus).
   Ranges is closed under all operations:  The result of all operations
   is a Ranges over the same domain.  (As opposed to value-range.h which
   may ICE for some operations, see below).

   The representation is unique in the sense that when we have two
   Ranges A and B, then
   1)  A == B  <==>  A.size == B.size  &&  Ai == Bi for all i.

   The representation is normalized:
   2)  Ai != {}          ;; There are no empty intervals.
   3)  Ai.hi < A{i+1}.lo ;; The Ai's are in increasing order and separated
			 ;; by at least one value (non-adjacent).
   The sub-intervals Ai are maintained as a std::vector.
   The computation of union and intersection scales like  A.size * B.size
   i.e. Ranges is only eligible for GCC when size() has a fixed upper
   bound independent of the program being compiled (or there are other
   means to guarantee that the complexity is linearistic).
   In the context of AVR, we have size() <= 3.

   The reason why we don't use value-range.h's irange or int_range is that
   these use the integers Z as their domain, which makes computations like
   invert() quite nasty as they may ICE for common cases.  Doing all
   these special cases (like one sub-interval touches the domain bounds)
   makes using value-range.h more laborious (and instable) than using our
   own mini Ranger.  */

struct Ranges
{
  // This is good enough as it covers (un)signed SImode.
  using T = HOST_WIDE_INT;
  typedef T scalar_type;

  // Non-empty ranges.  Empty sets are only used transiently;
  // Ranges.ranges[] doesn't use them.
  struct SubRange
  {
    // Lower and upper bound, inclusively.
    T lo, hi;

    SubRange intersect (const SubRange &r) const
    {
      if (lo >= r.lo && hi <= r.hi)
	return *this;
      else if (r.lo >= lo && r.hi <= hi)
	return r;
      else if (lo > r.hi || hi < r.lo)
	return SubRange { 1, 0 };
      else
	return SubRange { std::max (lo, r.lo), std::min (hi, r.hi) };
    }

    T cardinality () const
    {
      return std::max<T> (0, hi - lo + 1);
    }
  };

  // Finitely many intervals over [m_min, m_max] that are normalized:
  // No empty sets, increasing order, separated by at least one value.
  T m_min, m_max;
  std::vector<SubRange> ranges;

  // Not used anywhere in Ranges; can be used elsewhere.
  // May be clobbered by set operations.
  int tag = -1;

  enum initial_range { EMPTY, ALL };

  Ranges (T mi, T ma, initial_range ir)
    : m_min (mi), m_max (ma)
  {
    if (ir == ALL)
      push (mi, ma);
  }

  // Domain is the range of some [u]intN_t.
  static Ranges NBitsRanges (int n_bits, bool unsigned_p, initial_range ir)
  {
    T mask = ((T) 1 << n_bits) - 1;
    gcc_assert (mask > 0);
    T ma = mask >> ! unsigned_p;
    return Ranges (unsigned_p ? 0 : -ma - 1, ma, ir);
  }

  static void sort2 (Ranges &a, Ranges &b)
  {
    if (a.size () && b.size ())
      if (a.ranges[0].lo > b.ranges[0].lo)
	std::swap (a, b);
  }

  void print (FILE *file) const
  {
    if (file)
      {
	fprintf (file, " .tag%d=#%d={", tag, size ());
	for (const auto &r : ranges)
	  fprintf (file, "[ %ld, %ld ]", (long) r.lo, (long) r.hi);
	fprintf (file, "}\n");
      }
  }

  // The number of sub-intervals in .ranges.
  int size () const
  {
    return (int) ranges.size ();
  }

  // Append [LO, HI] & [m_min, m_max] to .ranges provided the
  // former is non-empty.
  void push (T lo, T hi)
  {
    lo = std::max (lo, m_min);
    hi = std::min (hi, m_max);

    if (lo <= hi)
      ranges.push_back (SubRange { lo, hi });
  }

  // Append R to .ranges provided the former is non-empty.
  void push (const SubRange &r)
  {
    push (r.lo, r.hi);
  }

  // Cardinality of the n-th interval.
  T cardinality (int n) const
  {
    return n < size () ? ranges[n].cardinality () : 0;
  }

  // Check that *this is normalized: .ranges are non-empty, non-overlapping,
  // non-adjacent and increasing.
  bool check () const
  {
    bool bad = size () && (ranges[0].lo < m_min
			   || ranges[size () - 1].hi > m_max);

    for (int n = 0; n < size (); ++n)
      {
	bad |= ranges[n].lo > ranges[n].hi;
	bad |= n > 0 && ranges[n - 1].hi >= ranges[n].lo;
      }

    if (bad)
      print (dump_file);

    return ! bad;
  }

  // Intersect A and B according to  (U Ai) & (U Bj) = U (Ai & Bj)
  // This has quadratic complexity, but also the nice property that
  // when A and B are normalized, then the result is too.
  void intersect (const Ranges &r)
  {
    gcc_assert (m_min == r.m_min && m_max == r.m_max);

    if (this == &r)
      return;

    std::vector<SubRange> rs;
    std::swap (rs, ranges);

    for (const auto &a : rs)
      for (const auto &b : r.ranges)
	push (a.intersect (b));
  }

  // Complement w.r.t. the domain [m_min, m_max].
  void invert ()
  {
    std::vector<SubRange> rs;
    std::swap (rs, ranges);

    if (rs.size () == 0)
	push (m_min, m_max);
    else
      {
	push (m_min, rs[0].lo - 1);

	for (size_t n = 1; n < rs.size (); ++n)
	  push (rs[n - 1].hi + 1, rs[n].lo - 1);

	push (rs[rs.size () - 1].hi + 1, m_max);
      }
  }

  // Set-minus.
  void minus (const Ranges &r)
  {
    gcc_assert (m_min == r.m_min && m_max == r.m_max);

    Ranges sub = r;
    sub.invert ();
    intersect (sub);
  }

  // Union of sets.  Not needed in avr.cc but added for completeness.
  // DeMorgan this for simplicity.
  void union_ (const Ranges &r)
  {
    gcc_assert (m_min == r.m_min && m_max == r.m_max);

    if (this != &r)
      {
	invert ();
	minus (r);
	invert ();
      }
  }

  // Get the truth Ranges for  x <cmp> val.  For example,
  // LT 3  will return  [m_min, 2].
  Ranges truth (rtx_code cmp, T val, bool strict = true)
  {
    if (strict)
      {
	if (avr_strict_signed_p (cmp))
	  gcc_assert (m_min == -m_max - 1);
	else if (avr_strict_unsigned_p (cmp))
	  gcc_assert (m_min == 0);

	gcc_assert (IN_RANGE (val, m_min, m_max));
      }

    bool rev = cmp == NE || cmp == LTU || cmp == LT || cmp == GTU || cmp == GT;
    if (rev)
      cmp = reverse_condition (cmp);

    T lo = m_min;
    T hi = m_max;

    if (cmp == EQ)
      lo = hi = val;
    else if (cmp == LEU || cmp == LE)
      hi = val;
    else if (cmp == GEU || cmp == GE)
      lo = val;
    else
      gcc_unreachable ();

    Ranges rs (m_min, m_max, Ranges::EMPTY);
    rs.push (lo, hi);

    if (rev)
      rs.invert ();

    return rs;
  }

}; // struct Ranges
