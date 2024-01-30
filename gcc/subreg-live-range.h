/* SUBREG liveness tracking classes for DF & IRA & LRA.
   Copyright (C) 2024 Free Software Foundation, Inc.
   Contributed by Lehua Ding (lehua.ding@rivai.ai), RiVAI Technologies Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_SUBREG_LIVE_RANGE_H
#define GCC_SUBREG_LIVE_RANGE_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include <unordered_map>
#include "sbitmap.h"

/* class subregs_live record the live subreg_ranges of registers.  */
class subregs_live
{
public:
  /* The key is usually the register's regno.  */
  std::unordered_map<unsigned int, sbitmap> lives;
  
  subregs_live () : lives () {}

  subregs_live (size_t n) : lives (n) {}

  ~subregs_live ()
  {
    for (auto &kv : lives)
      sbitmap_free (kv.second);
  }

  void clear ()
  {
    for (auto &kv : lives)
      sbitmap_free (kv.second);
    lives.clear ();
  }

  void clear (size_t n)
  {
    clear ();
    lives.rehash (n);
  }

  bool find_p (unsigned int regno) const
  {
    return lives.find (regno) != lives.end ();
  }

  sbitmap get_range (unsigned int regno)
  {
    gcc_assert (find_p (regno));
    return lives.at (regno);
  }

  const_sbitmap get_range (unsigned int regno) const
  {
    gcc_assert (find_p (regno));
    return lives.at (regno);
  }

  /* Added RANGE to regno's ranges. Return true if leads to a change.  */
  bool add_range (unsigned int regno, const_sbitmap range)
  {
    if (find_p (regno))
      {
	sbitmap curr = get_range (regno);
	gcc_assert (range->n_bits == curr->n_bits);
	return bitmap_ior (curr, curr, range);
      }
    else
      {
	sbitmap a = sbitmap_alloc (range->n_bits);
	lives.insert ({regno, a});
	sbitmap curr = get_range (regno);
	bitmap_copy (curr, range);
	return !bitmap_empty_p (range);
      }
  }
  /* Removed RANGE from regno's ranges. Return true if leads to a change.  */
  bool remove_range (unsigned int regno, const_sbitmap range)
  {
    if (find_p (regno))
      {
	sbitmap curr = get_range (regno);
	bitmap_check_sizes (curr, range);
	if (bitmap_subset_p (curr, range))
	  return remove_range (regno);

	auto_sbitmap a (range->n_bits);
	bitmap_not (a, range);
	return bitmap_and (curr, curr, a);
      }
    return false;
  }
  /* Removed the whole range of REGNO. Return true if leads to a change.  */
  bool remove_range (unsigned int regno)
  {
    if (find_p (regno))
      {
	sbitmap curr = get_range (regno);
	bool changed = !bitmap_empty_p (curr);
	sbitmap_free (curr);
	lives.erase (regno);
	return changed;
      }
    return false;
  }
  /* Replace the range of REGNO with RANGE.  Return true if leads to a change.
   */
  bool replace_range (unsigned int regno, const_sbitmap range)
  {
    if (find_p (regno))
      {
	sbitmap curr = get_range (regno);
	if (!bitmap_same_p (curr, range))
	  {
	    bitmap_copy (curr, range);
	    return true;
	  }
	else
	  return false;
      }
    else
      return add_range (regno, range);
  }
  /* Copy subregs_live SL. Return true if leads to a change.  */
  bool copy_lives (const subregs_live &sl)
  {
    bool changed = false;
    for (auto &kv : sl.lives)
      {
	unsigned int regno = kv.first;
	const_sbitmap range = kv.second;
	if (bitmap_empty_p (range))
	  continue;
	if (find_p (regno))
	  changed |= replace_range (regno, range);
	else
	  changed |= add_range (regno, range);
      }

    for (auto it = lives.cbegin (); it != lives.cend ();)
      {
	unsigned int regno = it->first;
	auto prev_it = it;
	it++;
	if (sl.empty_p (regno))
	  {
	    changed |= bitmap_empty_p (it->second);
	    lives.erase (prev_it);
	  }
      }

    return changed;
  }
  /* Added subregs_live SL. Return true if leads to a change.  */
  bool add_lives (const subregs_live &sl)
  {
    bool changed = false;
    for (auto &kv : sl.lives)
      {
	unsigned int regno = kv.first;
	const_sbitmap range = kv.second;
	if (find_p (regno))
	  {
	    sbitmap curr = get_range (regno);
	    changed |= bitmap_ior (curr, curr, range);
	  }
	else
	  changed |= add_range (regno, range);
      }
    return changed;
  }

  /* Return true if regno's live range is full.  */
  bool full_p (unsigned int regno) const
  {
    return find_p (regno) && bitmap_full_p (get_range (regno));
  }
  /* Return true if regno's live range is empty.  */
  bool empty_p (unsigned int regno) const
  {
    return !find_p (regno) || bitmap_empty_p (get_range (regno));
  }

  /* Debug helper.  */
  void dump (FILE *file, const char *indent = ";;     ") const;
};

#endif /* GCC_SUBREG_LIVE_RANGE_H */
