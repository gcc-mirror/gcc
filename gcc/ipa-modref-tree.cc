/* Data structure for the modref pass.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Contributed by David Cepelik and Jan Hubicka

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "ipa-modref-tree.h"
#include "selftest.h"
#include "tree-ssa-alias.h"
#include "gimple.h"
#include "cgraph.h"
#include "tree-streamer.h"

/* Return true if both accesses are the same.  */
bool
modref_access_node::operator == (modref_access_node &a) const
{
  if (parm_index != a.parm_index)
    return false;
  if (parm_index != MODREF_UNKNOWN_PARM
      && parm_index != MODREF_GLOBAL_MEMORY_PARM)
    {
      if (parm_offset_known != a.parm_offset_known)
	return false;
      if (parm_offset_known
	  && !known_eq (parm_offset, a.parm_offset))
	return false;
    }
  if (range_info_useful_p () != a.range_info_useful_p ())
    return false;
  if (range_info_useful_p ()
      && (!known_eq (a.offset, offset)
	  || !known_eq (a.size, size)
	  || !known_eq (a.max_size, max_size)))
    return false;
  return true;
}

/* Return true A is a subaccess.  */
bool
modref_access_node::contains (const modref_access_node &a) const
{
  poly_int64 aoffset_adj = 0;
  if (parm_index != MODREF_UNKNOWN_PARM)
    {
      if (parm_index != a.parm_index)
	return false;
      if (parm_offset_known)
	{
	   if (!a.parm_offset_known)
	     return false;
	   /* Accesses are never below parm_offset, so look
	      for smaller offset.
	      If access ranges are known still allow merging
	      when bit offsets comparison passes.  */
	   if (!known_le (parm_offset, a.parm_offset)
	       && !range_info_useful_p ())
	     return false;
	   /* We allow negative aoffset_adj here in case
	      there is an useful range.  This is because adding
	      a.offset may result in non-negative offset again.
	      Ubsan fails on val << LOG_BITS_PER_UNIT where val
	      is negative.  */
	   aoffset_adj = (a.parm_offset - parm_offset)
			 * BITS_PER_UNIT;
	}
    }
  if (range_info_useful_p ())
    {
      if (!a.range_info_useful_p ())
	return false;
      /* Sizes of stores are used to check that object is big enough
	 to fit the store, so smaller or unknown store is more general
	 than large store.  */
      if (known_size_p (size)
	  && (!known_size_p (a.size)
	      || !known_le (size, a.size)))
	return false;
      if (known_size_p (max_size))
	return known_subrange_p (a.offset + aoffset_adj,
				 a.max_size, offset, max_size);
      else
	return known_le (offset, a.offset + aoffset_adj);
    }
  return true;
}

/* Update access range to new parameters.
   If RECORD_ADJUSTMENTS is true, record number of changes in the access
   and if threshold is exceeded start dropping precision
   so only constantly many updates are possible.  This makes dataflow
   to converge.  */
void
modref_access_node::update (poly_int64 parm_offset1,
			    poly_int64 offset1, poly_int64 size1,
			    poly_int64 max_size1, bool record_adjustments)
{
  if (known_eq (parm_offset, parm_offset1)
      && known_eq (offset, offset1)
      && known_eq (size, size1)
      && known_eq (max_size, max_size1))
    return;
  if (!record_adjustments
      || (++adjustments) < param_modref_max_adjustments)
    {
      parm_offset = parm_offset1;
      offset = offset1;
      size = size1;
      max_size = max_size1;
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, "--param modref-max-adjustments limit reached:");
      if (!known_eq (parm_offset, parm_offset1))
	{
	  if (dump_file)
	    fprintf (dump_file, " parm_offset cleared");
	  parm_offset_known = false;
	}
      if (!known_eq (size, size1))
	{
	  size = -1;
	  if (dump_file)
	    fprintf (dump_file, " size cleared");
	}
      if (!known_eq (max_size, max_size1))
	{
	  max_size = -1;
	  if (dump_file)
	    fprintf (dump_file, " max_size cleared");
	}
      if (!known_eq (offset, offset1))
	{
	  offset = 0;
	  if (dump_file)
	    fprintf (dump_file, " offset cleared");
	}
      if (dump_file)
	fprintf (dump_file, "\n");
    }
}

/* Merge in access A if it is possible to do without losing
   precision.  Return true if successful.
   If RECORD_ADJUSTMENTs is true, remember how many interval
   was prolonged and punt when there are too many.  */
bool
modref_access_node::merge (const modref_access_node &a,
			   bool record_adjustments)
{
  poly_int64 offset1 = 0;
  poly_int64 aoffset1 = 0;
  poly_int64 new_parm_offset = 0;

  /* We assume that containment was tested earlier.  */
  gcc_checking_assert (!contains (a) && !a.contains (*this));
  if (parm_index != MODREF_UNKNOWN_PARM)
    {
      if (parm_index != a.parm_index)
	return false;
      if (parm_offset_known)
	{
	  if (!a.parm_offset_known)
	    return false;
	  if (!combined_offsets (a, &new_parm_offset, &offset1, &aoffset1))
	    return false;
	}
    }
  /* See if we can merge ranges.  */
  if (range_info_useful_p ())
    {
      /* In this case we have containment that should be
	 handled earlier.  */
      gcc_checking_assert (a.range_info_useful_p ());

      /* If a.size is less specified than size, merge only
	 if intervals are otherwise equivalent.  */
      if (known_size_p (size)
	  && (!known_size_p (a.size) || known_lt (a.size, size)))
	{
	  if (((known_size_p (max_size) || known_size_p (a.max_size))
	       && !known_eq (max_size, a.max_size))
	       || !known_eq (offset1, aoffset1))
	    return false;
	  update (new_parm_offset, offset1, a.size, max_size,
		  record_adjustments);
	  return true;
	}
      /* If sizes are same, we can extend the interval.  */
      if ((known_size_p (size) || known_size_p (a.size))
	  && !known_eq (size, a.size))
	return false;
      if (known_le (offset1, aoffset1))
	{
	  if (!known_size_p (max_size)
	      || known_ge (offset1 + max_size, aoffset1))
	    {
	      update2 (new_parm_offset, offset1, size, max_size,
		       aoffset1, a.size, a.max_size,
		       record_adjustments);
	      return true;
	    }
	}
      else if (known_le (aoffset1, offset1))
	{
	  if (!known_size_p (a.max_size)
	      || known_ge (aoffset1 + a.max_size, offset1))
	    {
	      update2 (new_parm_offset, offset1, size, max_size,
		       aoffset1, a.size, a.max_size,
		       record_adjustments);
	      return true;
	    }
	}
      return false;
    }
  update (new_parm_offset, offset1,
	  size, max_size, record_adjustments);
  return true;
}

/* Return true if A1 and B1 can be merged with lower information
   less than A2 and B2.
   Assume that no containment or lossless merging is possible.  */
bool
modref_access_node::closer_pair_p (const modref_access_node &a1,
				   const modref_access_node &b1,
				   const modref_access_node &a2,
				   const modref_access_node &b2)
{
  /* Merging different parm indexes comes to complete loss
     of range info.  */
  if (a1.parm_index != b1.parm_index)
    return false;
  if (a2.parm_index != b2.parm_index)
    return true;
  /* If parm is known and parm indexes are the same we should
     already have containment.  */
  gcc_checking_assert (a1.parm_offset_known && b1.parm_offset_known);
  gcc_checking_assert (a2.parm_offset_known && b2.parm_offset_known);

  /* First normalize offsets for parm offsets.  */
  poly_int64 new_parm_offset, offseta1, offsetb1, offseta2, offsetb2;
  if (!a1.combined_offsets (b1, &new_parm_offset, &offseta1, &offsetb1)
      || !a2.combined_offsets (b2, &new_parm_offset, &offseta2, &offsetb2))
    gcc_unreachable ();


  /* Now compute distance of the intervals.  */
  poly_offset_int dist1, dist2;
  if (known_le (offseta1, offsetb1))
    {
      if (!known_size_p (a1.max_size))
	dist1 = 0;
      else
	dist1 = (poly_offset_int)offsetb1
		- (poly_offset_int)offseta1
		- (poly_offset_int)a1.max_size;
    }
  else
    {
      if (!known_size_p (b1.max_size))
	dist1 = 0;
      else
	dist1 = (poly_offset_int)offseta1
		 - (poly_offset_int)offsetb1
		 - (poly_offset_int)b1.max_size;
    }
  if (known_le (offseta2, offsetb2))
    {
      if (!known_size_p (a2.max_size))
	dist2 = 0;
      else
	dist2 = (poly_offset_int)offsetb2
		- (poly_offset_int)offseta2
		- (poly_offset_int)a2.max_size;
    }
  else
    {
      if (!known_size_p (b2.max_size))
	dist2 = 0;
      else
	dist2 = offseta2
		- (poly_offset_int)offsetb2
		- (poly_offset_int)b2.max_size;
    }
  /* It may happen that intervals overlap in case size
     is different.  Prefer the overlap to non-overlap.  */
  if (known_lt (dist1, 0) && known_ge (dist2, 0))
    return true;
  if (known_lt (dist2, 0) && known_ge (dist1, 0))
    return false;
  if (known_lt (dist1, 0))
    /* If both overlaps minimize overlap.  */
    return known_le (dist2, dist1);
  else
    /* If both are disjoint look for smaller distance.  */
    return known_le (dist1, dist2);
}

/* Merge in access A while losing precision.  */
void
modref_access_node::forced_merge (const modref_access_node &a,
				  bool record_adjustments)
{
  if (parm_index != a.parm_index)
    {
      gcc_checking_assert (parm_index != MODREF_UNKNOWN_PARM);
      parm_index = MODREF_UNKNOWN_PARM;
      return;
    }

  /* We assume that containment and lossless merging
     was tested earlier.  */
  gcc_checking_assert (!contains (a) && !a.contains (*this)
		       && !merge (a, record_adjustments));
  gcc_checking_assert (parm_offset_known && a.parm_offset_known);

  poly_int64 new_parm_offset, offset1, aoffset1;
  if (!combined_offsets (a, &new_parm_offset, &offset1, &aoffset1))
    {
      parm_offset_known = false;
      return;
    }
  gcc_checking_assert (range_info_useful_p ()
		       && a.range_info_useful_p ());
  if (record_adjustments)
    adjustments += a.adjustments;
  update2 (new_parm_offset,
	   offset1, size, max_size,
	   aoffset1, a.size, a.max_size,
	   record_adjustments);
}

/* Merge two ranges both starting at parm_offset1 and update THIS
   with result.  */
void
modref_access_node::update2 (poly_int64 parm_offset1,
			     poly_int64 offset1, poly_int64 size1,
			     poly_int64 max_size1,
			     poly_int64 offset2, poly_int64 size2,
			     poly_int64 max_size2,
			     bool record_adjustments)
{
  poly_int64 new_size = size1;

  if (!known_size_p (size2)
      || known_le (size2, size1))
    new_size = size2;
  else
    gcc_checking_assert (known_le (size1, size2));

  if (known_le (offset1, offset2))
    ;
  else if (known_le (offset2, offset1))
    {
      std::swap (offset1, offset2);
      std::swap (max_size1, max_size2);
    }
  else
    gcc_unreachable ();

  poly_int64 new_max_size;

  if (!known_size_p (max_size1))
    new_max_size = max_size1;
  else if (!known_size_p (max_size2))
    new_max_size = max_size2;
  else
    {
      poly_offset_int s = (poly_offset_int)max_size2
			  + (poly_offset_int)offset2
			  - (poly_offset_int)offset1;
      if (s.to_shwi (&new_max_size))
	{
	  if (known_le (new_max_size, max_size1))
	    new_max_size = max_size1;
	}
      else
	new_max_size = -1;
    }

  update (parm_offset1, offset1,
	  new_size, new_max_size, record_adjustments);
}

/* Given access nodes THIS and A, return true if they
   can be done with common parm_offsets.  In this case
   return parm offset in new_parm_offset, new_offset
   which is start of range in THIS and new_aoffset that
   is start of range in A.  */
bool
modref_access_node::combined_offsets (const modref_access_node &a,
				      poly_int64 *new_parm_offset,
				      poly_int64 *new_offset,
				      poly_int64 *new_aoffset) const
{
  gcc_checking_assert (parm_offset_known && a.parm_offset_known);
  if (known_le (a.parm_offset, parm_offset))
    {
      *new_offset = offset
		    + ((parm_offset - a.parm_offset)
		       << LOG2_BITS_PER_UNIT);
      *new_aoffset = a.offset;
      *new_parm_offset = a.parm_offset;
      return true;
    }
  else if (known_le (parm_offset, a.parm_offset))
    {
      *new_aoffset = a.offset
		      + ((a.parm_offset - parm_offset)
			 << LOG2_BITS_PER_UNIT);
      *new_offset = offset;
      *new_parm_offset = parm_offset;
      return true;
    }
  else
    return false;
}

/* Try to optimize the access ACCESSES list after entry INDEX was modified.  */
void
modref_access_node::try_merge_with (vec <modref_access_node, va_gc> *&accesses,
				    size_t index)
{
  size_t i;

  for (i = 0; i < accesses->length ();)
    if (i != index)
      {
	bool found = false, restart = false;
	modref_access_node *a = &(*accesses)[i];
	modref_access_node *n = &(*accesses)[index];

	if (n->contains (*a))
	  found = true;
	if (!found && n->merge (*a, false))
	  found = restart = true;
	gcc_checking_assert (found || !a->merge (*n, false));
	if (found)
	  {
	    accesses->unordered_remove (i);
	    if (index == accesses->length ())
	      {
		index = i;
		i++;
	      }
	    if (restart)
	      i = 0;
	  }
	else
	  i++;
      }
    else
      i++;
}

/* Stream out to OB.  */

void
modref_access_node::stream_out (struct output_block *ob) const
{
  streamer_write_hwi (ob, parm_index);
  if (parm_index != MODREF_UNKNOWN_PARM)
    {
      streamer_write_uhwi (ob, parm_offset_known);
      if (parm_offset_known)
	{
	  streamer_write_poly_int64 (ob, parm_offset);
	  streamer_write_poly_int64 (ob, offset);
	  streamer_write_poly_int64 (ob, size);
	  streamer_write_poly_int64 (ob, max_size);
	}
    }
}

modref_access_node
modref_access_node::stream_in (struct lto_input_block *ib)
{
  int parm_index = streamer_read_hwi (ib);
  bool parm_offset_known = false;
  poly_int64 parm_offset = 0;
  poly_int64 offset = 0;
  poly_int64 size = -1;
  poly_int64 max_size = -1;

  if (parm_index != MODREF_UNKNOWN_PARM)
    {
      parm_offset_known = streamer_read_uhwi (ib);
      if (parm_offset_known)
	{
	  parm_offset = streamer_read_poly_int64 (ib);
	  offset = streamer_read_poly_int64 (ib);
	  size = streamer_read_poly_int64 (ib);
	  max_size = streamer_read_poly_int64 (ib);
	}
    }
  return {offset, size, max_size, parm_offset, parm_index,
	  parm_offset_known, false};
}

/* Insert access with OFFSET and SIZE.
   Collapse tree if it has more than MAX_ACCESSES entries.
   If RECORD_ADJUSTMENTs is true avoid too many interval extensions.
   Return true if record was changed.

   Return 0 if nothing changed, 1 if insert was successful and -1
   if entries should be collapsed.  */
int
modref_access_node::insert (vec <modref_access_node, va_gc> *&accesses,
			    modref_access_node a, size_t max_accesses,
			    bool record_adjustments)
{
  size_t i, j;
  modref_access_node *a2;

  /* Verify that list does not contain redundant accesses.  */
  if (flag_checking)
    {
      size_t i, i2;
      modref_access_node *a, *a2;

      FOR_EACH_VEC_SAFE_ELT (accesses, i, a)
	{
	  FOR_EACH_VEC_SAFE_ELT (accesses, i2, a2)
	    if (i != i2)
	      gcc_assert (!a->contains (*a2));
	}
    }

  FOR_EACH_VEC_SAFE_ELT (accesses, i, a2)
    {
      if (a2->contains (a))
	return 0;
      if (a.contains (*a2))
	{
	  a.adjustments = 0;
	  a2->parm_index = a.parm_index;
	  a2->parm_offset_known = a.parm_offset_known;
	  a2->update (a.parm_offset, a.offset, a.size, a.max_size,
		      record_adjustments);
	  modref_access_node::try_merge_with (accesses, i);
	  return 1;
	}
      if (a2->merge (a, record_adjustments))
	{
	  modref_access_node::try_merge_with (accesses, i);
	  return 1;
	}
      gcc_checking_assert (!(a == *a2));
    }

  /* If this base->ref pair has too many accesses stored, we will clear
     all accesses and bail out.  */
  if (accesses && accesses->length () >= max_accesses)
    {
      if (max_accesses < 2)
	return -1;
      /* Find least harmful merge and perform it.  */
      int best1 = -1, best2 = -1;
      FOR_EACH_VEC_SAFE_ELT (accesses, i, a2)
	{
	  for (j = i + 1; j < accesses->length (); j++)
	    if (best1 < 0
		|| modref_access_node::closer_pair_p
		     (*a2, (*accesses)[j],
		      (*accesses)[best1],
		      best2 < 0 ? a : (*accesses)[best2]))
	      {
		best1 = i;
		best2 = j;
	      }
	  if (modref_access_node::closer_pair_p
		     (*a2, a,
		      (*accesses)[best1],
		      best2 < 0 ? a : (*accesses)[best2]))
	    {
	      best1 = i;
	      best2 = -1;
	    }
	}
      (*accesses)[best1].forced_merge (best2 < 0 ? a : (*accesses)[best2],
				       record_adjustments);
      /* Check that merging indeed merged ranges.  */
      gcc_checking_assert ((*accesses)[best1].contains
			       (best2 < 0 ? a : (*accesses)[best2]));
      if (!(*accesses)[best1].useful_p ())
	return -1;
      if (dump_file && best2 >= 0)
	fprintf (dump_file,
		 "--param modref-max-accesses limit reached;"
		 " merging %i and %i\n", best1, best2);
      else if (dump_file)
	fprintf (dump_file,
		 "--param modref-max-accesses limit reached;"
		 " merging with %i\n", best1);
      modref_access_node::try_merge_with (accesses, best1);
      if (best2 >= 0)
	insert (accesses, a, max_accesses, record_adjustments);
      return 1;
    }
  a.adjustments = 0;
  vec_safe_push (accesses, a);
  return 1;
}

/* Return true if range info is useful.  */
bool
modref_access_node::range_info_useful_p () const
{
  return parm_index != MODREF_UNKNOWN_PARM
	 && parm_index != MODREF_GLOBAL_MEMORY_PARM
	 && parm_offset_known
	 && (known_size_p (size)
	     || known_size_p (max_size)
	     || known_ge (offset, 0));
}

/* Dump range to debug OUT.  */
void
modref_access_node::dump (FILE *out)
{
  if (parm_index != MODREF_UNKNOWN_PARM)
    {
      if (parm_index == MODREF_GLOBAL_MEMORY_PARM)
	fprintf (out, " Base in global memory");
      else if (parm_index >= 0)
	fprintf (out, " Parm %i", parm_index);
      else if (parm_index == MODREF_STATIC_CHAIN_PARM)
	fprintf (out, " Static chain");
      else
	gcc_unreachable ();
      if (parm_offset_known)
	{
	  fprintf (out, " param offset:");
	  print_dec ((poly_int64)parm_offset, out, SIGNED);
	}
    }
  if (range_info_useful_p ())
    {
      fprintf (out, " offset:");
      print_dec ((poly_int64)offset, out, SIGNED);
      fprintf (out, " size:");
      print_dec ((poly_int64)size, out, SIGNED);
      fprintf (out, " max_size:");
      print_dec ((poly_int64)max_size, out, SIGNED);
      if (adjustments)
	fprintf (out, " adjusted %i times", adjustments);
    }
  fprintf (out, "\n");
}

/* Return tree corresponding to parameter of the range in STMT.  */
tree
modref_access_node::get_call_arg (const gcall *stmt) const
{
  if (parm_index == MODREF_UNKNOWN_PARM
      || parm_index == MODREF_GLOBAL_MEMORY_PARM)
    return NULL;
  if (parm_index == MODREF_STATIC_CHAIN_PARM)
    return gimple_call_chain (stmt);
  /* MODREF_RETSLOT_PARM should not happen in access trees since the store
     is seen explicitly in the caller.  */
  gcc_checking_assert (parm_index >= 0);
  if (parm_index >= (int)gimple_call_num_args (stmt))
    return NULL;
  return gimple_call_arg (stmt, parm_index);
}

/* Return tree corresponding to parameter of the range in STMT.  */
bool
modref_access_node::get_ao_ref (const gcall *stmt, ao_ref *ref) const
{
  tree arg;

  if (!parm_offset_known
      || !(arg = get_call_arg (stmt))
      || !POINTER_TYPE_P (TREE_TYPE (arg)))
    return false;
  poly_offset_int off = (poly_offset_int)offset
	+ ((poly_offset_int)parm_offset << LOG2_BITS_PER_UNIT);
  poly_int64 off2;
  if (!off.to_shwi (&off2))
    return false;
  ao_ref_init_from_ptr_and_range (ref, arg, true, off2, size, max_size);
  return true;
}

/* Return true A is a subkill.  */
bool
modref_access_node::contains_for_kills (const modref_access_node &a) const
{
  poly_int64 aoffset_adj = 0;

  gcc_checking_assert (parm_index != MODREF_UNKNOWN_PARM
		       && a.parm_index != MODREF_UNKNOWN_PARM);
  if (parm_index != a.parm_index)
    return false;
  gcc_checking_assert (parm_offset_known && a.parm_offset_known);
  aoffset_adj = (a.parm_offset - parm_offset)
		* BITS_PER_UNIT;
  gcc_checking_assert (range_info_useful_p () && a.range_info_useful_p ());
  return known_subrange_p (a.offset + aoffset_adj,
			   a.max_size, offset, max_size);
}

/* Merge two ranges both starting at parm_offset1 and update THIS
   with result.  */
bool
modref_access_node::update_for_kills (poly_int64 parm_offset1,
				      poly_int64 offset1,
				      poly_int64 max_size1,
				      poly_int64 offset2,
				      poly_int64 max_size2,
				      bool record_adjustments)
{
  if (known_le (offset1, offset2))
    ;
  else if (known_le (offset2, offset1))
    {
      std::swap (offset1, offset2);
      std::swap (max_size1, max_size2);
    }
  else
    gcc_unreachable ();

  poly_int64 new_max_size = max_size2 + offset2 - offset1;
  if (known_le (new_max_size, max_size1))
    new_max_size = max_size1;
  if (known_eq (parm_offset, parm_offset1)
      && known_eq (offset, offset1)
      && known_eq (size, new_max_size)
      && known_eq (max_size, new_max_size))
    return false;

  if (!record_adjustments
      || (++adjustments) < param_modref_max_adjustments)
    {
      parm_offset = parm_offset1;
      offset = offset1;
      max_size = new_max_size;
      size = new_max_size;
      gcc_checking_assert (useful_for_kill_p ());
      return true;
    }
  return false;
}

/* Merge in access A if it is possible to do without losing
   precision.  Return true if successful.
   Unlike merge assume that both accesses are always executed
   and merge size the same was as max_size.  */
bool
modref_access_node::merge_for_kills (const modref_access_node &a,
				     bool record_adjustments)
{
  poly_int64 offset1 = 0;
  poly_int64 aoffset1 = 0;
  poly_int64 new_parm_offset = 0;

  /* We assume that containment was tested earlier.  */
  gcc_checking_assert (!contains_for_kills (a) && !a.contains_for_kills (*this)
		       && useful_for_kill_p () && a.useful_for_kill_p ());

  if (parm_index != a.parm_index
      || !combined_offsets (a, &new_parm_offset, &offset1, &aoffset1))
    return false;

  if (known_le (offset1, aoffset1))
   {
     if (!known_size_p (max_size)
	 || known_ge (offset1 + max_size, aoffset1))
       return update_for_kills (new_parm_offset, offset1, max_size,
				aoffset1, a.max_size, record_adjustments);
   }
  else if (known_le (aoffset1, offset1))
   {
     if (!known_size_p (a.max_size)
	 || known_ge (aoffset1 + a.max_size, offset1))
       return update_for_kills (new_parm_offset, offset1, max_size,
				aoffset1, a.max_size, record_adjustments);
   }
  return false;
}

/* Insert new kill A into KILLS.  If RECORD_ADJUSTMENTS is true limit number
   of changes to each entry.  Return true if something changed.  */

bool
modref_access_node::insert_kill (vec<modref_access_node> &kills,
				 modref_access_node &a, bool record_adjustments)
{
  size_t index;
  modref_access_node *a2;
  bool merge = false;

  gcc_checking_assert (a.useful_for_kill_p ());

  /* See if we have corresponding entry already or we can merge with
     neighboring entry.  */
  FOR_EACH_VEC_ELT (kills, index, a2)
    {
      if (a2->contains_for_kills (a))
	return false;
      if (a.contains_for_kills (*a2))
	{
	  a.adjustments = 0;
	  *a2 = a;
	  merge = true;
	  break;
	}
      if (a2->merge_for_kills (a, record_adjustments))
	{
	  merge = true;
	  break;
	}
    }
  /* If entry was not found, insert it.  */
  if (!merge)
    {
      if ((int)kills.length () >= param_modref_max_accesses)
	{
	  if (dump_file)
	    fprintf (dump_file, "--param modref-max-accesses limit reached:");
	  return false;
	}
      a.adjustments = 0;
      kills.safe_push (a);
      return true;
    }
  /* Extending range in an entry may make it possible to merge it with
     other entries.  */
  size_t i;

  for (i = 0; i < kills.length ();)
    if (i != index)
      {
	bool found = false, restart = false;
	modref_access_node *a = &kills[i];
	modref_access_node *n = &kills[index];

	if (n->contains_for_kills (*a))
	  found = true;
	if (!found && n->merge_for_kills (*a, false))
	  found = restart = true;
	gcc_checking_assert (found || !a->merge_for_kills (*n, false));
	if (found)
	  {
	    kills.unordered_remove (i);
	    if (index == kills.length ())
	      {
		index = i;
		i++;
	      }
	    if (restart)
	      i = 0;
	  }
	else
	  i++;
      }
    else
      i++;
  return true;
}


#if CHECKING_P

namespace selftest {

static void
test_insert_search_collapse ()
{
  modref_base_node<alias_set_type> *base_node;
  modref_ref_node<alias_set_type> *ref_node;
  modref_access_node a = unspecified_modref_access_node;

  modref_tree<alias_set_type> *t = new modref_tree<alias_set_type>();
  ASSERT_FALSE (t->every_base);

  /* Insert into an empty tree.  */
  t->insert (1, 2, 2, 1, 2, a, false);
  ASSERT_NE (t->bases, NULL);
  ASSERT_EQ (t->bases->length (), 1);
  ASSERT_FALSE (t->every_base);
  ASSERT_EQ (t->search (2), NULL);

  base_node = t->search (1);
  ASSERT_NE (base_node, NULL);
  ASSERT_EQ (base_node->base, 1);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_EQ (base_node->refs->length (), 1);
  ASSERT_EQ (base_node->search (1), NULL);

  ref_node = base_node->search (2);
  ASSERT_NE (ref_node, NULL);
  ASSERT_EQ (ref_node->ref, 2);

  /* Insert when base exists but ref does not.  */
  t->insert (1, 2, 2, 1, 3, a, false);
  ASSERT_NE (t->bases, NULL);
  ASSERT_EQ (t->bases->length (), 1);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (t->search (2), NULL);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_EQ (base_node->refs->length (), 2);

  ref_node = base_node->search (3);
  ASSERT_NE (ref_node, NULL);

  /* Insert when base and ref exist, but access is not dominated by nor
     dominates other accesses.  */
  t->insert (1, 2, 2, 1, 2, a, false);
  ASSERT_EQ (t->bases->length (), 1);
  ASSERT_EQ (t->search (1), base_node);

  ref_node = base_node->search (2);
  ASSERT_NE (ref_node, NULL);

  /* Insert when base and ref exist and access is dominated.  */
  t->insert (1, 2, 2, 1, 2, a, false);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (base_node->search (2), ref_node);

  /* Insert ref to trigger ref list collapse for base 1.  */
  t->insert (1, 2, 2, 1, 4, a, false);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (base_node->refs, NULL);
  ASSERT_EQ (base_node->search (2), NULL);
  ASSERT_EQ (base_node->search (3), NULL);
  ASSERT_TRUE (base_node->every_ref);

  /* Further inserts to collapsed ref list are ignored.  */
  t->insert (1, 2, 2, 1, 5, a, false);
  ASSERT_EQ (t->search (1), base_node);
  ASSERT_EQ (base_node->refs, NULL);
  ASSERT_EQ (base_node->search (2), NULL);
  ASSERT_EQ (base_node->search (3), NULL);
  ASSERT_TRUE (base_node->every_ref);

  /* Insert base to trigger base list collapse.  */
  t->insert (1, 2, 2, 5, 0, a, false);
  ASSERT_TRUE (t->every_base);
  ASSERT_EQ (t->bases, NULL);
  ASSERT_EQ (t->search (1), NULL);

  /* Further inserts to collapsed base list are ignored.  */
  t->insert (1, 2, 2, 7, 8, a, false);
  ASSERT_TRUE (t->every_base);
  ASSERT_EQ (t->bases, NULL);
  ASSERT_EQ (t->search (1), NULL);

  delete t;
}

static void
test_merge ()
{
  modref_tree<alias_set_type> *t1, *t2;
  modref_base_node<alias_set_type> *base_node;
  modref_access_node a = unspecified_modref_access_node;

  t1 = new modref_tree<alias_set_type>();
  t1->insert (3, 4, 1, 1, 1, a, false);
  t1->insert (3, 4, 1, 1, 2, a, false);
  t1->insert (3, 4, 1, 1, 3, a, false);
  t1->insert (3, 4, 1, 2, 1, a, false);
  t1->insert (3, 4, 1, 3, 1, a, false);

  t2 = new modref_tree<alias_set_type>();
  t2->insert (10, 10, 10, 1, 2, a, false);
  t2->insert (10, 10, 10, 1, 3, a, false);
  t2->insert (10, 10, 10, 1, 4, a, false);
  t2->insert (10, 10, 10, 3, 2, a, false);
  t2->insert (10, 10, 10, 3, 3, a, false);
  t2->insert (10, 10, 10, 3, 4, a, false);
  t2->insert (10, 10, 10, 3, 5, a, false);

  t1->merge (3, 4, 1, t2, NULL, NULL, false);

  ASSERT_FALSE (t1->every_base);
  ASSERT_NE (t1->bases, NULL);
  ASSERT_EQ (t1->bases->length (), 3);

  base_node = t1->search (1);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_FALSE (base_node->every_ref);
  ASSERT_EQ (base_node->refs->length (), 4);

  base_node = t1->search (2);
  ASSERT_NE (base_node->refs, NULL);
  ASSERT_FALSE (base_node->every_ref);
  ASSERT_EQ (base_node->refs->length (), 1);

  base_node = t1->search (3);
  ASSERT_EQ (base_node->refs, NULL);
  ASSERT_TRUE (base_node->every_ref);

  delete t1;
  delete t2;
}


void
ipa_modref_tree_cc_tests ()
{
  test_insert_search_collapse ();
  test_merge ();
}

} // namespace selftest

#endif

void
gt_ggc_mx (modref_tree < int >*const &tt)
{
  if (tt->bases)
    {
      ggc_test_and_set_mark (tt->bases);
      gt_ggc_mx (tt->bases);
    }
}

void
gt_ggc_mx (modref_tree < tree_node * >*const &tt)
{
  if (tt->bases)
    {
      ggc_test_and_set_mark (tt->bases);
      gt_ggc_mx (tt->bases);
    }
}

void gt_pch_nx (modref_tree<int>* const&) {}
void gt_pch_nx (modref_tree<tree_node*>* const&) {}
void gt_pch_nx (modref_tree<int>* const&, gt_pointer_operator, void *) {}
void gt_pch_nx (modref_tree<tree_node*>* const&, gt_pointer_operator, void *) {}

void gt_ggc_mx (modref_base_node<int>* &b)
{
  ggc_test_and_set_mark (b);
  if (b->refs)
    {
      ggc_test_and_set_mark (b->refs);
      gt_ggc_mx (b->refs);
    }
}

void gt_ggc_mx (modref_base_node<tree_node*>* &b)
{
  ggc_test_and_set_mark (b);
  if (b->refs)
    {
      ggc_test_and_set_mark (b->refs);
      gt_ggc_mx (b->refs);
    }
  if (b->base)
    gt_ggc_mx (b->base);
}

void gt_pch_nx (modref_base_node<int>*) {}
void gt_pch_nx (modref_base_node<tree_node*>*) {}
void gt_pch_nx (modref_base_node<int>*, gt_pointer_operator, void *) {}
void gt_pch_nx (modref_base_node<tree_node*>*, gt_pointer_operator, void *) {}

void gt_ggc_mx (modref_ref_node<int>* &r)
{
  ggc_test_and_set_mark (r);
  if (r->accesses)
    {
      ggc_test_and_set_mark (r->accesses);
      gt_ggc_mx (r->accesses);
    }
}

void gt_ggc_mx (modref_ref_node<tree_node*>* &r)
{
  ggc_test_and_set_mark (r);
  if (r->accesses)
    {
      ggc_test_and_set_mark (r->accesses);
      gt_ggc_mx (r->accesses);
    }
  if (r->ref)
    gt_ggc_mx (r->ref);
}

void gt_pch_nx (modref_ref_node<int>* ) {}
void gt_pch_nx (modref_ref_node<tree_node*>*) {}
void gt_pch_nx (modref_ref_node<int>*, gt_pointer_operator, void *) {}
void gt_pch_nx (modref_ref_node<tree_node*>*, gt_pointer_operator, void *) {}

void gt_ggc_mx (modref_access_node &)
{
}
