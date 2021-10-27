/* Data structure for the modref pass.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

/* modref_tree represent a decision tree that can be used by alias analysis
   oracle to determine whether given memory access can be affected by a function
   call.  For every function we collect two trees, one for loads and other
   for stores.  Tree consist of following levels:

   1) Base: this level represent base alias set of the access and refers
      to sons (ref nodes). Flag all_refs means that all possible references
      are aliasing.

      Because for LTO streaming we need to stream types rather than alias sets
      modref_base_node is implemented as a template.
   2) Ref: this level represent ref alias set and links to accesses unless
      all_refs flag is set.
      Again ref is an template to allow LTO streaming.
   3) Access: this level represent info about individual accesses.  Presently
      we record whether access is through a dereference of a function parameter
      and if so we record the access range.
*/

#ifndef GCC_MODREF_TREE_H
#define GCC_MODREF_TREE_H

struct ipa_modref_summary;

/* Memory access.  */
struct GTY(()) modref_access_node
{

  /* Access range information (in bits).  */
  poly_int64 offset;
  poly_int64 size;
  poly_int64 max_size;

  /* Offset from parameter pointer to the base of the access (in bytes).  */
  poly_int64 parm_offset;

  /* Index of parameter which specifies the base of access. -1 if base is not
     a function parameter.  */
  int parm_index;
  bool parm_offset_known;
  /* Number of times interval was extended during dataflow.
     This has to be limited in order to keep dataflow finite.  */
  unsigned char adjustments;

  /* Return true if access node holds no useful info.  */
  bool useful_p () const
    {
      return parm_index != -1;
    }
  /* Return true if range info is useful.  */
  bool range_info_useful_p () const
    {
      return parm_index != -1 && parm_offset_known
	     && (known_size_p (size)
		 || known_size_p (max_size)
		 || known_ge (offset, 0));
    }
  /* Return true if both accesses are the same.  */
  bool operator == (modref_access_node &a) const
    {
      if (parm_index != a.parm_index)
	return false;
      if (parm_index >= 0)
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
  bool contains (const modref_access_node &a) const
    {
      poly_int64 aoffset_adj = 0;
      if (parm_index >= 0)
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
		  when bit offsets comparsion passes.  */
	       if (!known_le (parm_offset, a.parm_offset)
		   && !range_info_useful_p ())
		 return false;
	       aoffset_adj = (a.parm_offset - parm_offset)
			     << LOG2_BITS_PER_UNIT;
	    }
	}
      if (range_info_useful_p ())
	{
	  if (!a.range_info_useful_p ())
	    return false;
	  /* Sizes of stores are used to check that object is big enough
	     to fit the store, so smaller or unknown sotre is more general
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
  void update (poly_int64 parm_offset1,
	       poly_int64 offset1, poly_int64 size1, poly_int64 max_size1,
	       bool record_adjustments)
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
	    fprintf (dump_file,
		     "--param param=modref-max-adjustments limit reached:");
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
  bool merge (const modref_access_node &a, bool record_adjustments)
    {
      poly_int64 offset1 = 0;
      poly_int64 aoffset1 = 0;
      poly_int64 new_parm_offset = 0;

      /* We assume that containment was tested earlier.  */
      gcc_checking_assert (!contains (a) && !a.contains (*this));
      if (parm_index >= 0)
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
  /* Return true if A1 and B1 can be merged with lower informatoin
     less than A2 and B2.
     Assume that no containment or lossless merging is possible.  */
  static bool closer_pair_p (const modref_access_node &a1,
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


      /* Now compute distnace of the intervals.  */
      poly_int64 dist1, dist2;
      if (known_le (offseta1, offsetb1))
	{
	  if (!known_size_p (a1.max_size))
	    dist1 = 0;
	  else
	    dist1 = offsetb1 - offseta1 - a1.max_size;
	}
      else
	{
	  if (!known_size_p (b1.max_size))
	    dist1 = 0;
	  else
	    dist1 = offseta1 - offsetb1 - b1.max_size;
	}
      if (known_le (offseta2, offsetb2))
	{
	  if (!known_size_p (a2.max_size))
	    dist2 = 0;
	  else
	    dist2 = offsetb2 - offseta2 - a2.max_size;
	}
      else
	{
	  if (!known_size_p (b2.max_size))
	    dist2 = 0;
	  else
	    dist2 = offseta2 - offsetb2 - b2.max_size;
	}
      /* It may happen that intervals overlap in case size
	 is different.  Preffer the overlap to non-overlap.  */
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
  void forced_merge (const modref_access_node &a, bool record_adjustments)
    {
      if (parm_index != a.parm_index)
	{
	  gcc_checking_assert (parm_index != -1);
	  parm_index = -1;
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
private:
  /* Merge two ranges both starting at parm_offset1 and update THIS
     with result.  */
  void update2 (poly_int64 parm_offset1,
		poly_int64 offset1, poly_int64 size1, poly_int64 max_size1,
		poly_int64 offset2, poly_int64 size2, poly_int64 max_size2,
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
	  new_max_size = max_size2 + offset2 - offset1;
	  if (known_le (new_max_size, max_size1))
	    new_max_size = max_size1;
	}

      update (parm_offset1, offset1,
	      new_size, new_max_size, record_adjustments);
    }
  /* Given access nodes THIS and A, return true if they
     can be done with common parm_offsets.  In this case
     return parm offset in new_parm_offset, new_offset
     which is start of range in THIS and new_aoffset that
     is start of range in A.  */
  bool combined_offsets (const modref_access_node &a,
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
};

/* Access node specifying no useful info.  */
const modref_access_node unspecified_modref_access_node
		 = {0, -1, -1, 0, -1, false, 0};

template <typename T>
struct GTY((user)) modref_ref_node
{
  T ref;
  bool every_access;
  vec <modref_access_node, va_gc> *accesses;

  modref_ref_node (T ref):
    ref (ref),
    every_access (false),
    accesses (NULL)
  {}

  /* Collapse the tree.  */
  void collapse ()
  {
    vec_free (accesses);
    accesses = NULL;
    every_access = true;
  }

  /* Verify that list does not contain redundant accesses.  */
  void verify ()
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

  /* Insert access with OFFSET and SIZE.
     Collapse tree if it has more than MAX_ACCESSES entries.
     If RECORD_ADJUSTMENTs is true avoid too many interval extensions.
     Return true if record was changed.  */
  bool insert_access (modref_access_node a, size_t max_accesses,
		      bool record_adjustments)
  {
    /* If this base->ref pair has no access information, bail out.  */
    if (every_access)
      return false;

    /* Otherwise, insert a node for the ref of the access under the base.  */
    size_t i, j;
    modref_access_node *a2;

    if (flag_checking)
      verify ();

    if (!a.useful_p ())
      {
	if (!every_access)
	  {
	    collapse ();
	    return true;
	  }
	return false;
      }

    FOR_EACH_VEC_SAFE_ELT (accesses, i, a2)
      {
	if (a2->contains (a))
	  return false;
	if (a.contains (*a2))
	  {
	    a.adjustments = 0;
	    a2->parm_index = a.parm_index;
	    a2->parm_offset_known = a.parm_offset_known;
	    a2->update (a.parm_offset, a.offset, a.size, a.max_size,
			record_adjustments);
	    try_merge_with (i);
	    return true;
	  }
	if (a2->merge (a, record_adjustments))
	  {
	    try_merge_with (i);
	    return true;
	  }
	gcc_checking_assert (!(a == *a2));
      }

    /* If this base->ref pair has too many accesses stored, we will clear
       all accesses and bail out.  */
    if (accesses && accesses->length () >= max_accesses)
      {
	if (max_accesses < 2)
	  {
	    collapse ();
	    if (dump_file)
	      fprintf (dump_file,
		       "--param param=modref-max-accesses limit reached;"
		       " collapsing\n");
	    return true;
	  }
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
	  {
	    collapse ();
	    if (dump_file)
	      fprintf (dump_file,
		       "--param param=modref-max-accesses limit reached;"
		       " collapsing\n");
	    return true;
	  }
	if (dump_file && best2 >= 0)
	  fprintf (dump_file,
		   "--param param=modref-max-accesses limit reached;"
		   " merging %i and %i\n", best1, best2);
	else if (dump_file)
	  fprintf (dump_file,
		   "--param param=modref-max-accesses limit reached;"
		   " merging with %i\n", best1);
	try_merge_with (best1);
	if (best2 >= 0)
	  insert_access (a, max_accesses, record_adjustments);
	return 1;
      }
    a.adjustments = 0;
    vec_safe_push (accesses, a);
    return true;
  }
private:
  /* Try to optimize the access list after entry INDEX was modified.  */
  void
  try_merge_with (size_t index)
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
};

/* Base of an access.  */
template <typename T>
struct GTY((user)) modref_base_node
{
  T base;
  vec <modref_ref_node <T> *, va_gc> *refs;
  bool every_ref;

  modref_base_node (T base):
    base (base),
    refs (NULL),
    every_ref (false) {}

  /* Search REF; return NULL if failed.  */
  modref_ref_node <T> *search (T ref)
  {
    size_t i;
    modref_ref_node <T> *n;
    FOR_EACH_VEC_SAFE_ELT (refs, i, n)
      if (n->ref == ref)
	return n;
    return NULL;
  }

  /* Insert REF; collapse tree if there are more than MAX_REFS.
     Return inserted ref and if CHANGED is non-null set it to true if
     something changed.  */
  modref_ref_node <T> *insert_ref (T ref, size_t max_refs,
				   bool *changed = NULL)
  {
    modref_ref_node <T> *ref_node;

    /* If the node is collapsed, don't do anything.  */
    if (every_ref)
      return NULL;

    /* Otherwise, insert a node for the ref of the access under the base.  */
    ref_node = search (ref);
    if (ref_node)
      return ref_node;

    /* We always allow inserting ref 0.  For non-0 refs there is upper
       limit on number of entries and if exceeded,
       drop ref conservatively to 0.  */
    if (ref && refs && refs->length () >= max_refs)
      {
	if (dump_file)
	  fprintf (dump_file, "--param param=modref-max-refs limit reached;"
		   " using 0\n");
	ref = 0;
	ref_node = search (ref);
	if (ref_node)
	  return ref_node;
      }

    if (changed)
      *changed = true;

    ref_node = new (ggc_alloc <modref_ref_node <T> > ())modref_ref_node <T>
								 (ref);
    vec_safe_push (refs, ref_node);
    return ref_node;
  }

  void collapse ()
  {
    size_t i;
    modref_ref_node <T> *r;

    if (refs)
      {
	FOR_EACH_VEC_SAFE_ELT (refs, i, r)
	  {
	    r->collapse ();
	    ggc_free (r);
	  }
	vec_free (refs);
      }
    refs = NULL;
    every_ref = true;
  }
};

/* Map translating parameters across function call.  */

struct modref_parm_map
{
  /* Index of parameter we translate to.
     -1 indicates that parameter is unknown
     -2 indicates that parameter points to local memory and access can be
	discarded.  */
  int parm_index;
  bool parm_offset_known;
  poly_int64 parm_offset;
};

/* Access tree for a single function.  */
template <typename T>
struct GTY((user)) modref_tree
{
  vec <modref_base_node <T> *, va_gc> *bases;
  size_t max_bases;
  size_t max_refs;
  size_t max_accesses;
  bool every_base;

  modref_tree (size_t max_bases, size_t max_refs, size_t max_accesses):
    bases (NULL),
    max_bases (max_bases),
    max_refs (max_refs),
    max_accesses (max_accesses),
    every_base (false) {}

  /* Insert BASE; collapse tree if there are more than MAX_REFS.
     Return inserted base and if CHANGED is non-null set it to true if
     something changed.
     If table gets full, try to insert REF instead.  */

  modref_base_node <T> *insert_base (T base, T ref, bool *changed = NULL)
  {
    modref_base_node <T> *base_node;

    /* If the node is collapsed, don't do anything.  */
    if (every_base)
      return NULL;

    /* Otherwise, insert a node for the base of the access into the tree.  */
    base_node = search (base);
    if (base_node)
      return base_node;

    /* We always allow inserting base 0.  For non-0 base there is upper
       limit on number of entries and if exceeded,
       drop base conservatively to ref and if it still does not fit to 0.  */
    if (base && bases && bases->length () >= max_bases)
      {
	base_node = search (ref);
	if (base_node)
	  {
	    if (dump_file)
	      fprintf (dump_file, "--param param=modref-max-bases"
		       " limit reached; using ref\n");
	    return base_node;
	  }
	if (dump_file)
	  fprintf (dump_file, "--param param=modref-max-bases"
		   " limit reached; using 0\n");
	base = 0;
	base_node = search (base);
	if (base_node)
	  return base_node;
      }

    if (changed)
      *changed = true;

    base_node = new (ggc_alloc <modref_base_node <T> > ())
			 modref_base_node <T> (base);
    vec_safe_push (bases, base_node);
    return base_node;
  }

  /* Insert memory access to the tree.
     Return true if something changed.  */
  bool insert (T base, T ref, modref_access_node a,
	       bool record_adjustments)
  {
    if (every_base)
      return false;

    bool changed = false;

    /* No useful information tracked; collapse everything.  */
    if (!base && !ref && !a.useful_p ())
      {
	collapse ();
	return true;
      }

    modref_base_node <T> *base_node = insert_base (base, ref, &changed);
    base = base_node->base;
    /* If table got full we may end up with useless base.  */
    if (!base && !ref && !a.useful_p ())
      {
	collapse ();
	return true;
      }
    if (base_node->every_ref)
      return changed;
    gcc_checking_assert (search (base) != NULL);

    /* No useful ref info tracked; collapse base.  */
    if (!ref && !a.useful_p ())
      {
	base_node->collapse ();
	return true;
      }

    modref_ref_node <T> *ref_node = base_node->insert_ref (ref, max_refs,
							   &changed);
    ref = ref_node->ref;

    if (ref_node->every_access)
      return changed;
    changed |= ref_node->insert_access (a, max_accesses,
					record_adjustments);
    /* See if we failed to add useful access.  */
    if (ref_node->every_access)
      {
	/* Collapse everything if there is no useful base and ref.  */
	if (!base && !ref)
	  {
	    collapse ();
	    gcc_checking_assert (changed);
	  }
	/* Collapse base if there is no useful ref.  */
	else if (!ref)
	  {
	    base_node->collapse ();
	    gcc_checking_assert (changed);
	  }
      }
    return changed;
  }

 /* Remove tree branches that are not useful (i.e. they will always pass).  */

 void cleanup ()
 {
   size_t i, j;
   modref_base_node <T> *base_node;
   modref_ref_node <T> *ref_node;

   if (!bases)
     return;

   for (i = 0; vec_safe_iterate (bases, i, &base_node);)
     {
       if (base_node->refs)
	 for (j = 0; vec_safe_iterate (base_node->refs, j, &ref_node);)
	   {
	     if (!ref_node->every_access
		 && (!ref_node->accesses
		     || !ref_node->accesses->length ()))
	       {
		 base_node->refs->unordered_remove (j);
		 vec_free (ref_node->accesses);
		 ggc_delete (ref_node);
	       }
	     else
	       j++;
	   }
       if (!base_node->every_ref
	   && (!base_node->refs || !base_node->refs->length ()))
	 {
	   bases->unordered_remove (i);
	   vec_free (base_node->refs);
	   ggc_delete (base_node);
	 }
       else
	 i++;
     }
   if (bases && !bases->length ())
     {
       vec_free (bases);
       bases = NULL;
     }
 }

  /* Merge OTHER into the tree.
     PARM_MAP, if non-NULL, maps parm indexes of callee to caller.  -2 is used
     to signalize that parameter is local and does not need to be tracked.
     Return true if something has changed.  */
  bool merge (modref_tree <T> *other, vec <modref_parm_map> *parm_map,
	      bool record_accesses)
  {
    if (!other || every_base)
      return false;
    if (other->every_base)
      {
	collapse ();
	return true;
      }

    bool changed = false;
    size_t i, j, k;
    modref_base_node <T> *base_node, *my_base_node;
    modref_ref_node <T> *ref_node;
    modref_access_node *access_node;
    bool release = false;

    /* For self-recursive functions we may end up merging summary into itself;
       produce copy first so we do not modify summary under our own hands.  */
    if (other == this)
      {
	release = true;
	other = modref_tree<T>::create_ggc (max_bases, max_refs, max_accesses);
	other->copy_from (this);
      }

    FOR_EACH_VEC_SAFE_ELT (other->bases, i, base_node)
      {
	if (base_node->every_ref)
	  {
	    my_base_node = insert_base (base_node->base, 0, &changed);
	    if (my_base_node && !my_base_node->every_ref)
	      {
		my_base_node->collapse ();
		cleanup ();
		changed = true;
	      }
	  }
	else
	  FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	    {
	      if (ref_node->every_access)
		{
		  changed |= insert (base_node->base,
				     ref_node->ref,
				     unspecified_modref_access_node,
				     record_accesses);
		}
	      else
		FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
		  {
		    modref_access_node a = *access_node;

		    if (a.parm_index != -1 && parm_map)
		      {
			if (a.parm_index >= (int)parm_map->length ())
			  a.parm_index = -1;
			else if ((*parm_map) [a.parm_index].parm_index == -2)
			  continue;
			else
			  {
			    a.parm_offset
				 += (*parm_map) [a.parm_index].parm_offset;
			    a.parm_offset_known
				 &= (*parm_map)
					 [a.parm_index].parm_offset_known;
			    a.parm_index
				 = (*parm_map) [a.parm_index].parm_index;
			  }
		      }
		    changed |= insert (base_node->base, ref_node->ref, a,
				       record_accesses);
		  }
	    }
      }
    if (release)
      ggc_delete (other);
    return changed;
  }

  /* Copy OTHER to THIS.  */
  void copy_from (modref_tree <T> *other)
  {
    merge (other, NULL, false);
  }

  /* Search BASE in tree; return NULL if failed.  */
  modref_base_node <T> *search (T base)
  {
    size_t i;
    modref_base_node <T> *n;
    FOR_EACH_VEC_SAFE_ELT (bases, i, n)
      if (n->base == base)
	return n;
    return NULL;
  }

  /* Return true if tree contains access to global memory.  */
  bool global_access_p ()
  {
    size_t i, j, k;
    modref_base_node <T> *base_node;
    modref_ref_node <T> *ref_node;
    modref_access_node *access_node;
    if (every_base)
      return true;
    FOR_EACH_VEC_SAFE_ELT (bases, i, base_node)
      {
	if (base_node->every_ref)
	  return true;
	FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	  {
	    if (ref_node->every_access)
	      return true;
	    FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
	      if (access_node->parm_index < 0)
		return true;
	  }
      }
    return false;
  }

  /* Return ggc allocated instance.  We explicitly call destructors via
     ggc_delete and do not want finalizers to be registered and
     called at the garbage collection time.  */
  static modref_tree<T> *create_ggc (size_t max_bases, size_t max_refs,
				     size_t max_accesses)
  {
    return new (ggc_alloc_no_dtor<modref_tree<T>> ())
	 modref_tree<T> (max_bases, max_refs, max_accesses);
  }

  /* Remove all records and mark tree to alias with everything.  */
  void collapse ()
  {
    size_t i;
    modref_base_node <T> *n;

    if (bases)
      {
	FOR_EACH_VEC_SAFE_ELT (bases, i, n)
	  {
	    n->collapse ();
	    ggc_free (n);
	  }
	vec_free (bases);
      }
    bases = NULL;
    every_base = true;
  }

  /* Release memory.  */
  ~modref_tree ()
  {
    collapse ();
  }

  /* Update parameter indexes in TT according to MAP.  */
  void
  remap_params (vec <int> *map)
  {
    size_t i;
    modref_base_node <T> *base_node;
    FOR_EACH_VEC_SAFE_ELT (bases, i, base_node)
      {
	size_t j;
	modref_ref_node <T> *ref_node;
	FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	  {
	    size_t k;
	    modref_access_node *access_node;
	    FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
	      if (access_node->parm_index > 0)
		{
		  if (access_node->parm_index < (int)map->length ())
		    access_node->parm_index = (*map)[access_node->parm_index];
		  else
		    access_node->parm_index = -1;
		}
	  }
      }
  }
};

void modref_c_tests ();

void gt_ggc_mx (modref_tree <int>* const&);
void gt_ggc_mx (modref_tree <tree_node*>* const&);
void gt_pch_nx (modref_tree <int>* const&);
void gt_pch_nx (modref_tree <tree_node*>* const&);
void gt_pch_nx (modref_tree <int>* const&, gt_pointer_operator op, void *cookie);
void gt_pch_nx (modref_tree <tree_node*>* const&, gt_pointer_operator op,
		void *cookie);

void gt_ggc_mx (modref_base_node <int>*);
void gt_ggc_mx (modref_base_node <tree_node*>* &);
void gt_pch_nx (modref_base_node <int>* const&);
void gt_pch_nx (modref_base_node <tree_node*>* const&);
void gt_pch_nx (modref_base_node <int>* const&, gt_pointer_operator op,
		void *cookie);
void gt_pch_nx (modref_base_node <tree_node*>* const&, gt_pointer_operator op,
		void *cookie);

void gt_ggc_mx (modref_ref_node <int>*);
void gt_ggc_mx (modref_ref_node <tree_node*>* &);
void gt_pch_nx (modref_ref_node <int>* const&);
void gt_pch_nx (modref_ref_node <tree_node*>* const&);
void gt_pch_nx (modref_ref_node <int>* const&, gt_pointer_operator op,
		void *cookie);
void gt_pch_nx (modref_ref_node <tree_node*>* const&, gt_pointer_operator op,
		void *cookie);

#endif
