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

  /* Return true if access node holds no useful info.  */
  bool useful_p () const
    {
      return parm_index != -1;
    }
  /* Return true if range info is useful.  */
  bool range_info_useful_p () const
    {
      return parm_index != -1 && parm_offset_known;
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
      if (range_info_useful_p ()
	  && (!known_eq (a.offset, offset)
	      || !known_eq (a.size, size)
	      || !known_eq (a.max_size, max_size)))
	return false;
      return true;
    }
};

/* Access node specifying no useful info.  */
const modref_access_node unspecified_modref_access_node
		 = {0, -1, -1, 0, -1, false};

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

  /* Search REF; return NULL if failed.  */
  modref_access_node *search (modref_access_node access)
  {
    size_t i;
    modref_access_node *a;
    FOR_EACH_VEC_SAFE_ELT (accesses, i, a)
      if (*a == access)
	return a;
    return NULL;
  }

  /* Collapse the tree.  */
  void collapse ()
  {
    vec_free (accesses);
    accesses = NULL;
    every_access = true;
  }

  /* Insert access with OFFSET and SIZE.
     Collapse tree if it has more than MAX_ACCESSES entries.
     Return true if record was changed.  */
  bool insert_access (modref_access_node a, size_t max_accesses)
  {
    /* If this base->ref pair has no access information, bail out.  */
    if (every_access)
      return false;

    /* Otherwise, insert a node for the ref of the access under the base.  */
    modref_access_node *access_node = search (a);
    if (access_node)
      return false;

    /* If this base->ref pair has too many accesses stored, we will clear
       all accesses and bail out.  */
    if ((accesses && accesses->length () >= max_accesses)
	|| !a.useful_p ())
      {
	if (dump_file && a.useful_p ())
	  fprintf (dump_file,
		   "--param param=modref-max-accesses limit reached\n");
	collapse ();
	return true;
      }
    vec_safe_push (accesses, a);
    return true;
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

    if (changed)
      *changed = true;

    /* Collapse the node if too full already.  */
    if (refs && refs->length () >= max_refs)
      {
	if (dump_file)
	  fprintf (dump_file, "--param param=modref-max-refs limit reached\n");
	collapse ();
	return NULL;
      }

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
     something changed.  */

  modref_base_node <T> *insert_base (T base, bool *changed = NULL)
  {
    modref_base_node <T> *base_node;

    /* If the node is collapsed, don't do anything.  */
    if (every_base)
      return NULL;

    /* Otherwise, insert a node for the base of the access into the tree.  */
    base_node = search (base);
    if (base_node)
      return base_node;

    if (changed)
      *changed = true;

    /* Collapse the node if too full already.  */
    if (bases && bases->length () >= max_bases)
      {
	if (dump_file)
	  fprintf (dump_file, "--param param=modref-max-bases limit reached\n");
	collapse ();
	return NULL;
      }

    base_node = new (ggc_alloc <modref_base_node <T> > ())
			 modref_base_node <T> (base);
    vec_safe_push (bases, base_node);
    return base_node;
  }

  /* Insert memory access to the tree.
     Return true if something changed.  */
  bool insert (T base, T ref, modref_access_node a)
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

    modref_base_node <T> *base_node = insert_base (base, &changed);
    if (!base_node || base_node->every_ref)
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

    /* If we failed to insert ref, just see if there is a cleanup possible.  */
    if (!ref_node)
      {
	/* No useful ref information and no useful base; collapse everything.  */
	if (!base && base_node->every_ref)
	  {
	    collapse ();
	    gcc_checking_assert (changed);
	  }
	else if (changed)
	  cleanup ();
      }
    else
      {
	if (ref_node->every_access)
	  return changed;
	changed |= ref_node->insert_access (a, max_accesses);
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
  bool merge (modref_tree <T> *other, vec <modref_parm_map> *parm_map)
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
	    my_base_node = insert_base (base_node->base, &changed);
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
				     unspecified_modref_access_node);
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
		    changed |= insert (base_node->base, ref_node->ref, a);
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
    merge (other, NULL);
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
