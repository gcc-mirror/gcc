/* Data structure for the modref pass.
   Copyright (C) 2020 Free Software Foundation, Inc.
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

   1) Base: this level represent base alias set of the acecess and refers
      to sons (ref nodes). Flag all_refs means that all possible references
      are aliasing.

      Because for LTO streaming we need to stream types rahter than alias sets
      modref_base_node is implemented as a template.
   2) Ref: this level represent ref alias set and links to acesses unless
      all_refs flag is et.
      Again ref is an template to allow LTO streaming.
   3) Access: this level represent info about individual accesses.  Presently
      we record whether access is trhough a dereference of a function parameter
*/

#ifndef GCC_MODREF_TREE_H
#define GCC_MODREF_TREE_H

struct ipa_modref_summary;

/* Memory access.  */
struct GTY(()) modref_access_node
{
  /* Index of parameter which specifies the base of access. -1 if base is not
     a function parameter.  */
  int parm_index;

  /* Return true if access node holds no useful info.  */
  bool useful_p ()
    {
      return parm_index != -1;
    }
};

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
      if (a->parm_index == access.parm_index)
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
     Collapse tree if it has more than MAX_ACCESSES entries.  */
  void insert_access (modref_access_node a, size_t max_accesses)
  {
    /* If this base->ref pair has no access information, bail out.  */
    if (every_access)
      return;

    /* Otherwise, insert a node for the ref of the access under the base.  */
    modref_access_node *access_node = search (a);
    if (access_node)
      return;

    /* If this base->ref pair has too many accesses stored, we will clear
       all accesses and bail out.  */
    if ((accesses && accesses->length () >= max_accesses)
	|| !a.useful_p ())
      {
	if (dump_file && a.useful_p ())
	  fprintf (dump_file,
		   "--param param=modref-max-accesses limit reached\n");
	collapse ();
	return;
      }
    vec_safe_push (accesses, a);
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

  /* Insert REF; collapse tree if there are more than MAX_REFS.  */
  modref_ref_node <T> *insert_ref (T ref, size_t max_refs)
  {
    modref_ref_node <T> *ref_node;

    /* If the node is collapsed, don't do anything.  */
    if (every_ref)
      return NULL;

    /* Otherwise, insert a node for the ref of the access under the base.  */
    ref_node = search (ref);
    if (ref_node)
      return ref_node;

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

  modref_base_node <T> *insert_base (T base)
  {
    modref_base_node <T> *base_node;

    /* If the node is collapsed, don't do anything.  */
    if (every_base)
      return NULL;

    /* Otherwise, insert a node for the base of the access into the tree.  */
    base_node = search (base);
    if (base_node)
      return base_node;

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

  /* Insert memory access to the tree.	*/
  void insert (T base, T ref, modref_access_node a)
  {
    /* No useful information tracked; collapse everything.  */
    if (!base && !ref && !a.useful_p ())
      {
	collapse ();
	return;
      }

    modref_base_node <T> *base_node = insert_base (base);
    if (!base_node)
      return;
    gcc_assert (search (base) != NULL);

    modref_ref_node <T> *ref_node = base_node->insert_ref (ref, max_refs);

    /* No useful ref information and no useful base; collapse everyting.  */
    if (!base && base_node->every_ref)
      {
	collapse ();
	return;
      }
    if (ref_node)
      {
	/* No useful ref and access; collapse ref.  */
	if (!ref && !a.useful_p ())
	  ref_node->collapse ();
	else
	  {
	    ref_node->insert_access (a, max_accesses);
	    /* If ref has collapses and there is no useful base; collapse
	       everything.  */
	    if (!base && !ref && ref_node->every_access)
	      collapse ();
	  }
      }
  }

 /* Remove tree branches that are not useful (i.e. they will allways pass).  */

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
     to signalize that parameter is local and does not need to be tracked.  */
  void merge (modref_tree <T> *other, vec <int> *parm_map)
  {
    if (!other)
      return;
    if (other->every_base)
      {
	collapse ();
	return;
      }

    size_t i, j, k;
    modref_base_node <T> *base_node, *my_base_node;
    modref_ref_node <T> *ref_node, *my_ref_node;
    modref_access_node *access_node;
    FOR_EACH_VEC_SAFE_ELT (other->bases, i, base_node)
      {
	my_base_node = insert_base (base_node->base);
	if (!my_base_node)
	  continue;

	if (base_node->every_ref)
	  {
	    my_base_node->collapse ();
	    continue;
	  }

	FOR_EACH_VEC_SAFE_ELT (base_node->refs, j, ref_node)
	  {
	    my_ref_node = my_base_node->insert_ref (ref_node->ref, max_refs);
	    if (!my_ref_node)
	      continue;

	    if (ref_node->every_access)
	      {
		my_ref_node->collapse ();
		continue;
	      }
	    FOR_EACH_VEC_SAFE_ELT (ref_node->accesses, k, access_node)
	      {
		modref_access_node a = *access_node;
		if (a.parm_index != -1 && parm_map)
		  {
		    if (a.parm_index >= (int)parm_map->length ())
		      a.parm_index = -1;
		    else if ((*parm_map) [a.parm_index] == -2)
		      continue;
		    else
		      a.parm_index = (*parm_map) [a.parm_index];
		  }
		my_ref_node->insert_access (a, max_accesses);
	      }
	  }
      }
    if (parm_map)
      cleanup ();
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
