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

#ifndef GCC_MODREF_TREE_H
#define GCC_MODREF_TREE_H

struct ipa_modref_summary;


template <typename T>
struct GTY((user)) modref_ref_node
{
  T ref;

  modref_ref_node (T ref):
    ref (ref)
  {}
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

    if (!ref)
      {
	collapse ();
	return NULL;
      }

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
	  ggc_free (r);
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
  bool every_base;

  modref_tree (size_t max_bases, size_t max_refs):
    bases (NULL),
    max_bases (max_bases),
    max_refs (max_refs),
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
  void insert (T base, T ref)
  {
    modref_base_node <T> *base_node;

    base_node = insert_base (base);

    if (!base && !ref)
      {
	collapse ();
	return;
      }
    if (!base_node)
      return;
    gcc_assert (search (base) != NULL);

    base_node->insert_ref (ref, max_refs);
    if (!base && base_node->every_ref)
      {
	collapse ();
	return;
      }
  }

  /* Merge OTHER into the tree.  */
  void merge (modref_tree <T> *other)
  {
    if (!other)
      return;
    if (other->every_base)
      {
	collapse ();
	return;
      }

    size_t i, j;
    modref_base_node <T> *base_node, *my_base_node;
    modref_ref_node <T> *ref_node, *my_ref_node;
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
	  }
      }
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
  static modref_tree<T> *create_ggc (size_t max_bases, size_t max_refs)
  {
    return new (ggc_alloc_no_dtor<modref_tree<T>> ())
	 modref_tree<T> (max_bases, max_refs);
  }

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
