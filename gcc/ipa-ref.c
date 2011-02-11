/* Interprocedural reference lists.
   Copyright (C) 2010
   Free Software Foundation, Inc.
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
#include "tree.h"
#include "ggc.h"
#include "target.h"
#include "cgraph.h"

static const char *ipa_ref_use_name[] = {"read","write","addr"};

/* Return ipa reference from REFERING_NODE or REFERING_VARPOOL_NODE
   to REFERED_NODE or REFERED_VARPOOL_NODE. USE_TYPE specify type
   of the use and STMT the statement (if it exists).  */

struct ipa_ref *
ipa_record_reference (struct cgraph_node *refering_node,
		      struct varpool_node *refering_varpool_node,
		      struct cgraph_node *refered_node,
		      struct varpool_node *refered_varpool_node,
		      enum ipa_ref_use use_type, gimple stmt)
{
  struct ipa_ref *ref;
  struct ipa_ref_list *list, *list2;
  VEC(ipa_ref_t,gc) *old_references;
  gcc_assert ((!refering_node) ^ (!refering_varpool_node));
  gcc_assert ((!refered_node) ^ (!refered_varpool_node));
  gcc_assert (!stmt || refering_node);

  list = (refering_node ? &refering_node->ref_list
	  : &refering_varpool_node->ref_list);
  old_references = list->references;
  VEC_safe_grow (ipa_ref_t, gc, list->references,
		 VEC_length (ipa_ref_t, list->references) + 1);
  ref = VEC_last (ipa_ref_t, list->references);

  list2 = (refered_node ? &refered_node->ref_list
	   : &refered_varpool_node->ref_list);
  VEC_safe_push (ipa_ref_ptr, heap, list2->refering, ref);
  ref->refered_index = VEC_length (ipa_ref_ptr, list2->refering) - 1;
  if (refering_node)
    {
      ref->refering.cgraph_node = refering_node;
      ref->refering_type = IPA_REF_CGRAPH;
    }
  else
    {
      ref->refering.varpool_node = refering_varpool_node;
      ref->refering_type = IPA_REF_VARPOOL;
      gcc_assert (use_type == IPA_REF_ADDR);
    }
  if (refered_node)
    {
      ref->refered.cgraph_node = refered_node;
      ref->refered_type = IPA_REF_CGRAPH;
      gcc_assert (use_type == IPA_REF_ADDR);
    }
  else
    {
      varpool_mark_needed_node (refered_varpool_node);
      ref->refered.varpool_node = refered_varpool_node;
      ref->refered_type = IPA_REF_VARPOOL;
    }
  ref->stmt = stmt;
  ref->use = use_type;

  /* If vector was moved in memory, update pointers.  */
  if (old_references != list->references)
    {
      int i;
      for (i = 0; ipa_ref_list_reference_iterate (list, i, ref); i++)
	VEC_replace (ipa_ref_ptr,
		     ipa_ref_refered_ref_list (ref)->refering,
		     ref->refered_index, ref);
    }
  return ref;
}

/* Remove reference REF.  */

void
ipa_remove_reference (struct ipa_ref *ref)
{
  struct ipa_ref_list *list = ipa_ref_refered_ref_list (ref);
  struct ipa_ref_list *list2 = ipa_ref_refering_ref_list (ref);
  VEC(ipa_ref_t,gc) *old_references = list2->references;
  struct ipa_ref *last;

  gcc_assert (VEC_index (ipa_ref_ptr, list->refering, ref->refered_index) == ref);
  last = VEC_last (ipa_ref_ptr, list->refering);
  if (ref != last)
    {
      VEC_replace (ipa_ref_ptr, list->refering,
		   ref->refered_index,
		   VEC_last (ipa_ref_ptr, list->refering));
      VEC_index (ipa_ref_ptr, list->refering,
		 ref->refered_index)->refered_index = ref->refered_index;
    }
  VEC_pop (ipa_ref_ptr, list->refering);

  last = VEC_last (ipa_ref_t, list2->references);
  if (ref != last)
    {
      *ref = *last;
      VEC_replace (ipa_ref_ptr,
		   ipa_ref_refered_ref_list (ref)->refering,
		   ref->refered_index, ref);
    }
  VEC_pop (ipa_ref_t, list2->references);
  gcc_assert (list2->references == old_references);
}

/* Remove all references in ref list LIST.  */

void
ipa_remove_all_references (struct ipa_ref_list *list)
{
  while (VEC_length (ipa_ref_t, list->references))
    ipa_remove_reference (VEC_last (ipa_ref_t, list->references));
  VEC_free (ipa_ref_t, gc, list->references);
  list->references = NULL;
}

/* Remove all references in ref list LIST.  */

void
ipa_remove_all_refering (struct ipa_ref_list *list)
{
  while (VEC_length (ipa_ref_ptr, list->refering))
    ipa_remove_reference (VEC_last (ipa_ref_ptr, list->refering));
  VEC_free (ipa_ref_ptr, heap, list->refering);
  list->refering = NULL;
}

/* Dump references in LIST to FILE.  */

void
ipa_dump_references (FILE * file, struct ipa_ref_list *list)
{
  struct ipa_ref *ref;
  int i;
  for (i = 0; ipa_ref_list_reference_iterate (list, i, ref); i++)
    {
      if (ref->refered_type == IPA_REF_CGRAPH)
	{
	  fprintf (file, " fn:%s/%i (%s)", cgraph_node_name (ipa_ref_node (ref)),
		   ipa_ref_node (ref)->uid,
		   ipa_ref_use_name [ref->use]);
	}
      else
	fprintf (file, " var:%s (%s)",
		 varpool_node_name (ipa_ref_varpool_node (ref)),
		 ipa_ref_use_name [ref->use]);
    }
  fprintf (file, "\n");
}

/* Dump refering in LIST to FILE.  */

void
ipa_dump_refering (FILE * file, struct ipa_ref_list *list)
{
  struct ipa_ref *ref;
  int i;
  for (i = 0; ipa_ref_list_refering_iterate (list, i, ref); i++)
    {
      if (ref->refering_type == IPA_REF_CGRAPH)
	fprintf (file, " fn:%s/%i (%s)",
		 cgraph_node_name (ipa_ref_refering_node (ref)),
		 ipa_ref_refering_node (ref)->uid,
		 ipa_ref_use_name [ref->use]);
      else
	fprintf (file, " var:%s (%s)",
		 varpool_node_name (ipa_ref_refering_varpool_node (ref)),
		 ipa_ref_use_name [ref->use]);
    }
  fprintf (file, "\n");
}

/* Clone all references from SRC to DEST_NODE or DEST_VARPOOL_NODE.  */

void
ipa_clone_references (struct cgraph_node *dest_node,
		      struct varpool_node *dest_varpool_node,
		      struct ipa_ref_list *src)
{
  struct ipa_ref *ref;
  int i;
  for (i = 0; ipa_ref_list_reference_iterate (src, i, ref); i++)
    ipa_record_reference (dest_node, dest_varpool_node,
			  ref->refered_type == IPA_REF_CGRAPH
			  ? ipa_ref_node (ref) : NULL,
			  ref->refered_type == IPA_REF_VARPOOL
			  ? ipa_ref_varpool_node (ref) : NULL,
			  ref->use, ref->stmt);
}

/* Clone all refering from SRC to DEST_NODE or DEST_VARPOOL_NODE.  */

void
ipa_clone_refering (struct cgraph_node *dest_node,
		    struct varpool_node *dest_varpool_node,
		    struct ipa_ref_list *src)
{
  struct ipa_ref *ref;
  int i;
  for (i = 0; ipa_ref_list_refering_iterate (src, i, ref); i++)
    ipa_record_reference (
			  ref->refering_type == IPA_REF_CGRAPH
			  ? ipa_ref_refering_node (ref) : NULL,
			  ref->refering_type == IPA_REF_VARPOOL
			  ? ipa_ref_refering_varpool_node (ref) : NULL,
			  dest_node, dest_varpool_node,
			  ref->use, ref->stmt);
}

/* Return true when execution of REF can load to return from
   function. */
bool
ipa_ref_cannot_lead_to_return (struct ipa_ref *ref)
{
  return cgraph_node_cannot_return (ipa_ref_refering_node (ref));
}
