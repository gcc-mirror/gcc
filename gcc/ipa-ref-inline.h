/* IPA reference lists.
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

/* Return callgraph node REF is refering.  */
static inline struct cgraph_node *
ipa_ref_node (struct ipa_ref *ref)
{
  gcc_assert (ref->refered_type == IPA_REF_CGRAPH);
  return ref->refered.cgraph_node;
}

/* Return varpool node REF is refering.  */

static inline struct varpool_node *
ipa_ref_varpool_node (struct ipa_ref *ref)
{
  gcc_assert (ref->refered_type == IPA_REF_VARPOOL);
  return ref->refered.varpool_node;
}

/* Return cgraph node REF is in.  */

static inline struct cgraph_node *
ipa_ref_refering_node (struct ipa_ref *ref)
{
  gcc_assert (ref->refering_type == IPA_REF_CGRAPH);
  return ref->refering.cgraph_node;
}

/* Return varpool node REF is in.  */

static inline struct varpool_node *
ipa_ref_refering_varpool_node (struct ipa_ref *ref)
{
  gcc_assert (ref->refering_type == IPA_REF_VARPOOL);
  return ref->refering.varpool_node;
}

/* Return reference list REF is in.  */

static inline struct ipa_ref_list *
ipa_ref_refering_ref_list (struct ipa_ref *ref)
{
  if (ref->refering_type == IPA_REF_CGRAPH)
    return &ipa_ref_refering_node (ref)->ref_list;
  else
    return &ipa_ref_refering_varpool_node (ref)->ref_list;
}

/* Return reference list REF is in.  */

static inline struct ipa_ref_list *
ipa_ref_refered_ref_list (struct ipa_ref *ref)
{
  if (ref->refered_type == IPA_REF_CGRAPH)
    return &ipa_ref_node (ref)->ref_list;
  else
    return &ipa_ref_varpool_node (ref)->ref_list;
}

/* Return first reference in LIST or NULL if empty.  */

static inline struct ipa_ref *
ipa_ref_list_first_reference (struct ipa_ref_list *list)
{
  if (!VEC_length (ipa_ref_t, list->references))
    return NULL;
  return VEC_index (ipa_ref_t, list->references, 0);
}

/* Return first refering ref in LIST or NULL if empty.  */

static inline struct ipa_ref *
ipa_ref_list_first_refering (struct ipa_ref_list *list)
{
  if (!VEC_length (ipa_ref_ptr, list->refering))
    return NULL;
  return VEC_index (ipa_ref_ptr, list->refering, 0);
}

/* Clear reference list.  */

static inline void
ipa_empty_ref_list (struct ipa_ref_list *list)
{
  list->refering = NULL;
  list->references = NULL;
}

/* Clear reference list.  */

static inline unsigned int
ipa_ref_list_nreferences (struct ipa_ref_list *list)
{
  return VEC_length (ipa_ref_t, list->references);
}

#define ipa_ref_list_reference_iterate(L,I,P) \
   VEC_iterate(ipa_ref_t, (L)->references, (I), (P))
#define ipa_ref_list_refering_iterate(L,I,P) \
   VEC_iterate(ipa_ref_ptr, (L)->refering, (I), (P))
