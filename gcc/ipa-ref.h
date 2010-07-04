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

struct cgraph_node;
struct varpool_node;

/* How the reference is done.  */
enum GTY(()) ipa_ref_use
{
  IPA_REF_LOAD,
  IPA_REF_STORE,
  IPA_REF_ADDR
};

/* Type of refering or refered type.  */
enum GTY(()) ipa_ref_type
{
  IPA_REF_CGRAPH,
  IPA_REF_VARPOOL
};

/* We can have references spanning both callgraph and varpool,
   so all pointers needs to be of both types.  */
union GTY(()) ipa_ref_ptr_u
{
  struct cgraph_node * GTY((tag ("IPA_REF_CGRAPH"))) cgraph_node;
  struct varpool_node * GTY((tag ("IPA_REF_VARPOOL"))) varpool_node;
};

/* Record of reference in callgraph or varpool.  */
struct GTY(()) ipa_ref
{
  union ipa_ref_ptr_u GTY ((desc ("%1.refering_type"))) refering;
  union ipa_ref_ptr_u GTY ((desc ("%1.refered_type"))) refered;
  gimple stmt;
  unsigned int refered_index;
  ENUM_BITFIELD (ipa_ref_type) refering_type:1;
  ENUM_BITFIELD (ipa_ref_type) refered_type:1;
  ENUM_BITFIELD (ipa_ref_use) use:2;
};

typedef struct ipa_ref ipa_ref_t;
typedef struct ipa_ref *ipa_ref_ptr;

DEF_VEC_O(ipa_ref_t);
DEF_VEC_ALLOC_O(ipa_ref_t,gc);
DEF_VEC_P(ipa_ref_ptr);
DEF_VEC_ALLOC_P(ipa_ref_ptr,heap);

/* List of references.  This is stored in both callgraph and varpool nodes.  */
struct GTY(()) ipa_ref_list
{
  /* Store actual references in references vector.  */
  VEC(ipa_ref_t,gc) *references;
  /* Refering is vector of pointers to references.  It must not live in GGC space
     or GGC will try to mark middle of references vectors.  */
  VEC(ipa_ref_ptr,heap) * GTY((skip)) refering;
};

struct ipa_ref * ipa_record_reference (struct cgraph_node *,
				       struct varpool_node *,
				       struct cgraph_node *,
				       struct varpool_node *,
				       enum ipa_ref_use, gimple);

void ipa_remove_reference (struct ipa_ref *);
void ipa_remove_all_references (struct ipa_ref_list *);
void ipa_remove_all_refering (struct ipa_ref_list *);
void ipa_dump_references (FILE *, struct ipa_ref_list *);
void ipa_dump_refering (FILE *, struct ipa_ref_list *);
void ipa_clone_references (struct cgraph_node *, struct varpool_node *, struct ipa_ref_list *);
void ipa_clone_refering (struct cgraph_node *, struct varpool_node *, struct ipa_ref_list *);
bool ipa_ref_cannot_lead_to_return (struct ipa_ref *);

