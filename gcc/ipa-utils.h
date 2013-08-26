/* Utilities for ipa analysis.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

#ifndef GCC_IPA_UTILS_H
#define GCC_IPA_UTILS_H
#include "tree.h"
#include "cgraph.h"

struct ipa_dfs_info {
  int dfn_number;
  int low_link;
  /* This field will have the samy value for any two nodes in the same strongly
     connected component.  */
  int scc_no;
  bool new_node;
  bool on_stack;
  struct cgraph_node* next_cycle;
  PTR aux;
};


/* In ipa-utils.c  */
void ipa_print_order (FILE*, const char *, struct cgraph_node**, int);
int ipa_reduced_postorder (struct cgraph_node **, bool, bool,
			  bool (*ignore_edge) (struct cgraph_edge *));
void ipa_free_postorder_info (void);
vec<cgraph_node_ptr> ipa_get_nodes_in_cycle (struct cgraph_node *);
int ipa_reverse_postorder (struct cgraph_node **);
tree get_base_var (tree);

/* In ipa-devirt.c  */

struct odr_type_d;
typedef odr_type_d *odr_type;
void build_type_inheritance_graph (void);
void update_type_inheritance_graph (void);
vec <cgraph_node *>
possible_polymorphic_call_targets (tree, HOST_WIDE_INT,
				   bool *final = NULL,
				   void **cache_token = NULL);
odr_type get_odr_type (tree, bool insert = false);
void dump_possible_polymorphic_call_targets (FILE *, tree, HOST_WIDE_INT);
bool possible_polymorphic_call_target_p (tree, HOST_WIDE_INT,
					 struct cgraph_node *n);
tree method_class_type (tree);

/* Return vector containing possible targets of polymorphic call E.
   If FINALP is non-NULL, store true if the list is complette. 
   CACHE_TOKEN (if non-NULL) will get stored to an unique ID of entry
   in the target cache.  If user needs to visit every target list
   just once, it can memoize them.

   Returned vector is placed into cache.  It is NOT caller's responsibility
   to free it.  The vector can be freed on cgraph_remove_node call if
   the particular node is a virtual function present in the cache.  */

inline vec <cgraph_node *>
possible_polymorphic_call_targets (struct cgraph_edge *e,
				   bool *final = NULL,
				   void **cache_token = NULL)
{
  gcc_checking_assert (e->indirect_info->polymorphic);
  return possible_polymorphic_call_targets (e->indirect_info->otr_type,
					    e->indirect_info->otr_token,
					    final, cache_token);
}

/* Dump possible targets of a polymorphic call E into F.  */

inline void
dump_possible_polymorphic_call_targets (FILE *f, struct cgraph_edge *e)
{
  gcc_checking_assert (e->indirect_info->polymorphic);
  dump_possible_polymorphic_call_targets (f, e->indirect_info->otr_type,
					  e->indirect_info->otr_token);
}

/* Return true if N can be possibly target of a polymorphic call of
   E.  */

inline bool
possible_polymorphic_call_target_p (struct cgraph_edge *e,
				    struct cgraph_node *n)
{
  return possible_polymorphic_call_target_p (e->indirect_info->otr_type,
					     e->indirect_info->otr_token, n);
}
#endif  /* GCC_IPA_UTILS_H  */


