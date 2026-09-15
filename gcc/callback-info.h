/* Callback attribute summary
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by Josef Melcr <jmelcr@gcc.gnu.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef CALLBACK_INFO_H
#define CALLBACK_INFO_H

/* Summary aggregating all information relevant to callback edges.  Most of the
   information can be calculated from the attribute as well, but keeping it
   separate allows us to modify the info without touching the underlying
   attribute.  */
class callback_info
{
public:
  /* Index of the callback function in the argument list.  Currently also used
     as an id for the callback attribute of the function (needed if a function
     has multiple callback attributes).  This limits us to a single callback
     attribute per parameter.  If we want to allow multiple attributes per
     parameter, we will need a separate id field.  */
  unsigned fn_idx;

  /* Mapping from the indices of the arguments passed to the callback to their
     indices in the call to the dispatching function or to
     ARG_MAPPING_UNKNOWN_IDX if they do not correspond to any. */
  auto_vec<int> arg_mapping;

  /* Mapping in the opposite direction, from indices of the actual
     arguments of the call to the dispatcher to indices of arguments in
     which they are passed to the callback or to ARG_MAPPING_UNKNOWN_IDX
     if they are not passed to the callback.  */
  auto_vec<int> inverted_mapping;

  /* TRUE iff the associated callback edge was redirected.  */
  bool redirected;

  /* Stream in callback_info.  */
  void stream_in (lto_input_block *ib);

  /* Stream out callback_info.  */
  void stream_out (lto_simple_output_block *ib) const;

  /* Returns the id of the associated callback attribute.  */
  unsigned get_id () const;

  /* Initializes the summary.  */
  void init (cgraph_edge *e, unsigned fn_idx, tree attr);

  /* Create the summary for a callback call CBE of internal function IFN. */
  static callback_info *create_internal_summary (cgraph_edge *cbe,
						 internal_fn ifn);
};

class callback_info_sum_t : public call_summary<callback_info *>
{
public:
  callback_info_sum_t (symbol_table *table, bool ggc)
    : call_summary<callback_info *> (table, ggc)
  {}

  /* Populates the callback_info_sum if it's NULL.  */
  static void check_create_info_sum ();

  /* Frees the callback_info_sum pointer.  */
  static void free_info_sum ();

  /* Duplication function for the cgraph_edge duplication hook.  */
  void duplicate (cgraph_edge *src, cgraph_edge *dst, callback_info *src_s,
		  callback_info *dst_s) override;
};

extern callback_info_sum_t *callback_info_sum;

/* Return the argument of STMT at IDX if CI is null, otherwise use the mapping
   in CI and return the remapped argument.  Presumes that the remapped index is
   not ARG_MAPPING_UNKNOWN_IDX.  */
tree callback_call_arg_summary (const gimple *stmt, int idx, callback_info *ci);

/* Return the number of arguments of STMT if CI is null,
   ci->arg_mapping.length() otherwise.  */
unsigned callback_call_num_args_summary (const gcall *stmt, callback_info *ci);

/* Returns TRUE iff STMT is an internal call statement to IFN_ASSUME.  */
bool callback_ifn_assume_call_p (const gimple *stmt);

/* Returns TRUE iff STMT is an internal call statement to IFN_ASSUME with a
   callback edge pointing to the outlined body.  */
bool callback_ifn_assume_call_with_edge_p (cgraph_node *caller, gimple *stmt);

#endif /* CALLBACK_INFO_H  */
