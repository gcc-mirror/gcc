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

  /* Mapping from the dispatching function's arguments to the callback
     function.  */
  auto_vec<int> arg_mapping;

  /* TRUE iff the associated callback edge was redirected.  */
  bool redirected;

  /* Stream in callback_info.  */
  void stream_in (lto_input_block *ib);

  /* Stream out callback_info.  */
  void stream_out (lto_simple_output_block *ib) const;

  /* Returns the id of the associated callback attribute.  */
  unsigned get_id () const;

  /* Initializes the summary.  */
  void init (unsigned fn_idx, tree attr);
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

#endif /* CALLBACK_INFO_H  */
