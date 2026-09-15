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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "builtins.h"
#include "options.h"
#include "gimple-range.h"
#include "attribs.h"
#include "symbol-summary.h"
#include "data-streamer.h"
#include "attr-callback.h"
#include "callback-info.h"

callback_info_sum_t *callback_info_sum = NULL;

/* Calculates the CI->INVERTED_MAPPING field from CI->ARG_MAPPING.  */
static void
invert_arg_mapping (cgraph_edge *e, callback_info *ci)
{
  auto &mapping = ci->arg_mapping;
  auto &inverted_mapping = ci->inverted_mapping;
  unsigned num = gimple_call_num_args (e->call_stmt);
  inverted_mapping.reserve (num);
  for (unsigned i = 0; i < num; i++)
    inverted_mapping.quick_push (ARG_MAPPING_UNKNOWN_IDX);

  for (unsigned i = 0; i < mapping.length (); i++)
    if (mapping[i] != ARG_MAPPING_UNKNOWN_IDX)
      inverted_mapping[mapping[i]] = i;
}

/* Streams a vector into an output block, in format N ELEM_1 ... ELEM_N*/
static void
stream_vector_out (lto_simple_output_block *ob, const auto_vec<int> &v)
{
  streamer_write_uhwi_stream (ob->main_stream, v.length ());
  for (int elem : v)
    streamer_write_hwi_stream (ob->main_stream, elem);
}

/* Reads a vector from an LTO input block.  The format is supposed to be the
   same as stream_vector_out.  */
static void
stream_vector_in (lto_input_block *ib, auto_vec<int> &v)
{
  unsigned length = streamer_read_uhwi (ib);
  v.reserve (length);

  for (unsigned i = 0; i < length; i++)
    {
      int elem = streamer_read_hwi (ib);
      v.safe_push (elem);
    }
}

/* Stream out callback_info.  */
void
callback_info::stream_out (lto_simple_output_block *ob) const
{
  streamer_write_uhwi_stream (ob->main_stream, fn_idx);
  stream_vector_out (ob, arg_mapping);
  stream_vector_out (ob, inverted_mapping);
  bitpack_d bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, redirected, 1);
  streamer_write_bitpack (&bp);
}

/* Stream in callback_info.  */
void
callback_info::stream_in (lto_input_block *ib)
{
  fn_idx = streamer_read_uhwi (ib);
  stream_vector_in (ib, arg_mapping);
  stream_vector_in (ib, inverted_mapping);
  bitpack_d bp = streamer_read_bitpack (ib);
  redirected = bp_unpack_value (&bp, 1);
}

/* Returns the id of the associated callback attribute.  See the comment of the
   fn_idx field.  */
unsigned
callback_info::get_id () const
{
  return fn_idx;
}

void
callback_info::init (cgraph_edge *e, unsigned fn_idx, tree attr)
{
  this->fn_idx = fn_idx;
  arg_mapping = callback_get_arg_mapping_from_attr (attr);
  invert_arg_mapping (e, this);
  redirected = false;
}

/* Initialize the summary for a callback call CBE of internal function IFN.  */
callback_info *
callback_info::create_internal_summary (cgraph_edge *cbe, internal_fn ifn)
{
  callback_info_sum_t::check_create_info_sum ();
  callback_info *ci = callback_info_sum->get_create (cbe);
  switch (ifn)
    {
    case IFN_ASSUME:
      {
	ci->fn_idx = 0;
	unsigned n = gimple_call_num_args (cbe->call_stmt);
	for (unsigned i = 0; i < n - 1; i++)
	  ci->arg_mapping.safe_push (i + 1);
	invert_arg_mapping (cbe, ci);
	break;
      }
    default:
      gcc_unreachable ();
    }
  ci->redirected = false;
  return ci;
}

/* Populates the callback_info_sum if it's NULL.  */
void
callback_info_sum_t::check_create_info_sum (void)
{
  if (!callback_info_sum)
    callback_info_sum = new callback_info_sum_t (symtab, false);
}

/* Frees the callback_info_sum pointer.  */
void
callback_info_sum_t::free_info_sum (void)
{
  if (callback_info_sum)
    delete callback_info_sum;
  callback_info_sum = NULL;
}

/* Duplication function for the cgraph_edge duplication hook.  */
void
callback_info_sum_t::duplicate (cgraph_edge *, cgraph_edge *,
				callback_info *src_s, callback_info *dst_s)
{
  dst_s->fn_idx = src_s->fn_idx;
  /* Might need to be adjusted in the future, if argument modifications are
     implemented.  */
  dst_s->arg_mapping = src_s->arg_mapping.copy ();
  dst_s->inverted_mapping = src_s->inverted_mapping.copy ();
  dst_s->redirected = src_s->redirected;
}

/* Return the argument at IDX if CI is null, otherwise use the mapping in CI and
   return the remapped argument.  Presumes that the remapped index is not
   ARG_MAPPING_UNKNOWN_IDX.  */
tree
callback_call_arg_summary (const gimple *stmt, int idx, callback_info *ci)
{
  if (ci)
    idx = ci->arg_mapping[idx];
  gcc_checking_assert (idx != ARG_MAPPING_UNKNOWN_IDX);
  return gimple_call_arg (stmt, idx);
}

/* Return the number of arguments of STMT if CI is null,
   ci->arg_mapping.length() otherwise.  */
unsigned
callback_call_num_args_summary (const gcall *stmt, callback_info *ci)
{
  if (ci)
    return ci->arg_mapping.length ();
  return gimple_call_num_args (stmt);
}

/* Returns TRUE iff STMT is an internal call statement to IFN_ASSUME.  */
bool
callback_ifn_assume_call_p (const gimple *stmt)
{
  return is_gimple_call (stmt) && gimple_call_internal_p (stmt)
	 && (gimple_call_internal_fn (stmt) == IFN_ASSUME);
}

/* Returns TRUE iff STMT is an internal call statement to IFN_ASSUME with a
   callback edge pointing to the outlined body.  */
bool
callback_ifn_assume_call_with_edge_p (cgraph_node *caller, gimple *stmt)
{
  /* Fast path for most cases.  */
  if (!callback_ifn_assume_call_p (stmt))
    return false;

  cgraph_edge *cbei = caller->get_edge (stmt);

  return cbei != nullptr;
}
