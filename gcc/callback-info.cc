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
#include "callback-info.h"
#include "attr-callback.h"

callback_info_sum_t *callback_info_sum = NULL;

/* Stream out callback_info.  */
void
callback_info::stream_out (lto_simple_output_block *ob) const
{
  streamer_write_uhwi_stream (ob->main_stream, fn_idx);
  streamer_write_uhwi_stream (ob->main_stream, arg_mapping.length ());

  for (int idx : arg_mapping)
    streamer_write_hwi_stream (ob->main_stream, idx);

  bitpack_d bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, redirected, 1);
  streamer_write_bitpack (&bp);
}

/* Stream in callback_info.  */
void
callback_info::stream_in (lto_input_block *ib)
{
  fn_idx = streamer_read_uhwi (ib);
  unsigned length = streamer_read_uhwi (ib);
  arg_mapping.reserve (length);

  for (unsigned i = 0; i < length; i++)
    {
      int idx = streamer_read_hwi (ib);
      arg_mapping.safe_push (idx);
    }

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
callback_info::init (unsigned fn_idx, tree attr)
{
  this->fn_idx = fn_idx;
  arg_mapping = callback_get_arg_mapping_from_attr (attr);
  redirected = false;
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
  dst_s->redirected = src_s->redirected;
}
