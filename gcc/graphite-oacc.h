/* Functions for analyzing the OpenACC loop structure from Graphite.

   Copyright (C) 2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_GRAPHITE_OACC_H
#define GCC_GRAPHITE_OACC_H

#include "stringpool.h"
#include "omp-general.h"
#include "attribs.h"
#include "cfgloop.h"
#include "tree-pretty-print.h"
#include "print-tree.h"

static inline bool oacc_function_p (function *fun)
{
  return oacc_get_fn_attrib (fun->decl);
}

extern bool is_oacc_private (tree var, loop_p loop);
extern void oacc_add_private_var_kills (loop_p loop, vec<tree> *kills);

extern const gcall* find_oacc_head_mark (loop_p loop, bool last = false);

extern void collect_oacc_reduction_vars (loop_p loop, hash_set<tree> &vars);
extern void collect_oacc_firstprivate_vars (loop_p loop, hash_set<tree> &vars);
extern void collect_oacc_private_scalars (loop_p loop, hash_set<tree> &vars);
extern void collect_oacc_privatized_vars (gcall *marker, hash_set<tree> &vars);

extern gcall* get_oacc_firstprivate_call (loop_p loop);
extern gcall* get_oacc_private_scalars_call (loop_p loop);

extern bool graphite_analyze_oacc_function_p (function *fun);
extern bool graphite_analyze_oacc_target_region_type_p (function *fun);

extern gcall* get_oacc_firstprivate_call (loop_p loop);
extern gcall* get_oacc_private_scalars_call (loop_p loop);

#endif /* GCC_GRAPHITE_OACC_H */
