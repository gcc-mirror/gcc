/* rtegraph.h runtime exception graph header.

Copyright (C) 2019-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef RTEGRAPH_H
#define RTEGRAPH_H

struct rtenode;

extern rtenode *rtegraph_init_rtenode (gimple *g, tree fndecl, bool is_func_call);
extern rtenode *rtegraph_lookup (gimple *g, tree fndecl, bool is_call);
extern void rtegraph_candidates_include (rtenode *n);
extern void rtegraph_allnodes_include (rtenode *n);
extern void rtegraph_externs_include (rtenode *n);
extern void rtegraph_constructors_include (rtenode *n);
extern void rtegraph_include_rtscall (rtenode *func);
extern void rtegraph_include_function_call (rtenode *func);
extern void rtegraph_set_current_function (rtenode *func);
extern tree rtegraph_get_func (rtenode *func);

extern void rtegraph_discover (void);
extern void rtegraph_init (void);
extern void rtegraph_finish (void);

#endif  /* RTEGRAPH_H.  */
