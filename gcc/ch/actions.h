/* Declarations for ch-actions.c.
   Copyright (C) 1992, 93, 1994 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* used by compile_file */

void init_chill PARAMS ((void));

extern int grant_count;

extern void push_handler PARAMS ((void));
extern void pop_handler PARAMS ((int));
extern void push_action PARAMS ((void));

extern int  chill_handle_single_dimension_case_label PARAMS ((tree, tree, int *, int *));
extern tree build_chill_multi_dimension_case_expr    PARAMS ((tree, tree, tree));
extern tree build_multi_case_selector_expression     PARAMS ((tree, tree));
extern void compute_else_ranges                      PARAMS ((tree, tree));
