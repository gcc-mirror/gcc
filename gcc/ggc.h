/* Garbage collection for the GNU compiler.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#include "gansidecl.h"

/* Symbols are marked with `ggc' for `gcc gc' so as not to interfere with
   an external gc library that might be linked in.  */

/* Startup */

extern void init_ggc PROTO ((void));

/* Allocation.  */

struct rtx_def *ggc_alloc_rtx PROTO ((int nslots));
struct rtvec_def *ggc_alloc_rtvec PROTO ((int nelt));
union tree_node *ggc_alloc_tree PROTO ((int length));
char *ggc_alloc_string PROTO ((const char *contents, int length));

/* Invoke the collector.  This is really just a hint, but in the case of
   the simple collector, the only time it will happen.  */

void ggc_collect PROTO ((void));

/* Manipulate global roots that are needed between calls to gc.  */
void ggc_add_root PROTO ((void *base, int nelt, int size,
			   void (*)(void *)));
void ggc_add_rtx_root PROTO ((struct rtx_def **, int nelt));
void ggc_add_tree_root PROTO ((union tree_node **, int nelt));
void ggc_del_root PROTO ((void *base));

/* Mark nodes from the gc_add_root callback.  */
void ggc_mark_rtx PROTO ((struct rtx_def *));
void ggc_mark_rtvec PROTO ((struct rtvec_def *));
void ggc_mark_tree PROTO ((union tree_node *));
void ggc_mark_string PROTO ((char *));

/* Callbacks to the languages.  */

/* This is the language's opportunity to mark nodes held through
   the lang_specific hooks in the tree.  */
void lang_mark_tree PROTO ((union tree_node *));

/* And similarly to free that data when the tree node is released.  */
void lang_cleanup_tree PROTO ((union tree_node *));

/* Mark functions for various structs scattered about.  */

void mark_temp_slot PROTO ((void *));
void mark_function_chain PROTO ((void *));
void mark_eh_state PROTO ((void *));
void mark_stmt_state PROTO ((void *));
void mark_emit_state PROTO ((void *));
void mark_varasm_state PROTO ((void *));
void mark_optab PROTO ((void *));
