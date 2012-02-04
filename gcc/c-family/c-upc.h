/* Definitions of UPC front-end entry points used for C and C++.
   that are called from within the C front end.
   respectively.
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef GCC_C_COMMON_UPC_H
#define GCC_C_COMMON_UPC_H


/* UPC entry points.  */

/* The following UPC functions are called by the C front-end;
 * they all must have corresponding stubs in stub-upc.c.  */

extern int count_upc_threads_refs (tree);
extern void deny_pragma_upc (void);
extern int get_upc_consistency_mode (void);
extern int get_upc_pupc_mode(void);
extern int disable_pupc_mode(void);
extern void set_pupc_mode(int);
extern int is_multiple_of_upc_threads (tree);
extern void permit_pragma_upc (void);
extern void pop_upc_consistency_mode (void);
extern int pragma_upc_permitted_p (void);
extern void push_upc_consistency_mode (void);
extern void set_upc_consistency_mode (int);
extern void set_upc_threads_refs_to_one (tree *);
extern tree upc_affinity_test (location_t, tree);
extern tree upc_grok_layout_qualifier (location_t, enum tree_code,
                                       tree, tree, tree);
extern tree upc_blocksizeof (location_t, tree);
extern tree upc_build_pointer_type (tree);
extern tree upc_build_sync_stmt (location_t, tree, tree);
extern int upc_check_decl_init (tree, tree);
extern void upc_check_decl (tree);
extern void upc_cpp_builtins (cpp_reader *);
extern void upc_decl_init (tree, tree);
extern int upc_diagnose_deprecated_stmt (location_t, tree);
extern tree upc_elemsizeof (location_t, tree);
extern tree upc_get_block_factor (const tree);
extern tree upc_instrument_forall (location_t, int);
extern int upc_is_null_pts_p (tree);
extern tree upc_localsizeof (location_t, tree);
extern tree upc_num_threads (void);
extern tree upc_pts_diff (tree, tree);
extern tree upc_pts_increment (location_t, enum tree_code, tree);
extern tree upc_pts_int_sum (location_t, enum tree_code, tree, tree);
extern tree upc_rts_forall_depth_var (void);
extern void upc_set_decl_section (tree);
extern void upc_write_global_declarations (void);

#endif /* ! GCC_C_COMMON_UPC_H */
