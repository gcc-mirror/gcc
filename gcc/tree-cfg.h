/* Data and Control Flow Analysis for Trees.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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

#ifndef _TREE_CFG_H
#define _TREE_CFG_H

/* Location to track pending stmt for edge insertion.  */
#define PENDING_STMT(e)	((e)->insns.g)

extern void init_empty_tree_cfg_for_function (struct function *);
extern void init_empty_tree_cfg (void);
extern void fold_cond_expr_cond (void);
extern void start_recording_case_labels (void);
extern void end_recording_case_labels (void);
extern basic_block label_to_block_fn (struct function *, tree);
#define label_to_block(t) (label_to_block_fn (cfun, t))
extern void make_abnormal_goto_edges (basic_block, bool);
extern void cleanup_dead_labels (void);
extern void group_case_labels_stmt (gimple);
extern void group_case_labels (void);
extern void replace_uses_by (tree, tree);
extern basic_block single_noncomplex_succ (basic_block bb);
extern void notice_special_calls (gimple);
extern void clear_special_calls (void);
extern edge find_taken_edge (basic_block, tree);
extern void gimple_debug_bb (basic_block);
extern basic_block gimple_debug_bb_n (int);
extern void gimple_debug_cfg (int);
extern void gimple_dump_cfg (FILE *, int);
extern void dump_cfg_stats (FILE *);
extern void debug_cfg_stats (void);
extern bool stmt_can_make_abnormal_goto (gimple);
extern bool is_ctrl_stmt (gimple);
extern bool is_ctrl_altering_stmt (gimple);
extern bool simple_goto_p (gimple);
extern bool stmt_ends_bb_p (gimple);
extern bool assert_unreachable_fallthru_edge_p (edge);
extern void delete_tree_cfg_annotations (void);
extern gimple first_stmt (basic_block);
extern gimple last_stmt (basic_block);
extern gimple last_and_only_stmt (basic_block);
extern void verify_gimple_in_seq (gimple_seq);
extern void verify_gimple_in_cfg (struct function *);
extern tree gimple_block_label (basic_block);
extern void add_phi_args_after_copy_bb (basic_block);
extern void add_phi_args_after_copy (basic_block *, unsigned, edge);
extern bool gimple_duplicate_sese_region (edge, edge, basic_block *, unsigned,
					basic_block *, bool);
extern bool gimple_duplicate_sese_tail (edge, edge, basic_block *, unsigned,
				      basic_block *);
extern void gather_blocks_in_sese_region (basic_block entry, basic_block exit,
					  vec<basic_block> *bbs_p);
extern basic_block move_sese_region_to_fn (struct function *, basic_block,
				           basic_block, tree);
extern void dump_function_to_file (tree, FILE *, int);
extern void debug_function (tree, int) ;
extern void print_loops_bb (FILE *, basic_block, int, int);
extern void print_loops (FILE *, int);
extern void debug (struct loop &ref);
extern void debug (struct loop *ptr);
extern void debug_verbose (struct loop &ref);
extern void debug_verbose (struct loop *ptr);
extern void debug_loops (int);
extern void debug_loop (struct loop *, int);
extern void debug_loop_num (unsigned, int);
extern void remove_edge_and_dominated_blocks (edge);
extern bool gimple_purge_dead_eh_edges (basic_block);
extern bool gimple_purge_all_dead_eh_edges (const_bitmap);
extern bool gimple_purge_dead_abnormal_call_edges (basic_block);
extern bool gimple_purge_all_dead_abnormal_call_edges (const_bitmap);
extern tree gimplify_build3 (gimple_stmt_iterator *, enum tree_code,
			     tree, tree, tree, tree);
extern tree gimplify_build2 (gimple_stmt_iterator *, enum tree_code,
			     tree, tree, tree);
extern tree gimplify_build1 (gimple_stmt_iterator *, enum tree_code,
			     tree, tree);
extern void extract_true_false_edges_from_block (basic_block, edge *, edge *);
extern unsigned int execute_fixup_cfg (void);

#endif /* _TREE_CFG_H  */
