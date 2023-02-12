// Function-related RTL SSA classes                                 -*- C++ -*-
// Copyright (C) 2020-2023 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

namespace rtl_ssa {

// SSA-related information about a function.  It contains three levels
// of information, each in reverse postorder:
//
// - a list of extended basic blocks
// - a list of basic blocks
// - a list of instructions
//
// It also maintains a list of definitions of memory, and a list of
// definitions of each register.
//
// See doc/rtl.texi for more details about the way this information
// is organized and how changes to it are made.
class function_info
{
  // The default obstack alignment takes long double into account.
  // Since we have no use for that here, and since we allocate many
  // relatively small objects, it's better to specify an alignment
  // explicitly.  The allocation routines assert that the alignment
  // is enough for the objects being allocated.
  //
  // Because various structures use pointer_mux, we need at least 2 bytes
  // of alignment.
  static const size_t obstack_alignment = sizeof (void *);

public:
  // Construct SSA form for function FN.
  function_info (function *fn);
  ~function_info ();

  // Return a list of all the extended basic blocks in the function, in reverse
  // postorder.  The list includes the entry and exit blocks.
  iterator_range<ebb_iterator> ebbs () const;

  // Like ebbs (), but in the reverse order.
  iterator_range<reverse_ebb_iterator> reverse_ebbs () const;

  // Return a list of all the basic blocks in the function, in reverse
  // postorder.  The list includes the entry and exit blocks.
  iterator_range<bb_iterator> bbs () const;

  // Like bbs (), but in the reverse order.
  iterator_range<reverse_bb_iterator> reverse_bbs () const;

  // Return the SSA information for the basic block with index INDEX.
  bb_info *bb (unsigned int index) const { return m_bbs[index]; }

  // Return the SSA information for CFG_BB.
  bb_info *bb (basic_block cfg_bb) const { return m_bbs[cfg_bb->index]; }

  // Return a list of all the instructions in the function, in reverse
  // postorder.  The list includes both real and artificial instructions.
  //
  // Iterations over the list will pick up any new instructions that are
  // inserted after the iterator's current instruction.
  iterator_range<any_insn_iterator> all_insns () const;

  // Like all_insns (), but in the reverse order.
  //
  // Iterations over the list will pick up any new instructions that are
  // inserted before the iterator's current instruction.
  iterator_range<reverse_any_insn_iterator> reverse_all_insns () const;

  // Like all_insns (), but without the debug instructions.
  iterator_range<nondebug_insn_iterator> nondebug_insns () const;

  // Like reverse_all_insns (), but without the debug instructions.
  iterator_range<reverse_nondebug_insn_iterator>
    reverse_nondebug_insns () const;

  // Return the first and last instructions in insns ().
  insn_info *first_insn () const { return m_first_insn; }
  insn_info *last_insn () const { return m_last_insn; }

  // Return a list of all definitions of memory, in reverse postorder.
  // This includes both real stores by instructions and artificial
  // definitions by things like phi nodes.
  iterator_range<def_iterator> mem_defs () const;

  // Return a list of all definitions of register REGNO, in reverse postorder.
  // This includes both real stores by instructions and artificial
  // definitions by things like phi nodes.
  iterator_range<def_iterator> reg_defs (unsigned int regno) const;

  // Check if all uses of register REGNO are either unconditionally undefined
  // or use the same single dominating definition.  Return the definition
  // if so, otherwise return null.
  set_info *single_dominating_def (unsigned int regno) const;

  // Look for a definition of RESOURCE at INSN.  Return the result of the
  // search as a def_lookup; see the comments there for more details.
  def_lookup find_def (resource_info resource, insn_info *insn);

  // Return an RAII object that owns all temporary RTL SSA memory
  // allocated during a change attempt.  The object should remain in
  // scope until the change has been aborted or successfully completed.
  obstack_watermark new_change_attempt () { return &m_temp_obstack; }

  // Make a best attempt to check whether the values used by USES are
  // available on entry to BB, without solving a full dataflow problem.
  // If all the values are already live on entry to BB or can be made
  // available there, return a use_array that describes the uses as
  // if they occured at the start of BB.  These uses are purely temporary,
  // and will not become permanent unless applied using change_insns.
  //
  // If the operation fails, return an invalid use_array.
  //
  // WATERMARK is a watermark returned by new_change_attempt ().
  // WILL_BE_DEBUG_USES is true if the returned use_array will be
  // used only for debug instructions.
  use_array make_uses_available (obstack_watermark &watermark,
				 use_array uses, bb_info *bb,
				 bool will_be_debug_uses);

  // If CHANGE doesn't already clobber REGNO, try to add such a clobber,
  // limiting the movement range in order to make the clobber valid.
  // When determining whether REGNO is live, ignore accesses made by an
  // instruction I if IGNORE (I) is true.  The caller then assumes the
  // responsibility of ensuring that CHANGE and I are placed in a valid order.
  //
  // Return true on success.  Leave CHANGE unmodified when returning false.
  //
  // WATERMARK is a watermark returned by new_change_attempt ().
  template<typename IgnorePredicate>
  bool add_regno_clobber (obstack_watermark &watermark, insn_change &change,
			  unsigned int regno, IgnorePredicate ignore);

  // Return true if change_insns will be able to perform the changes
  // described by CHANGES.
  bool verify_insn_changes (array_slice<insn_change *const> changes);

  // Perform all the changes in CHANGES, keeping the instructions in the
  // order specified by the CHANGES array.  On return, the SSA information
  // remains up-to-date.  The same is true for instruction-level DF
  // information, although the block-level DF information might be
  // marked dirty.
  void change_insns (array_slice<insn_change *> changes);

  // Like change_insns, but for a single change CHANGE.
  void change_insn (insn_change &change);

  // If the changes that have been made to instructions require updates
  // to the CFG, perform those updates now.  Return true if something changed.
  // If it did:
  //
  // - The SSA information is now invalid and needs to be recomputed.
  //
  // - Dominance information is no longer available (in either direction).
  //
  // - The caller will need to call cleanup_cfg at some point.
  //
  // ??? We could probably update the SSA information for simple updates,
  // but currently nothing would benefit.  These late CFG changes are
  // relatively rare anyway, since gimple optimisers should remove most
  // unnecessary control flow.
  bool perform_pending_updates ();

  // Print the contents of the function to PP.
  void print (pretty_printer *pp) const;

private:
  class bb_phi_info;
  class build_info;
  class bb_walker;

  // Return an RAII object that owns all objects allocated by
  // allocate_temp during its lifetime.
  obstack_watermark temp_watermark () { return &m_temp_obstack; }

  template<typename T, typename... Ts>
  T *allocate (Ts... args);

  template<typename T, typename... Ts>
  T *allocate_temp (Ts... args);

  access_array temp_access_array (access_array accesses);

  clobber_group *need_clobber_group (clobber_info *);
  def_node *need_def_node (def_info *);
  def_splay_tree need_def_splay_tree (def_info *);

  use_info *make_use_available (use_info *, bb_info *, bool);
  def_array insert_temp_clobber (obstack_watermark &, insn_info *,
				 unsigned int, def_array);

  void insert_def_before (def_info *, def_info *);
  void insert_def_after (def_info *, def_info *);
  void remove_def_from_list (def_info *);

  void add_clobber (clobber_info *, clobber_group *);
  void remove_clobber (clobber_info *, clobber_group *);
  void prepend_clobber_to_group (clobber_info *, clobber_group *);
  void append_clobber_to_group (clobber_info *, clobber_group *);
  void merge_clobber_groups (clobber_info *, clobber_info *,
			     def_info *);
  clobber_info *split_clobber_group (clobber_group *, insn_info *);

  void append_def (def_info *);
  void add_def (def_info *);
  void remove_def (def_info *);

  void need_use_splay_tree (set_info *);

  static void insert_use_before (use_info *, use_info *);
  static void insert_use_after (use_info *, use_info *);

  void add_use (use_info *);
  void remove_use (use_info *);

  insn_info::order_node *need_order_node (insn_info *);

  void add_insn_after (insn_info *, insn_info *);
  void append_insn (insn_info *);
  void remove_insn (insn_info *);

  insn_info *append_artificial_insn (bb_info *, rtx_insn * = nullptr);

  void start_insn_accesses ();
  void finish_insn_accesses (insn_info *);

  use_info *create_reg_use (build_info &, insn_info *, resource_info);
  void record_use (build_info &, insn_info *, rtx_obj_reference);
  void record_call_clobbers (build_info &, insn_info *, rtx_call_insn *);
  void record_def (build_info &, insn_info *, rtx_obj_reference);
  void add_insn_to_block (build_info &, rtx_insn *);

  void add_reg_unused_notes (insn_info *);

  void add_live_out_use (bb_info *, set_info *);
  set_info *live_out_value (bb_info *, set_info *);

  void append_phi (ebb_info *, phi_info *);
  void remove_phi (phi_info *);
  void delete_phi (phi_info *);
  void replace_phi (phi_info *, set_info *);
  phi_info *create_phi (ebb_info *, resource_info, access_info **,
			unsigned int);
  phi_info *create_degenerate_phi (ebb_info *, set_info *);

  bb_info *create_bb_info (basic_block);
  void append_bb (bb_info *);

  insn_info *add_placeholder_after (insn_info *);
  void possibly_queue_changes (insn_change &);
  void finalize_new_accesses (insn_change &);
  void apply_changes_to_insn (insn_change &);

  void init_function_data ();
  void calculate_potential_phi_regs (build_info &);
  void place_phis (build_info &);
  void create_ebbs (build_info &);
  void add_entry_block_defs (build_info &);
  void calculate_ebb_live_in_for_debug (build_info &);
  void add_phi_nodes (build_info &);
  void add_artificial_accesses (build_info &, df_ref_flags);
  void add_block_contents (build_info &);
  void record_block_live_out (build_info &);
  void start_block (build_info &, bb_info *);
  void end_block (build_info &, bb_info *);
  void populate_phi_inputs (build_info &);
  void process_all_blocks ();

  void simplify_phi_setup (phi_info *, set_info **, bitmap);
  void simplify_phi_propagate (phi_info *, set_info **, bitmap, bitmap);
  void simplify_phis ();

  // The function that this object describes.
  function *m_fn;

  // The lowest (negative) in-use artificial insn uid minus one.
  int m_next_artificial_uid;

  // The highest in-use phi uid plus one.
  unsigned int m_next_phi_uid;

  // The highest in-use register number plus one.
  unsigned int m_num_regs;

  // M_DEFS[R] is the first definition of register R - 1 in a reverse
  // postorder traversal of the function, or null if the function has
  // no definition of R.  Applying last () gives the last definition of R.
  //
  // M_DEFS[0] is for memory; MEM_REGNO + 1 == 0.
  auto_vec<def_info *> m_defs;

  // M_BBS[BI] gives the SSA information about the block with index BI.
  auto_vec<bb_info *> m_bbs;

  // An obstack used to allocate the main RTL SSA information.
  obstack m_obstack;

  // An obstack used for temporary work, such as while building up a list
  // of possible instruction changes.
  obstack m_temp_obstack;

  // The start of each obstack, so that all memory in them can be freed.
  char *m_obstack_start;
  char *m_temp_obstack_start;

  // The entry and exit blocks.
  bb_info *m_first_bb;
  bb_info *m_last_bb;

  // The first and last instructions in a reverse postorder traversal
  // of the function.
  insn_info *m_first_insn;
  insn_info *m_last_insn;

  // The last nondebug instruction in the list of instructions.
  // This is only different from m_last_insn when building the initial
  // SSA information; after that, the last instruction is always a
  // BB end instruction.
  insn_info *m_last_nondebug_insn;

  // Temporary working state when building up lists of definitions and uses.
  // Keeping them around should reduce the number of unnecessary reallocations.
  auto_vec<access_info *> m_temp_defs;
  auto_vec<access_info *> m_temp_uses;

  // A list of phis that are no longer in use.  Their uids are still unique
  // and so can be recycled.
  phi_info *m_free_phis;

  // A list of instructions that have been changed in ways that need
  // further processing later, such as removing dead instructions or
  // altering the CFG.
  auto_vec<insn_info *> m_queued_insn_updates;

  // The INSN_UIDs of all instructions in M_QUEUED_INSN_UPDATES.
  auto_bitmap m_queued_insn_update_uids;

  // A basic_block is in this bitmap if we need to call purge_dead_edges
  // on it.  As with M_QUEUED_INSN_UPDATES, these updates are queued until
  // a convenient point.
  auto_bitmap m_need_to_purge_dead_edges;
};

void pp_function (pretty_printer *, const function_info *);
}

void dump (FILE *, const rtl_ssa::function_info *);

void DEBUG_FUNCTION debug (const rtl_ssa::function_info *);
