/* Control flow redundancy hardening.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_ALGORITHM /* find */
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "memmodel.h"
#include "tm_p.h"
#include "tree.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimplify.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "tree-cfg.h"
#include "tree-cfgcleanup.h"
#include "tree-eh.h"
#include "except.h"
#include "sbitmap.h"
#include "basic-block.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "cgraph.h"
#include "alias.h"
#include "varasm.h"
#include "output.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "intl.h"

namespace {

/* This pass introduces verification, at function exits, that booleans
   set in each basic block during function execution reflect the
   control flow graph: for each visited block, check that at least one
   predecessor and at least one successor were also visited.  This
   sort of hardening may detect various kinds of attacks.  */

/* Define a pass to harden code through control flow redundancy.  */

const pass_data pass_data_harden_control_flow_redundancy = {
  GIMPLE_PASS,
  "hardcfr",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg | PROP_ssa, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  TODO_cleanup_cfg, // properties_start
  0,        // properties_finish
};

class pass_harden_control_flow_redundancy : public gimple_opt_pass
{
public:
  pass_harden_control_flow_redundancy (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_harden_control_flow_redundancy, ctxt)
  {}
  opt_pass *clone () { return new pass_harden_control_flow_redundancy (m_ctxt); }
  virtual bool gate (function *fun) {
    /* Return quickly if the pass is disabled, without checking any of
       the conditions that might give rise to warnings that would only
       be appropriate if hardening was requested.  */
    if (!flag_harden_control_flow_redundancy)
      return false;

    /* Functions that return more than once, like setjmp and vfork
       (that also gets this flag set), will start recording a path
       after the first return, and then may take another path when
       they return again.  The unterminated path may then be flagged
       as an error.  ??? We could save the visited array before the
       call and restore it if it returns again.  */
    if (fun->calls_setjmp)
      {
	warning_at (DECL_SOURCE_LOCATION (fun->decl), 0,
		    "%qD calls %<setjmp%> or similar,"
		    " %<-fharden-control-flow-redundancy%> is not supported",
		    fun->decl);
	return false;
      }

    /* Some targets bypass the abnormal dispatcher block in nonlocal
       gotos, and then we'd miss its visited bit.  It might be doable
       to make it work uniformly, but this feature is not used often
       enough to make it worthwhile.  */
    if (fun->has_nonlocal_label)
      {
	warning_at (DECL_SOURCE_LOCATION (fun->decl), 0,
		    "%qD receives nonlocal gotos,"
		    " %<-fharden-control-flow-redundancy%> is not supported",
		    fun->decl);
	return false;
      }

    if (fun->cfg && param_hardcfr_max_blocks > 0
	&& (n_basic_blocks_for_fn (fun) - NUM_FIXED_BLOCKS
	    > param_hardcfr_max_blocks))
      {
	warning_at (DECL_SOURCE_LOCATION (fun->decl), 0,
		    "%qD has more than %u blocks, the requested"
		    " maximum for %<-fharden-control-flow-redundancy%>",
		    fun->decl, param_hardcfr_max_blocks);
	return false;
      }

    return true;
  }
  virtual unsigned int execute (function *);
};

}

/* Return TRUE iff CFR checks should be inserted before returning
   calls.  */

static bool
check_returning_calls_p ()
{
  return
    flag_harden_control_flow_redundancy_check_returning_calls > 0
    || (flag_harden_control_flow_redundancy_check_returning_calls < 0
	/* Gates pass_tail_calls.  */
	&& flag_optimize_sibling_calls
	/* Gates pass_all_optimizations.  */
	&& optimize >= 1 && !optimize_debug);
}

/* Scan BB from the end, updating *RETPTR if given as return stmts and
   copies are found.  Return a call or a stmt that cannot appear after
   a tail call, or NULL if the top of the block is reached without
   finding any.  */

static gimple *
hardcfr_scan_block (basic_block bb, tree **retptr)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      /* Ignore labels, returns, nops, clobbers and debug stmts.  */
      if (gimple_code (stmt) == GIMPLE_LABEL
	  || gimple_code (stmt) == GIMPLE_NOP
	  || gimple_code (stmt) == GIMPLE_PREDICT
	  || gimple_clobber_p (stmt)
	  || is_gimple_debug (stmt))
	continue;

      if (gimple_code (stmt) == GIMPLE_RETURN)
	{
	  greturn *gret = as_a <greturn *> (stmt);
	  if (retptr)
	    {
	      gcc_checking_assert (!*retptr);
	      *retptr = gimple_return_retval_ptr (gret);
	    }
	  continue;
	}

      /* Check for a call.  */
      if (is_gimple_call (stmt))
	return stmt;

      /* Allow simple copies to the return value, updating the return
	 value to be found in earlier assignments.  */
      if (retptr && *retptr && gimple_assign_single_p (stmt)
	  && **retptr == gimple_assign_lhs (stmt))
	{
	  *retptr = gimple_assign_rhs1_ptr (stmt);
	  continue;
	}

      return stmt;
    }

  /* Any other kind of stmt will prevent a tail call.  */
  return NULL;
}

/* Return TRUE iff CALL is to be preceded by a CFR checkpoint, i.e.,
   if it's a returning call (one whose result is ultimately returned
   without intervening non-copy statements) and we're checking
   returning calls, a __builtin_return call (noreturn with a path to
   the exit block), a must-tail call, or a tail call.  */

static bool
returning_call_p (gcall *call)
{
  if (!(gimple_call_noreturn_p (call)
	|| gimple_call_must_tail_p (call)
	|| gimple_call_tail_p (call)
	|| check_returning_calls_p ()))
    return false;

  /* Quickly check that there's a path to exit compatible with a
     returning call.  Detect infinite loops by limiting the path
     length to the basic block count, and by looking for duplicate
     blocks before allocating more memory for the path, for amortized
     O(n).  */
  auto_vec<basic_block, 10> path;
  for (basic_block bb = gimple_bb (call);
       bb != EXIT_BLOCK_PTR_FOR_FN (cfun);
       bb = single_succ (bb))
    if (!single_succ_p (bb)
	|| (single_succ_edge (bb)->flags & EDGE_EH) != 0
	|| n_basic_blocks_for_fn (cfun) - path.length () <= NUM_FIXED_BLOCKS
	|| (path.length () == path.allocated ()
	    && std::find (path.begin (), path.end (), bb) != path.end ()))
      return false;
    else
      path.safe_push (bb);

  /* Check the stmts in the blocks and trace the return value.  */
  tree *retptr = NULL;
  for (;;)
    {
      gcc_checking_assert (!path.is_empty ());
      basic_block bb = path.pop ();
      gimple *stop = hardcfr_scan_block (bb, &retptr);
      if (stop)
	{
	  if (stop != call)
	    return false;
	  gcc_checking_assert (path.is_empty ());
	  break;
	}

      gphi *retphi = NULL;
      if (retptr && *retptr && TREE_CODE (*retptr) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (*retptr)
	  && SSA_NAME_DEF_STMT (*retptr)
	  && is_a <gphi *> (SSA_NAME_DEF_STMT (*retptr))
	  && gimple_bb (SSA_NAME_DEF_STMT (*retptr)) == bb)
	{
	  retphi = as_a <gphi *> (SSA_NAME_DEF_STMT (*retptr));
	  gcc_checking_assert (gimple_phi_result (retphi) == *retptr);
	}
      else
	continue;

      gcc_checking_assert (!path.is_empty ());
      edge e = single_succ_edge (path.last ());
      int i = EDGE_COUNT (bb->preds);
      while (i--)
	if (EDGE_PRED (bb, i) == e)
	  break;
      gcc_checking_assert (i >= 0);
      retptr = gimple_phi_arg_def_ptr (retphi, i);
    }

  return (gimple_call_noreturn_p (call)
	  || gimple_call_must_tail_p (call)
	  || gimple_call_tail_p (call)
	  || (gimple_call_lhs (call) == (retptr ? *retptr : NULL)
	      && check_returning_calls_p ()));
}

typedef auto_vec<edge, 10> chk_edges_t;

/* Declare for mutual recursion.  */
static bool hardcfr_sibcall_search_preds (basic_block bb,
					  chk_edges_t &chk_edges,
					  int &count_chkcall,
					  auto_sbitmap &chkcall_blocks,
					  int &count_postchk,
					  auto_sbitmap &postchk_blocks,
					  tree *retptr);

/* Search backwards from the end of BB for a mandatory or potential
   sibcall.  Schedule the block to be handled sort-of like noreturn if
   so.  Recurse to preds, with updated RETPTR, if the block only
   contains stmts that may follow such a call, scheduling checking at
   edges and marking blocks as post-check as needed.  Return true iff,
   at the end of the block, a check will have already been
   performed.  */

static bool
hardcfr_sibcall_search_block (basic_block bb,
			      chk_edges_t &chk_edges,
			      int &count_chkcall,
			      auto_sbitmap &chkcall_blocks,
			      int &count_postchk,
			      auto_sbitmap &postchk_blocks,
			      tree *retptr)
{
  /* Conditionals and internal exceptions rule out tail calls.  */
  if (!single_succ_p (bb)
      || (single_succ_edge (bb)->flags & EDGE_EH) != 0)
    return false;

  gimple *stmt = hardcfr_scan_block (bb, &retptr);
  if (!stmt)
    return hardcfr_sibcall_search_preds (bb, chk_edges,
					 count_chkcall, chkcall_blocks,
					 count_postchk, postchk_blocks,
					 retptr);

  if (!is_a <gcall *> (stmt))
    return false;

  /* Avoid disrupting mandatory or early-marked tail calls,
     inserting the check before them.  This works for
     must-tail calls, but tail calling as an optimization is
     detected too late for us.

     Also check for noreturn calls here.  Noreturn calls won't
     normally have edges to exit, so they won't be found here,
     but __builtin_return does, and we must check before
     it, so handle it like a tail call.  */
  gcall *call = as_a <gcall *> (stmt);
  if (!(gimple_call_noreturn_p (call)
	|| gimple_call_must_tail_p (call)
	|| gimple_call_tail_p (call)
	|| (gimple_call_lhs (call) == (retptr ? *retptr : NULL)
	    && check_returning_calls_p ())))
    return false;

  gcc_checking_assert (returning_call_p (call));

  /* We found a call that is to be preceded by checking.  */
  if (bitmap_set_bit (chkcall_blocks, bb->index))
    ++count_chkcall;
  else
    gcc_unreachable ();
  return true;
}


/* Search preds of BB for a mandatory or potential sibcall or
   returning call, and arrange for the blocks containing them to have
   a check inserted before the call, like noreturn calls.  If any
   preds are found to perform checking, schedule checks at the edges
   of those that don't, and mark BB as postcheck..  */

static bool
hardcfr_sibcall_search_preds (basic_block bb,
			      chk_edges_t &chk_edges,
			      int &count_chkcall,
			      auto_sbitmap &chkcall_blocks,
			      int &count_postchk,
			      auto_sbitmap &postchk_blocks,
			      tree *retptr)
{
  /* For the exit block, we wish to force a check at every
     predecessor, so pretend we've already found a pred that had
     checking, so that we schedule checking at every one of its pred
     edges.  */
  bool first = bb->index >= NUM_FIXED_BLOCKS;
  bool postchecked = true;

  gphi *retphi = NULL;
  if (retptr && *retptr && TREE_CODE (*retptr) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (*retptr)
      && SSA_NAME_DEF_STMT (*retptr)
      && is_a <gphi *> (SSA_NAME_DEF_STMT (*retptr))
      && gimple_bb (SSA_NAME_DEF_STMT (*retptr)) == bb)
    {
      retphi = as_a <gphi *> (SSA_NAME_DEF_STMT (*retptr));
      gcc_checking_assert (gimple_phi_result (retphi) == *retptr);
    }

  for (int i = EDGE_COUNT (bb->preds); i--; first = false)
    {
      edge e = EDGE_PRED (bb, i);

      bool checked
	= hardcfr_sibcall_search_block (e->src, chk_edges,
					count_chkcall, chkcall_blocks,
					count_postchk, postchk_blocks,
					!retphi ? retptr
					: gimple_phi_arg_def_ptr (retphi, i));

      if (first)
	{
	  postchecked = checked;
	  continue;
	}

      /* When we first find a checked block, force a check at every
	 other incoming edge we've already visited, and those we
	 visit afterwards that don't have their own check, so that
	 when we reach BB, the check has already been performed.  */
      if (!postchecked && checked)
	{
	  for (int j = EDGE_COUNT (bb->preds); --j > i; )
	    chk_edges.safe_push (EDGE_PRED (bb, j));
	  postchecked = true;
	}
      if (postchecked && !checked)
	chk_edges.safe_push (EDGE_PRED (bb, i));
    }

  if (postchecked && bb->index >= NUM_FIXED_BLOCKS)
    {
      if (bitmap_set_bit (postchk_blocks, bb->index))
	count_postchk++;
      else
	gcc_unreachable ();
    }

  return postchecked;
}


class rt_bb_visited
{
  /* Use a sufficiently wide unsigned type to hold basic block numbers.  */
  typedef size_t blknum;

  /* Record the original block count of the function.  */
  blknum nblocks;
  /* Record the number of bits per VWORD (short for VISITED WORD), an
     efficient mode to set and test bits for blocks we visited, and to
     encode the CFG in case out-of-line verification is used.  */
  unsigned vword_bits;

  /* Hold the unsigned integral VWORD type.  */
  tree vword_type;
  /* Hold a pointer-to-VWORD type.  */
  tree vword_ptr;

  /* Hold a growing sequence used to check, inline or out-of-line,
     that VISITED encodes an expected execution path.  */
  gimple_seq ckseq;
  /* If nonNULL, hold a growing representation of the CFG for
     out-of-line testing.  */
  tree rtcfg;

  /* Hold the declaration of an array of VWORDs, used as an array of
     NBLOCKS-2 bits.  */
  tree visited;

  /* If performing inline checking, hold a declarations of boolean
     variables used for inline checking.  CKBLK holds the result of
     testing whether the VISITED bit corresponding to a predecessor or
     successor is set, CKINV inverts that bit, CKPART gets cleared if
     a block was not visited or if CKINV for any of its predecessors
     or successors is set, and CKFAIL gets set if CKPART remains set
     at the end of a block's predecessors or successors list.  */
  tree ckfail, ckpart, ckinv, ckblk;

  /* If we need to deal with abnormal edges, we insert SSA_NAMEs for
     boolean true and false.  */
  tree vfalse, vtrue;

  /* Convert a block index N to a block vindex, the index used to
     identify it in the VISITED array.  Check that it's in range:
     neither ENTRY nor EXIT, but maybe one-past-the-end, to compute
     the visited array length.  */
  blknum num2idx (blknum n) {
    gcc_checking_assert (n >= NUM_FIXED_BLOCKS && n <= nblocks);
    return (n - NUM_FIXED_BLOCKS);
  }
  /* Return the block vindex for BB, that must not be ENTRY or
     EXIT.  */
  blknum bb2idx (basic_block bb) {
    gcc_checking_assert (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun)
			 && bb != EXIT_BLOCK_PTR_FOR_FN (cfun));
    gcc_checking_assert (blknum (bb->index) < nblocks);
    return num2idx (bb->index);
  }

  /* Compute the type to be used for the VISITED array.  */
  tree vtype ()
  {
    blknum n = num2idx (nblocks);
    return build_array_type_nelts (vword_type,
				   (n + vword_bits - 1) / vword_bits);
  }

  /* Compute and return the index into VISITED for block BB.  If BITP
     is non-NULL, also compute and store the bit mask corresponding to
     block BB in *BITP, so that (visited[index] & mask) tells whether
     BB was visited.  */
  tree vwordidx (basic_block bb, tree *bitp = NULL)
  {
    blknum idx = bb2idx (bb);
    if (bitp)
      {
	unsigned bit = idx % vword_bits;
	/* We don't need to adjust shifts to follow native bit
	   endianness here, all of our uses of the CFG and visited
	   bitmaps, whether at compile or runtime, are shifted bits on
	   full words.  This adjustment here would require a
	   corresponding adjustment at runtime, which would be nothing
	   but undesirable overhead for us.  */
	if (0 /* && BITS_BIG_ENDIAN */)
	  bit = vword_bits - bit - 1;
	wide_int wbit = wi::set_bit_in_zero (bit, vword_bits);
	*bitp = wide_int_to_tree (vword_type, wbit);
      }
    return build_int_cst (vword_ptr, idx / vword_bits);
  }

  /* Return an expr to accesses the visited element that holds
     information about BB.  If BITP is non-NULL, set it to the mask to
     tell which bit in that expr refers to BB.  */
  tree vword (basic_block bb, tree *bitp = NULL)
  {
    return build2 (MEM_REF, vword_type,
		   build1 (ADDR_EXPR, vword_ptr, visited),
		   int_const_binop (MULT_EXPR, vwordidx (bb, bitp),
				    fold_convert (vword_ptr,
						  TYPE_SIZE_UNIT
						  (vword_type))));
  }

  /* Return an expr that evaluates to true iff BB was marked as
     VISITED.  Add any gimple stmts to SEQP.  */
  tree vindex (basic_block bb, gimple_seq *seqp)
  {
    if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
	|| bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
      return boolean_true_node;

    tree bit, setme = vword (bb, &bit);
    tree temp = create_tmp_var (vword_type, ".cfrtemp");

    gassign *vload = gimple_build_assign (temp, setme);
    gimple_seq_add_stmt (seqp, vload);

    gassign *vmask = gimple_build_assign (temp, BIT_AND_EXPR, temp, bit);
    gimple_seq_add_stmt (seqp, vmask);

    return build2 (NE_EXPR, boolean_type_node,
		   temp, build_int_cst (vword_type, 0));
  }

  /* Set the bit corresponding to BB in VISITED.  Add to SEQ any
     required gimple stmts, and return SEQ, possibly modified.  */
  gimple_seq vset (basic_block bb, gimple_seq seq = NULL)
  {
    tree bit, setme = vword (bb, &bit);
    tree temp = create_tmp_var (vword_type, ".cfrtemp");

    gassign *vload = gimple_build_assign (temp, setme);
    gimple_seq_add_stmt (&seq, vload);

    gassign *vbitset = gimple_build_assign (temp, BIT_IOR_EXPR, temp, bit);
    gimple_seq_add_stmt (&seq, vbitset);

    gassign *vstore = gimple_build_assign (unshare_expr (setme), temp);
    gimple_seq_add_stmt (&seq, vstore);

    /* Prevent stores into visited from being deferred, forcing
       subsequent bitsets to reload the word rather than reusing
       values already in register.  The purpose is threefold: make the
       bitset get to memory in this block, so that control flow
       attacks in functions called in this block don't easily bypass
       the bitset; prevent the bitset word from being retained in a
       register across blocks, which could, in an attack scenario,
       make a later block set more than one bit; and prevent hoisting
       or sinking loads or stores of bitset words out of loops or even
       throughout functions, which could significantly weaken the
       verification.  This is equivalent to making the bitsetting
       volatile within the function body, but without changing its
       type; making the bitset volatile would make inline checking far
       less optimizable for no reason.  */
    vec<tree, va_gc> *inputs = NULL;
    vec<tree, va_gc> *outputs = NULL;
    vec_safe_push (outputs,
		   build_tree_list
		   (build_tree_list
		    (NULL_TREE, build_string (2, "=m")),
		    visited));
    vec_safe_push (inputs,
		   build_tree_list
		   (build_tree_list
		    (NULL_TREE, build_string (1, "m")),
		    visited));
    gasm *stabilize = gimple_build_asm_vec ("", inputs, outputs,
					    NULL, NULL);
    gimple_seq_add_stmt (&seq, stabilize);

    return seq;
  }

public:
  /* Prepare to add control flow redundancy testing to CFUN.  */
  rt_bb_visited (int checkpoints)
    : nblocks (n_basic_blocks_for_fn (cfun)),
      vword_type (NULL), ckseq (NULL), rtcfg (NULL),
      vfalse (NULL), vtrue (NULL)
  {
    /* If we've already added a declaration for the builtin checker,
       extract vword_type and vword_bits from its declaration.  */
    if (tree checkfn = builtin_decl_explicit (BUILT_IN___HARDCFR_CHECK))
      {
	tree check_arg_list = TYPE_ARG_TYPES (TREE_TYPE (checkfn));
	tree vword_const_ptr_type = TREE_VALUE (TREE_CHAIN (check_arg_list));
	vword_type = TYPE_MAIN_VARIANT (TREE_TYPE (vword_const_ptr_type));
	vword_bits = tree_to_shwi (TYPE_SIZE (vword_type));
      }
    /* Otherwise, select vword_bits, vword_type et al, and use it to
       declare the builtin checker.  */
    else
      {
	/* This setting needs to be kept in sync with libgcc/hardcfr.c.
	   We aim for at least 28 bits, which enables us to refer to as
	   many as 28 << 28 blocks in a function's CFG.  That's way over
	   4G blocks.  */
	machine_mode VWORDmode;
	if (BITS_PER_UNIT >= 28)
	  {
	    VWORDmode = QImode;
	    vword_bits = BITS_PER_UNIT;
	  }
	else if (BITS_PER_UNIT >= 14)
	  {
	    VWORDmode = HImode;
	    vword_bits = 2 * BITS_PER_UNIT;
	  }
	else
	  {
	    VWORDmode = SImode;
	    vword_bits = 4 * BITS_PER_UNIT;
	  }

	vword_type = lang_hooks.types.type_for_mode (VWORDmode, 1);
	gcc_checking_assert (vword_bits == tree_to_shwi (TYPE_SIZE
							 (vword_type)));

	vword_type = build_variant_type_copy (vword_type);
	TYPE_ALIAS_SET (vword_type) = new_alias_set ();

	tree vword_const = build_qualified_type (vword_type, TYPE_QUAL_CONST);
	tree vword_const_ptr = build_pointer_type (vword_const);
	tree type = build_function_type_list (void_type_node, sizetype,
					      vword_const_ptr, vword_const_ptr,
					      NULL_TREE);
	tree decl = add_builtin_function_ext_scope
	  ("__builtin___hardcfr_check",
	   type, BUILT_IN___HARDCFR_CHECK, BUILT_IN_NORMAL,
	   "__hardcfr_check", NULL_TREE);
	TREE_NOTHROW (decl) = true;
	set_builtin_decl (BUILT_IN___HARDCFR_CHECK, decl, true);
      }

    /* The checker uses a qualified pointer, so we can't reuse it,
       so build a new one.  */
    vword_ptr = build_pointer_type (vword_type);

    tree visited_type = vtype ();
    visited = create_tmp_var (visited_type, ".cfrvisited");

    if (nblocks - NUM_FIXED_BLOCKS > blknum (param_hardcfr_max_inline_blocks)
	|| checkpoints > 1)
      {
	/* Make sure vword_bits is wide enough for the representation
	   of nblocks in rtcfg.  Compare with vword_bits << vword_bits,
	   but avoiding overflows, shifting nblocks right instead.  If
	   vword_bits is wider than HOST_WIDE_INT, assume it fits, so
	   as to avoid undefined shifts.  */
	gcc_assert (HOST_BITS_PER_WIDE_INT <= vword_bits
		    || (((unsigned HOST_WIDE_INT)(num2idx (nblocks))
			 >> vword_bits) < vword_bits));

	/* Build a terminator for the constructor list.  */
	rtcfg = build_tree_list (NULL_TREE, NULL_TREE);
	return;
      }

    ckfail = create_tmp_var (boolean_type_node, ".cfrfail");
    ckpart = create_tmp_var (boolean_type_node, ".cfrpart");
    ckinv = create_tmp_var (boolean_type_node, ".cfrinv");
    ckblk = create_tmp_var (boolean_type_node, ".cfrblk");

    gassign *ckfail_init = gimple_build_assign (ckfail, boolean_false_node);
    gimple_seq_add_stmt (&ckseq, ckfail_init);
  }

  /* Insert SEQ before a resx or a call in INSBB.  */
  void insert_exit_check_in_block (gimple_seq seq, basic_block insbb)
  {
    gimple_stmt_iterator gsi = gsi_last_bb (insbb);

    while (!gsi_end_p (gsi))
      if (is_a <gresx *> (gsi_stmt (gsi))
	  || is_a <gcall *> (gsi_stmt (gsi)))
	break;
      else
	gsi_prev (&gsi);

    gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
  }

  /* Insert SEQ on E.  */
  void insert_exit_check_on_edge (gimple_seq seq, edge e)
  {
    if (!(e->flags & EDGE_ABNORMAL))
      {
	gsi_insert_seq_on_edge_immediate (e, seq);
	return;
      }

    /* Initialize SSA boolean constants for use in abnormal PHIs.  */
    if (!vfalse)
      {
	vfalse = make_ssa_name (boolean_type_node);
	vtrue = make_ssa_name (boolean_type_node);

	gimple_seq vft_seq = NULL;
	gassign *vfalse_init = gimple_build_assign (vfalse, boolean_false_node);
	gimple_seq_add_stmt (&vft_seq, vfalse_init);
	gassign *vtrue_init = gimple_build_assign (vtrue, boolean_true_node);
	gimple_seq_add_stmt (&vft_seq, vtrue_init);

	gsi_insert_seq_on_edge_immediate (single_succ_edge
					  (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
					  vft_seq);
      }

    /* We can't insert on abnormal edges, but we can arrange for SEQ
       to execute conditionally at dest.  Add a PHI boolean with TRUE
       from E and FALSE from other preds, split the whole block, add a
       test for the PHI to run a new block with SEQ or skip straight
       to the original block.  If there are multiple incoming abnormal
       edges, we'll do this multiple times.  ??? Unless there are
       multiple abnormal edges with different postcheck status, we
       could split the block and redirect other edges, rearranging the
       PHI nodes.  Optimizers already know how to do this, so we can
       keep things simple here.  */
    basic_block bb = e->dest;
    basic_block bb_postcheck = split_block_after_labels (bb)->dest;

    basic_block bb_check = create_empty_bb (e->dest);
    bb_check->count = e->count ();
    if (dom_info_available_p (CDI_DOMINATORS))
      set_immediate_dominator (CDI_DOMINATORS, bb_check, bb);
    if (current_loops)
      add_bb_to_loop (bb_check, current_loops->tree_root);

    gimple_stmt_iterator chkpt = gsi_after_labels (bb_check);
    gsi_insert_seq_before_without_update (&chkpt, seq, GSI_SAME_STMT);
    edge edge_postcheck = make_edge (bb_check, bb_postcheck, EDGE_FALLTHRU);
    edge_postcheck->probability = profile_probability::always ();

    tree cond_var = make_ssa_name (boolean_type_node);
    gcond *cond = gimple_build_cond (NE_EXPR, cond_var, boolean_false_node,
				     NULL, NULL);
    gimple_stmt_iterator condpt = gsi_after_labels (bb);
    gsi_insert_before (&condpt, cond, GSI_SAME_STMT);
    edge edge_nocheck = single_succ_edge (bb);
    edge_nocheck->flags &= ~EDGE_FALLTHRU;
    edge_nocheck->flags |= EDGE_FALSE_VALUE;
    edge edge_check = make_edge (bb, bb_check, EDGE_TRUE_VALUE);
    edge_check->probability = e->count ().probability_in (bb->count);
    edge_nocheck->probability = edge_check->probability.invert ();

    gphi *cond_phi = create_phi_node (cond_var, bb);
    for (int i = 0, ei = EDGE_COUNT (bb->preds); i < ei; i++)
      {
	edge pred = EDGE_PRED (bb, i);
	bool check_edge = pred == e;
	tree val = check_edge ? vtrue : vfalse;
	add_phi_arg (cond_phi, val, pred, UNKNOWN_LOCATION);
      }
  }

  /* Add checking code to CHK_EDGES and CHKCALL_BLOCKS, and
     initialization code on the entry edge.  Before this point, the
     CFG has been undisturbed, and all the needed data has been
     collected and safely stowed.  */
  void check (chk_edges_t &chk_edges,
	      int count_chkcall, auto_sbitmap const &chkcall_blocks)
  {
    /* If we're using out-of-line checking, create and statically
       initialize the CFG checking representation, generate the
       checker call for the checking sequence, and insert it in all
       exit edges, if there's more than one.  If there's only one, we
       use the same logic as the inline case to insert the check
       sequence.  */
    if (rtcfg)
      {
	/* Unreverse the list, and drop the tail node turned into head.  */
	rtcfg = TREE_CHAIN (nreverse (rtcfg));

	/* Turn the indices stored in TREE_PURPOSE into separate
	   nodes.  It was useful to keep them together to enable
	   combination of masks and for clear separation of
	   terminators while constructing it, but now we have to turn
	   it into a sequence of words.  */
	for (tree node = rtcfg; node; node = TREE_CHAIN (node))
	  {
	    tree wordidx = TREE_PURPOSE (node);
	    if (!wordidx)
	      continue;

	    TREE_PURPOSE (node) = NULL_TREE;
	    TREE_CHAIN (node) = tree_cons (NULL_TREE,
					   fold_convert (vword_type, wordidx),
					   TREE_CHAIN (node));
	  }

	/* Build the static initializer for the array with the CFG
	   representation for out-of-line checking.  */
	tree init = build_constructor_from_list (NULL_TREE, rtcfg);
	TREE_TYPE (init) = build_array_type_nelts (vword_type,
						   CONSTRUCTOR_NELTS (init));
	char buf[32];
	ASM_GENERATE_INTERNAL_LABEL (buf, "Lhardcfg",
				     current_function_funcdef_no);
	rtcfg = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			    get_identifier (buf),
			    TREE_TYPE (init));
	TREE_READONLY (rtcfg) = 1;
	TREE_STATIC (rtcfg) = 1;
	TREE_ADDRESSABLE (rtcfg) = 1;
	TREE_USED (rtcfg) = 1;
	DECL_ARTIFICIAL (rtcfg) = 1;
	DECL_IGNORED_P (rtcfg) = 1;
	DECL_INITIAL (rtcfg) = init;
	make_decl_rtl (rtcfg);
	varpool_node::finalize_decl (rtcfg);

	/* Add the checker call to ckseq.  */
	gcall *call_chk = gimple_build_call (builtin_decl_explicit
					     (BUILT_IN___HARDCFR_CHECK), 3,
					     build_int_cst (sizetype,
							    num2idx (nblocks)),
					     build1 (ADDR_EXPR, vword_ptr,
						     visited),
					     build1 (ADDR_EXPR, vword_ptr,
						     rtcfg));
	gimple_seq_add_stmt (&ckseq, call_chk);

	gimple *clobber = gimple_build_assign (visited,
					       build_clobber
					       (TREE_TYPE (visited)));
	gimple_seq_add_stmt (&ckseq, clobber);

	/* If we have multiple exit edges, insert (copies of)
	   ckseq in all of them.  */
	for (int i = chk_edges.length (); i--; )
	  {
	    gimple_seq seq = ckseq;
	    /* Copy the sequence, unless we're dealing with the
	       last edge (we're counting down to zero).  */
	    if (i || count_chkcall)
	      seq = gimple_seq_copy (seq);

	    edge e = chk_edges[i];

	    if (dump_file)
	      {
		if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		  fprintf (dump_file,
			   "Inserting out-of-line check in"
			   " block %i's edge to exit.\n",
			   e->src->index);
		else
		  fprintf (dump_file,
			   "Inserting out-of-line check in"
			   " block %i's edge to postcheck block %i.\n",
			   e->src->index, e->dest->index);
	      }

	    insert_exit_check_on_edge (seq, e);

	    gcc_checking_assert (!bitmap_bit_p (chkcall_blocks, e->src->index));
	  }

	sbitmap_iterator it;
	unsigned i;
	EXECUTE_IF_SET_IN_BITMAP (chkcall_blocks, 0, i, it)
	  {
	    basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);

	    gimple_seq seq = ckseq;
	    gcc_checking_assert (count_chkcall > 0);
	    if (--count_chkcall)
	      seq = gimple_seq_copy (seq);

	    if (dump_file)
	      fprintf (dump_file,
		       "Inserting out-of-line check before stmt in block %i.\n",
		       bb->index);

	    insert_exit_check_in_block (seq, bb);
	  }

	gcc_checking_assert (count_chkcall == 0);
      }
    else
      {
	/* Inline checking requires a single exit edge.  */
	gimple *last = gimple_build_assign (visited,
					    build_clobber
					    (TREE_TYPE (visited)));
	gimple_seq_add_stmt (&ckseq, last);

	if (!count_chkcall)
	  {
	    edge e = single_pred_edge (EXIT_BLOCK_PTR_FOR_FN (cfun));

	    if (dump_file)
	      {
		if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		  fprintf (dump_file,
			   "Inserting out-of-line check in"
			   " block %i's edge to postcheck block %i.\n",
			   e->src->index, e->dest->index);
		else
		  fprintf (dump_file,
			   "Inserting inline check in"
			   " block %i's edge to exit.\n",
			   e->src->index);
	      }

	    insert_exit_check_on_edge (ckseq, e);
	  }
	else
	  {
	    gcc_checking_assert (count_chkcall == 1);

	    sbitmap_iterator it;
	    unsigned i;
	    EXECUTE_IF_SET_IN_BITMAP (chkcall_blocks, 0, i, it)
	      {
		basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);

		gimple_seq seq = ckseq;
		gcc_checking_assert (count_chkcall > 0);
		if (--count_chkcall)
		  seq = gimple_seq_copy (seq);

		if (dump_file)
		  fprintf (dump_file,
			   "Inserting inline check before stmt in block %i.\n",
			   bb->index);

		insert_exit_check_in_block (seq, bb);
	      }

	    gcc_checking_assert (count_chkcall == 0);
	  }

	/* The inserted ckseq computes CKFAIL at LAST.  Now we have to
	   conditionally trap on it.  */
	basic_block insbb = gimple_bb (last);

	/* Create a block with the unconditional trap.  */
	basic_block trp = create_empty_bb (insbb);
	gimple_stmt_iterator gsit = gsi_after_labels (trp);

	gcall *trap = gimple_build_call (builtin_decl_explicit
					 (BUILT_IN_TRAP), 0);
	gsi_insert_before (&gsit, trap, GSI_SAME_STMT);

	if (BB_PARTITION (insbb))
	  BB_SET_PARTITION (trp, BB_COLD_PARTITION);

	if (current_loops)
	  add_bb_to_loop (trp, current_loops->tree_root);

	/* Insert a conditional branch to the trap block.  If the
	   conditional wouldn't be the last stmt, split the block.  */
	gimple_stmt_iterator gsi = gsi_for_stmt (last);
	if (!gsi_one_before_end_p (gsi))
	  split_block (gsi_bb (gsi), gsi_stmt (gsi));

	gcond *cond = gimple_build_cond (NE_EXPR, ckfail,
					 fold_convert (TREE_TYPE (ckfail),
						       boolean_false_node),
					 NULL, NULL);
	gsi_insert_after (&gsi, cond, GSI_SAME_STMT);

	/* Adjust the edges.  */
	single_succ_edge (gsi_bb (gsi))->flags &= ~EDGE_FALLTHRU;
	single_succ_edge (gsi_bb (gsi))->flags |= EDGE_FALSE_VALUE;
	single_succ_edge (gsi_bb (gsi))->probability
	  = profile_probability::always ();
	edge e = make_edge (gsi_bb (gsi), trp, EDGE_TRUE_VALUE);
	e->probability = profile_probability::never ();
	gcc_checking_assert (e->dest == trp);
	gcc_checking_assert (!e->dest->count.initialized_p ());
	e->dest->count = e->count ();

	/* Set the trap's dominator after splitting.  */
	if (dom_info_available_p (CDI_DOMINATORS))
	  set_immediate_dominator (CDI_DOMINATORS, trp, gimple_bb (last));
      }

    /* Insert initializers for visited at the entry.  Do this after
       other insertions, to avoid messing with block numbers.  */
    gimple_seq iseq = NULL;

    gcall *vinit = gimple_build_call (builtin_decl_explicit
				      (BUILT_IN_MEMSET), 3,
				      build1 (ADDR_EXPR,
					      build_pointer_type
					      (TREE_TYPE (visited)),
					      visited),
				      integer_zero_node,
				      TYPE_SIZE_UNIT (TREE_TYPE (visited)));
    gimple_seq_add_stmt (&iseq, vinit);

    gsi_insert_seq_on_edge_immediate (single_succ_edge
				      (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
				      iseq);
  }

  /* Push onto RTCFG a (mask, index) pair to test for IBB when BB is
     visited. XSELF is to be the ENTRY or EXIT block (depending on
     whether we're looking at preds or succs), to be remapped to BB
     because we can't represent them, and there's no point in testing
     them anyway.  Return true if no further blocks need to be visited
     in the list, because we've already encountered a
     self-reference.  */
  bool
  push_rtcfg_pair (basic_block ibb, basic_block bb,
		   basic_block xself)
  {
    /* We don't have a bit to test for the entry and exit
       blocks, but it is always visited, so we test for the
       block itself, which gets us the right result and
       enables the self-test optimization below.  */
    if (ibb == xself)
      ibb = bb;

    tree mask, idx = vwordidx (ibb, &mask);
    /* Combine masks with the same idx, but not if we're going
       to optimize for self-test.  */
    if (ibb != bb && TREE_PURPOSE (rtcfg)
	&& tree_int_cst_equal (idx, TREE_PURPOSE (rtcfg)))
      TREE_VALUE (rtcfg) = int_const_binop (BIT_IOR_EXPR, mask,
					    TREE_VALUE (rtcfg));
    else
      rtcfg = tree_cons (idx, mask, rtcfg);

    /* For self-tests (i.e., tests that the block itself was
       also visited), testing anything else is pointless,
       because it's a tautology, so just drop other edges.  */
    if (ibb == bb)
      {
	while (TREE_PURPOSE (TREE_CHAIN (rtcfg)))
	  TREE_CHAIN (rtcfg) = TREE_CHAIN (TREE_CHAIN (rtcfg));
	return true;
      }

    return false;
  }

  /* Add to CKSEQ stmts to clear CKPART if OBB is visited.  */
  void
  build_block_check (basic_block obb)
  {
    tree vobb = fold_convert (TREE_TYPE (ckblk),
			      vindex (obb, &ckseq));
    gassign *blkrunp = gimple_build_assign (ckblk, vobb);
    gimple_seq_add_stmt (&ckseq, blkrunp);

    gassign *blknotrunp = gimple_build_assign (ckinv,
					       EQ_EXPR,
					       ckblk,
					       fold_convert
					       (TREE_TYPE (ckblk),
						boolean_false_node));
    gimple_seq_add_stmt (&ckseq, blknotrunp);

    gassign *andblk = gimple_build_assign (ckpart,
					   BIT_AND_EXPR,
					   ckpart, ckinv);
    gimple_seq_add_stmt (&ckseq, andblk);
  }

  /* Add to BB code to set its bit in VISITED, and add to RTCFG or
     CKSEQ the data or code needed to check BB's predecessors and
     successors.  If CHECKPOINT, assume the block is a checkpoint,
     whether or not it has an edge to EXIT.  If POSTCHECK, assume the
     block post-dominates checkpoints and therefore no bitmap setting
     or checks are to be performed in or for it.  Do NOT change the
     CFG.  */
  void visit (basic_block bb, bool checkpoint, bool postcheck)
  {
    /* Set the bit in VISITED when entering the block.  */
    gimple_stmt_iterator gsi = gsi_after_labels (bb);
    if (!postcheck)
      gsi_insert_seq_before (&gsi, vset (bb), GSI_SAME_STMT);

    if (rtcfg)
      {
	if (!postcheck)
	  {
	    /* Build a list of (index, mask) terminated by (NULL, 0).
	       Consolidate masks with the same index when they're
	       adjacent.  First, predecessors.  Count backwards, because
	       we're going to reverse the list.  The order shouldn't
	       matter, but let's not make it surprising.  */
	    for (int i = EDGE_COUNT (bb->preds); i--; )
	      if (push_rtcfg_pair (EDGE_PRED (bb, i)->src, bb,
				   ENTRY_BLOCK_PTR_FOR_FN (cfun)))
		break;
	  }
	rtcfg = tree_cons (NULL_TREE, build_int_cst (vword_type, 0), rtcfg);

	if (!postcheck)
	  {
	    /* Then, successors.  */
	    if (!checkpoint
		|| !push_rtcfg_pair (EXIT_BLOCK_PTR_FOR_FN (cfun),
				     bb, EXIT_BLOCK_PTR_FOR_FN (cfun)))
	      for (int i = EDGE_COUNT (bb->succs); i--; )
		if (push_rtcfg_pair (EDGE_SUCC (bb, i)->dest, bb,
				     EXIT_BLOCK_PTR_FOR_FN (cfun)))
		  break;
	  }
	rtcfg = tree_cons (NULL_TREE, build_int_cst (vword_type, 0), rtcfg);
      }
    else if (!postcheck)
      {
	/* Schedule test to fail if the block was reached but somehow none
	   of its predecessors were.  */
	tree bit = fold_convert (TREE_TYPE (ckpart), vindex (bb, &ckseq));
	gassign *blkrunp = gimple_build_assign (ckpart, bit);
	gimple_seq_add_stmt (&ckseq, blkrunp);

	for (int i = 0, e = EDGE_COUNT (bb->preds); i < e; i++)
	  build_block_check (EDGE_PRED (bb, i)->src);
	gimple *orfailp = gimple_build_assign (ckfail, BIT_IOR_EXPR,
					       ckfail, ckpart);
	gimple_seq_add_stmt (&ckseq, orfailp);

	/* Likewise for successors.  */
	gassign *blkruns = gimple_build_assign (ckpart, unshare_expr (bit));
	gimple_seq_add_stmt (&ckseq, blkruns);

	if (checkpoint)
	  build_block_check (EXIT_BLOCK_PTR_FOR_FN (cfun));
	for (int i = 0, e = EDGE_COUNT (bb->succs); i < e; i++)
	  build_block_check (EDGE_SUCC (bb, i)->dest);

	gimple *orfails = gimple_build_assign (ckfail, BIT_IOR_EXPR,
					       ckfail, ckpart);
	gimple_seq_add_stmt (&ckseq, orfails);
      }
  }
};

/* Avoid checking before noreturn calls that are known (expected,
   really) to finish by throwing an exception, rather than by ending
   the program or looping forever.  Such functions have to be
   annotated, with an attribute (expected_throw) or flag (ECF_XTHROW),
   so that exception-raising functions, such as C++'s __cxa_throw,
   __cxa_rethrow, and Ada's gnat_rcheck_*, gnat_reraise*,
   ada.exception.raise_exception*, and the language-independent
   unwinders could be detected here and handled differently from other
   noreturn functions.  */
static bool
always_throwing_noreturn_call_p (gimple *stmt)
{
  if (!is_a <gcall *> (stmt))
    return is_a <gresx *> (stmt);

  gcall *call = as_a <gcall *> (stmt);
  return (gimple_call_noreturn_p (call)
	  && gimple_call_expected_throw_p (call));
}

/* Control flow redundancy hardening: record the execution path, and
   verify at exit that an expect path was taken.  */

unsigned int
pass_harden_control_flow_redundancy::execute (function *fun)
{
  bool const check_at_escaping_exceptions
    = (flag_exceptions
       && flag_harden_control_flow_redundancy_check_exceptions);
  bool const check_before_noreturn_calls
    = flag_harden_control_flow_redundancy_check_noreturn > HCFRNR_NEVER;
  bool const check_before_nothrow_noreturn_calls
    = (check_before_noreturn_calls
       && flag_harden_control_flow_redundancy_check_noreturn >= HCFRNR_NOTHROW);
  bool const check_before_throwing_noreturn_calls
    = (flag_exceptions
       && check_before_noreturn_calls
       && flag_harden_control_flow_redundancy_check_noreturn > HCFRNR_NOTHROW);
  bool const check_before_always_throwing_noreturn_calls
    = (flag_exceptions
       && check_before_noreturn_calls
       && flag_harden_control_flow_redundancy_check_noreturn >= HCFRNR_ALWAYS);
  basic_block bb;
  basic_block bb_eh_cleanup = NULL;

  if (flag_harden_control_flow_redundancy_skip_leaf)
    {
      bool found_calls_p = false;

      FOR_EACH_BB_FN (bb, fun)
	{
	  for (gimple_stmt_iterator gsi = gsi_last_bb (bb);
	       !gsi_end_p (gsi); gsi_prev (&gsi))
	    if (is_a <gcall *> (gsi_stmt (gsi)))
	      {
		found_calls_p = true;
		break;
	      }
	  if (found_calls_p)
	    break;
	}

      if (!found_calls_p)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Disabling CFR for leaf function, as requested\n");

	  return 0;
	}
    }

  if (check_at_escaping_exceptions)
    {
      int lp_eh_cleanup = -1;

      /* Record the preexisting blocks, to avoid visiting newly-created
	 blocks.  */
      auto_sbitmap to_visit (last_basic_block_for_fn (fun));
      bitmap_clear (to_visit);

      FOR_EACH_BB_FN (bb, fun)
	bitmap_set_bit (to_visit, bb->index);

      /* Scan the blocks for stmts with escaping exceptions, that
	 wouldn't be denoted in the CFG, and associate them with an
	 empty cleanup handler around the whole function.  Walk
	 backwards, so that even when we split the block, */
      sbitmap_iterator it;
      unsigned i;
      EXECUTE_IF_SET_IN_BITMAP (to_visit, 0, i, it)
	{
	  bb = BASIC_BLOCK_FOR_FN (fun, i);

	  for (gimple_stmt_iterator gsi = gsi_last_bb (bb);
	       !gsi_end_p (gsi); gsi_prev (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (!stmt_could_throw_p (fun, stmt))
		continue;

	      /* If it must not throw, or if it already has a handler,
		 we need not worry about it.  */
	      if (lookup_stmt_eh_lp (stmt) != 0)
		continue;

	      /* Don't split blocks at, nor add EH edges to, tail
		 calls, we will add verification before the call
		 anyway.  */
	      if (is_a <gcall *> (stmt)
		  && (gimple_call_must_tail_p (as_a <gcall *> (stmt))
		      || gimple_call_tail_p (as_a <gcall *> (stmt))
		      || returning_call_p (as_a <gcall *> (stmt))))
		continue;

	      if (!gsi_one_before_end_p (gsi))
		split_block (bb, stmt);
	      /* A resx or noreturn call needs not be associated with
		 the cleanup handler if we're going to add checking
		 before it.  We only test cases that didn't require
		 block splitting because noreturn calls would always
		 be at the end of blocks, and we test for zero
		 successors because if there is an edge, it's not
		 noreturn, as any EH edges would have already been
		 caught by the lookup_stmt_eh_lp test above.  */
	      else if (check_before_noreturn_calls
		       && EDGE_COUNT (bb->succs) == 0
		       && (is_a <gresx *> (stmt)
			   ? check_before_always_throwing_noreturn_calls
			   : (!is_a <gcall *> (stmt)
			      || !gimple_call_noreturn_p (stmt))
			   ? (gcc_unreachable (), false)
			   : (!flag_exceptions
			      || gimple_call_nothrow_p (as_a <gcall *> (stmt)))
			   ? check_before_nothrow_noreturn_calls
			   : always_throwing_noreturn_call_p (stmt)
			   ? check_before_always_throwing_noreturn_calls
			   : check_before_throwing_noreturn_calls))
		{
		  if (dump_file)
		    {
		      fprintf (dump_file,
			       "Bypassing cleanup for noreturn stmt"
			       " in block %i:\n",
			       bb->index);
		      print_gimple_stmt (dump_file, stmt, 0);
		    }
		  continue;
		}

	      if (!bb_eh_cleanup)
		{
		  bb_eh_cleanup = create_empty_bb (bb);
		  if (dom_info_available_p (CDI_DOMINATORS))
		    set_immediate_dominator (CDI_DOMINATORS, bb_eh_cleanup, bb);
		  if (current_loops)
		    add_bb_to_loop (bb_eh_cleanup, current_loops->tree_root);

		  /* Make the new block an EH cleanup for the call.  */
		  eh_region new_r = gen_eh_region_cleanup (NULL);
		  eh_landing_pad lp = gen_eh_landing_pad (new_r);
		  tree label = gimple_block_label (bb_eh_cleanup);
		  lp->post_landing_pad = label;
		  EH_LANDING_PAD_NR (label) = lp_eh_cleanup = lp->index;

		  /* Just propagate the exception.
		     We will later insert the verifier call.  */
		  gimple_stmt_iterator ehgsi;
		  ehgsi = gsi_after_labels (bb_eh_cleanup);
		  gresx *resx = gimple_build_resx (new_r->index);
		  gsi_insert_before (&ehgsi, resx, GSI_SAME_STMT);

		  if (dump_file)
		    fprintf (dump_file,
			     "Created cleanup block %i:\n",
			     bb_eh_cleanup->index);
		}
	      else if (dom_info_available_p (CDI_DOMINATORS))
		{
		  basic_block immdom;
		  immdom = get_immediate_dominator (CDI_DOMINATORS,
						    bb_eh_cleanup);
		  if (!dominated_by_p (CDI_DOMINATORS, bb, immdom))
		    {
		      immdom = nearest_common_dominator (CDI_DOMINATORS,
							 immdom, bb);
		      set_immediate_dominator (CDI_DOMINATORS,
					       bb_eh_cleanup, immdom);
		    }
		}

	      if (dump_file)
		{
		  fprintf (dump_file,
			   "Associated cleanup block with stmt in block %i:\n",
			   bb->index);
		  print_gimple_stmt (dump_file, stmt, 0);
		}

	      add_stmt_to_eh_lp (stmt, lp_eh_cleanup);
	      /* Finally, wire the EH cleanup block into the CFG.  */
	      edge neeh = make_eh_edge (stmt);
	      neeh->probability = profile_probability::never ();
	      gcc_checking_assert (neeh->dest == bb_eh_cleanup);
	      if (neeh->dest->count.initialized_p ())
		neeh->dest->count += neeh->count ();
	      else
		neeh->dest->count = neeh->count ();
	    }
	}

      if (bb_eh_cleanup)
	{
	  /* A cfg_cleanup after bb_eh_cleanup makes for a more compact
	     rtcfg, and it avoids bb numbering differences when we split
	     blocks because of trailing debug insns only.  */
	  cleanup_tree_cfg ();
	  gcc_checking_assert (EDGE_COUNT (bb_eh_cleanup->succs) == 0);
	}
    }

  /* These record blocks with calls that are to be preceded by
     checkpoints, such as noreturn calls (if so chosen), must-tail
     calls, potential early-marked tail calls, and returning calls (if
     so chosen).  */
  int count_chkcall = 0;
  auto_sbitmap chkcall_blocks (last_basic_block_for_fn (fun));
  bitmap_clear (chkcall_blocks);

  /* We wish to add verification at blocks without successors, such as
     noreturn calls (raising or not) and the reraise at the cleanup
     block, but not other reraises: they will go through the cleanup
     block.  */
  if (check_before_noreturn_calls)
    FOR_EACH_BB_FN (bb, fun)
      {
	gimple_stmt_iterator gsi = gsi_last_bb (bb);
	if (gsi_end_p (gsi))
	  continue;
	gimple *stmt = gsi_stmt (gsi);

	if (EDGE_COUNT (bb->succs) == 0)
	  {
	    /* A stmt at the end of a block without any successors is
	       either a resx or a noreturn call without a local
	       handler.  Check that it's one of the desired
	       checkpoints.  */
	    if (flag_exceptions && is_a <gresx *> (stmt)
		? (check_before_always_throwing_noreturn_calls
		   || bb == bb_eh_cleanup)
		: (!is_a <gcall *> (stmt)
		   || !gimple_call_noreturn_p (stmt))
		? (stmt_can_make_abnormal_goto (stmt)
		   /* ??? Check before indirect nonlocal goto, or
		      calls thereof?  */
		   ? false
		   /* Catch cases in which successors would be
		      expected.  */
		   : (gcc_unreachable (), false))
		: (!flag_exceptions
		   || gimple_call_nothrow_p (as_a <gcall *> (stmt)))
		? check_before_nothrow_noreturn_calls
		: always_throwing_noreturn_call_p (stmt)
		? check_before_always_throwing_noreturn_calls
		: check_before_throwing_noreturn_calls)
	      {
		if (dump_file)
		  {
		    fprintf (dump_file,
			     "Scheduling check before stmt"
			     " in succ-less block %i:\n",
			     bb->index);
		    print_gimple_stmt (dump_file, stmt, 0);
		  }

		if (bitmap_set_bit (chkcall_blocks, bb->index))
		  count_chkcall++;
		else
		  gcc_unreachable ();
	      }
	    continue;
	  }

	/* If there are no exceptions, it would seem like any noreturn
	   call must have zero successor edges, but __builtin_return
	   gets successor edges.  We don't want to handle it here, it
	   will be dealt with in sibcall_search_preds.  Otherwise,
	   check for blocks without non-EH successors, but skip those
	   with resx stmts and edges (i.e., those other than that in
	   bb_eh_cleanup), since those will go through bb_eh_cleanup,
	   that will have been counted as noreturn above because it
	   has no successors.  */
	gcc_checking_assert (bb != bb_eh_cleanup
			     || !check_at_escaping_exceptions);
	if (flag_exceptions && is_a <gresx *> (stmt)
	    ? check_before_always_throwing_noreturn_calls
	    : (!is_a <gcall *> (stmt)
	       || !gimple_call_noreturn_p (stmt))
	    ? false
	    : (!flag_exceptions
	       || gimple_call_nothrow_p (as_a <gcall *> (stmt)))
	    ? false /* rather than check_before_nothrow_noreturn_calls */
	    : always_throwing_noreturn_call_p (stmt)
	    ? check_before_always_throwing_noreturn_calls
	    : check_before_throwing_noreturn_calls)
	  {
	    gcc_checking_assert (single_succ_p (bb)
				 && (single_succ_edge (bb)->flags & EDGE_EH));

	    if (dump_file)
	      {
		fprintf (dump_file,
			 "Scheduling check before stmt"
			 " in EH-succ block %i:\n",
			 bb->index);
		print_gimple_stmt (dump_file, stmt, 0);
	      }

	    if (bitmap_set_bit (chkcall_blocks, bb->index))
	      count_chkcall++;
	    else
	      gcc_unreachable ();
	  }
      }
  else if (bb_eh_cleanup)
    {
      if (bitmap_set_bit (chkcall_blocks, bb_eh_cleanup->index))
	count_chkcall++;
      else
	gcc_unreachable ();
    }

  gcc_checking_assert (!bb_eh_cleanup
		       || bitmap_bit_p (chkcall_blocks, bb_eh_cleanup->index));

  /* If we don't have edges to exit nor noreturn calls (including the
     cleanup reraise), then we may skip instrumentation: that would
     amount to a function that ends with an infinite loop.  */
  if (!count_chkcall
      && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (fun)->preds) == 0)
    {
      if (dump_file)
	fprintf (dump_file,
		 "Disabling CFR, no exit paths to check\n");

      return 0;
    }

  /* Search for must-tail calls, early-marked potential tail calls,
     and, if requested, returning calls.  As we introduce early
     checks, */
  int count_postchk = 0;
  auto_sbitmap postchk_blocks (last_basic_block_for_fn (fun));
  bitmap_clear (postchk_blocks);
  chk_edges_t chk_edges;
  hardcfr_sibcall_search_preds (EXIT_BLOCK_PTR_FOR_FN (fun), chk_edges,
				count_chkcall, chkcall_blocks,
				count_postchk, postchk_blocks,
				NULL);

  rt_bb_visited vstd (chk_edges.length () + count_chkcall);

  auto_sbitmap combined_blocks (last_basic_block_for_fn (fun));
  bitmap_copy (combined_blocks, chkcall_blocks);
  int i;
  edge *e;
  FOR_EACH_VEC_ELT (chk_edges, i, e)
    if (!bitmap_set_bit (combined_blocks, (*e)->src->index))
      /* There may be multiple chk_edges with the same src block;
	 guard againt overlaps with chkcall_blocks only.  */
      gcc_assert (!bitmap_bit_p (chkcall_blocks, (*e)->src->index));

  /* Visit blocks in index order, because building rtcfg depends on
     that.  Blocks must be compact, which the cleanup_cfg requirement
     ensures.  This would also enable FOR_EACH_BB_FN to be used to
     iterate in index order, but bb_eh_cleanup block splits and
     insertions changes that.  */
  gcc_checking_assert (n_basic_blocks_for_fn (fun)
		       == last_basic_block_for_fn (fun));
  for (int i = NUM_FIXED_BLOCKS; i < n_basic_blocks_for_fn (fun); i++)
    {
      bb = BASIC_BLOCK_FOR_FN (fun, i);
      gcc_checking_assert (bb->index == i);
      vstd.visit (bb, bitmap_bit_p (combined_blocks, i),
		  bitmap_bit_p (postchk_blocks, i));
    }

  vstd.check (chk_edges, count_chkcall, chkcall_blocks);

  return
    TODO_update_ssa
    | TODO_cleanup_cfg
    | TODO_verify_il;
}

/* Instantiate a hardcfr pass.  */

gimple_opt_pass *
make_pass_harden_control_flow_redundancy (gcc::context *ctxt)
{
  return new pass_harden_control_flow_redundancy (ctxt);
}
