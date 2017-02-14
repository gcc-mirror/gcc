/* Unit tests for function-handling.
   Copyright (C) 2015-2017 Free Software Foundation, Inc.

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
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "opts.h"
#include "signop.h"
#include "hash-set.h"
#include "fixed-value.h"
#include "alias.h"
#include "flags.h"
#include "symtab.h"
#include "tree-core.h"
#include "stor-layout.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "rtl.h"
#include "predict.h"
#include "vec.h"
#include "hashtab.h"
#include "hash-set.h"
#include "machmode.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "cfganal.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "gimple-expr.h"
#include "toplev.h"
#include "print-tree.h"
#include "tree-iterator.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "basic-block.h"
#include "double-int.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "stmt.h"
#include "hash-table.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "tree-pass.h"
#include "context.h"
#include "hash-map.h"
#include "plugin-api.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "selftest.h"
#include "print-rtl.h"

#if CHECKING_P

namespace selftest {

/* Helper function for selftests of function-creation.  */

static tree
make_fndecl (tree return_type,
	     const char *name,
	     vec <tree> &param_types,
	     bool is_variadic = false)
{
  tree fn_type;
  if (is_variadic)
    fn_type = build_varargs_function_type_array (return_type,
						 param_types.length (),
						 param_types.address ());
  else
    fn_type = build_function_type_array (return_type,
					 param_types.length (),
					 param_types.address ());
  /* FIXME: this uses input_location: */
  tree fndecl = build_fn_decl (name, fn_type);

  return fndecl;
}

/* Verify creating a function declaration equivalent to the following
     int test_fndecl_int_void (void);
   C declaration.  */

static void
test_fndecl_int_void ()
{
  auto_vec <tree> param_types;
  const char *name = "test_fndecl_int_void";
  tree fndecl = make_fndecl (integer_type_node,
			     name,
			     param_types);
  ASSERT_TRUE (fndecl != NULL);

  /* Verify name of decl.  */
  tree declname = DECL_NAME (fndecl);
  ASSERT_TRUE (declname != NULL);
  ASSERT_EQ (IDENTIFIER_NODE, TREE_CODE (declname));
  /* We expect it to use a *copy* of the string we passed in.  */
  const char *identifier_ptr = IDENTIFIER_POINTER (declname);
  ASSERT_NE (name, identifier_ptr);
  ASSERT_EQ (0, strcmp ("test_fndecl_int_void", identifier_ptr));

  /* Verify type of fndecl.  */
  ASSERT_EQ (FUNCTION_DECL, TREE_CODE (fndecl));
  tree fntype = TREE_TYPE (fndecl);
  ASSERT_EQ (FUNCTION_TYPE, TREE_CODE (fntype));

  /* Verify return type.  */
  ASSERT_EQ (integer_type_node, TREE_TYPE (fntype));

  /* Verify "void" args.  */
  tree argtypes = TYPE_ARG_TYPES (fntype);
  ASSERT_EQ (TREE_LIST, TREE_CODE (argtypes));
  ASSERT_EQ (void_type_node, TREE_VALUE (argtypes));
  ASSERT_EQ (NULL, TREE_CHAIN (argtypes));
}

/* Verify creating a function declaration equivalent to the following
     float test_fndecl_float_intchar (int, char);
   C declaration.  */

static void
test_fndecl_float_intchar ()
{
  auto_vec <tree> param_types;
  param_types.safe_push (integer_type_node);
  param_types.safe_push (char_type_node);
  const char *name = "test_fndecl_float_intchar";
  tree fndecl = make_fndecl (float_type_node,
			     name,
			     param_types);
  ASSERT_TRUE (fndecl != NULL);

  /* Verify name of decl.  */
  tree declname = DECL_NAME (fndecl);
  ASSERT_TRUE (declname != NULL);
  ASSERT_EQ (IDENTIFIER_NODE, TREE_CODE (declname));
  /* We expect it to use a *copy* of the string we passed in.  */
  const char *identifier_ptr = IDENTIFIER_POINTER (declname);
  ASSERT_NE (name, identifier_ptr);
  ASSERT_EQ (0, strcmp (name, identifier_ptr));

  /* Verify type of fndecl.  */
  ASSERT_EQ (FUNCTION_DECL, TREE_CODE (fndecl));
  tree fntype = TREE_TYPE (fndecl);
  ASSERT_EQ (FUNCTION_TYPE, TREE_CODE (fntype));

  /* Verify return type.  */
  ASSERT_EQ (float_type_node, TREE_TYPE (fntype));

  /* Verify "(int, char)" args.  */
  tree arg0 = TYPE_ARG_TYPES (fntype);
  ASSERT_EQ (TREE_LIST, TREE_CODE (arg0));
  ASSERT_EQ (integer_type_node, TREE_VALUE (arg0));
  tree arg1 = TREE_CHAIN (arg0);
  ASSERT_TRUE (arg1 != NULL);
  ASSERT_EQ (TREE_LIST, TREE_CODE (arg1));
  ASSERT_EQ (char_type_node, TREE_VALUE (arg1));
  tree argterm = TREE_CHAIN (arg1);
  ASSERT_TRUE (argterm != NULL);
  ASSERT_EQ (TREE_LIST, TREE_CODE (argterm));
  ASSERT_EQ (void_type_node, TREE_VALUE (argterm));
  ASSERT_EQ (NULL, TREE_CHAIN (argterm));
}

/* The test cases using these helper functions take a trivial function:

     int test_fn (void) { return 42; }

   and test various conversions done to it:

   - gimplification
   - construction of the CFG
   - conversion to SSA form
   - expansion to RTL form

   In avoid having one overlong test case, this is broken
   up into separate test cases for each stage, with helper functions
   to minimize code duplication.

   Another approach would be to attempt to directly construct a function
   in the appropriate representation at each stage, though presumably
   that would exhibit different kinds of failure compared to this
   approach.  */

/* Construct this function:
   int test_fn (void) { return 42; }
   in generic tree form.  Return the fndecl.  */

static tree
build_trivial_generic_function ()
{
  auto_vec <tree> param_types;
  tree fndecl = make_fndecl (integer_type_node,
			     "test_fn",
			     param_types);
  ASSERT_TRUE (fndecl != NULL);

  /* Populate the function.  */
  tree retval = build_decl (UNKNOWN_LOCATION, RESULT_DECL,
			    NULL_TREE, integer_type_node);
  DECL_ARTIFICIAL (retval) = 1;
  DECL_IGNORED_P (retval) = 1;
  DECL_RESULT (fndecl) = retval;

  /* Create a BIND_EXPR, and within it, a statement list.  */
  tree stmt_list = alloc_stmt_list ();
  tree_stmt_iterator stmt_iter = tsi_start (stmt_list);
  tree block = make_node (BLOCK);
  tree bind_expr
    = build3 (BIND_EXPR, void_type_node, NULL, stmt_list, block);

  tree modify_retval = build2 (MODIFY_EXPR,
			       integer_type_node,
			       retval,
			       build_int_cst (integer_type_node, 42));
  tree return_stmt = build1 (RETURN_EXPR,
			     integer_type_node,
			     modify_retval);
  tsi_link_after (&stmt_iter, return_stmt, TSI_CONTINUE_LINKING);

  DECL_INITIAL (fndecl) = block;
  BLOCK_SUPERCONTEXT (block) = fndecl;

  /* how to add to function? the following appears to be how to
     set the body of a fndecl: */
  DECL_SAVED_TREE(fndecl) = bind_expr;

  /* Ensure that locals appear in the debuginfo.  */
  BLOCK_VARS (block) = BIND_EXPR_VARS (bind_expr);

  return fndecl;
}

/* Construct this function:
     int test_fn (void) { return 42; }
   in "high gimple" form.  Return the fndecl.  */

static tree
build_trivial_high_gimple_function ()
{
  /* Construct a trivial function, and gimplify it: */
  tree fndecl = build_trivial_generic_function ();
  gimplify_function_tree (fndecl);
  return fndecl;
}

/* Build a CFG for a function in gimple form.  */

static void
build_cfg (tree fndecl)
{
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);
  ASSERT_EQ (fndecl, fun->decl);

  /* We first have to lower control flow; for our trivial test function
     this gives us:
	 test_fn ()
	 {
	   D.56 = 42;
	   goto <D.57>;
	   <D.57>:
	   return D.56;
	 }
  */
  gimple_opt_pass *lower_cf_pass = make_pass_lower_cf (g);
  push_cfun (fun);
  lower_cf_pass->execute (fun);
  pop_cfun ();
  delete lower_cf_pass;

  /* We can now convert to CFG form; for our trivial test function this
     gives us:
	 test_fn ()
	 {
	   <bb 2>:
	   D.56 = 42;
	   return D.56;
	 }
  */
  gimple_opt_pass *build_cfg_pass = make_pass_build_cfg (g);
  push_cfun (fun);
  build_cfg_pass->execute (fun);
  pop_cfun ();
  delete build_cfg_pass;
}

/* Convert a gimple+CFG function to SSA form.  */

static void
convert_to_ssa (tree fndecl)
{
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);
  ASSERT_EQ (fndecl, fun->decl);

  gimple_opt_pass *build_ssa_pass = make_pass_build_ssa (g);
  push_cfun (fun);
  build_ssa_pass->execute (fun);
  pop_cfun ();
  delete build_ssa_pass;
}

/* Assuming we have a simple 3-block CFG like this:
     [ENTRY] -> [block2] -> [EXIT]
   get the "real" basic block (block 2).  */

static basic_block
get_real_block (function *fun)
{
  ASSERT_TRUE (fun->cfg != NULL);
  ASSERT_EQ (3, n_basic_blocks_for_fn (fun));
  basic_block bb2 = (*fun->cfg->x_basic_block_info)[2];
  ASSERT_TRUE (bb2 != NULL);
  return bb2;
}

/* Verify that we have a simple 3-block CFG: the two "fake" ones, and
   a "real" one:
     [ENTRY] -> [block2] -> [EXIT].  */

static void
verify_three_block_cfg (function *fun)
{
  ASSERT_TRUE (fun->cfg != NULL);
  ASSERT_EQ (3, n_basic_blocks_for_fn (fun));
  ASSERT_EQ (2, n_edges_for_fn (fun));

  /* The "fake" basic blocks.  */
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (fun);
  ASSERT_TRUE (entry != NULL);
  ASSERT_EQ (ENTRY_BLOCK, entry->index);

  basic_block exit = EXIT_BLOCK_PTR_FOR_FN (fun);
  ASSERT_TRUE (exit != NULL);
  ASSERT_EQ (EXIT_BLOCK, exit->index);

  /* The "real" basic block.  */
  basic_block bb2 = get_real_block (fun);
  ASSERT_TRUE (bb2 != NULL);
  ASSERT_EQ (2, bb2->index);

  /* Verify connectivity.  */
  ASSERT_EQ (NULL, entry->preds);
  ASSERT_EQ (1, entry->succs->length ());

  edge from_entry_to_bb2 = (*entry->succs)[0];
  ASSERT_EQ (entry, from_entry_to_bb2->src);
  ASSERT_EQ (bb2, from_entry_to_bb2->dest);

  ASSERT_EQ (1, bb2->preds->length ());
  ASSERT_EQ (from_entry_to_bb2, (*bb2->preds)[0]);
  ASSERT_EQ (1, bb2->succs->length ());

  edge from_bb2_to_exit = (*bb2->succs)[0];
  ASSERT_EQ (bb2, from_bb2_to_exit->src);
  ASSERT_EQ (exit, from_bb2_to_exit->dest);

  ASSERT_EQ (1, exit->preds->length ());
  ASSERT_EQ (from_bb2_to_exit, (*exit->preds)[0]);
  ASSERT_EQ (NULL, exit->succs);
}

/* As above, but additionally verify the gimple statements are sane.  */

static void
verify_three_block_gimple_cfg (function *fun)
{
  verify_three_block_cfg (fun);

  /* The "fake" basic blocks should be flagged as gimple, but with have no
     statements.  */
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (fun);
  ASSERT_TRUE (entry != NULL);
  ASSERT_EQ (0, entry->flags & BB_RTL);
  ASSERT_EQ (NULL, bb_seq (entry));

  basic_block exit = EXIT_BLOCK_PTR_FOR_FN (fun);
  ASSERT_TRUE (exit != NULL);
  ASSERT_EQ (0, entry->flags & BB_RTL);
  ASSERT_EQ (NULL, bb_seq (exit));

  /* The "real" basic block should be flagged as gimple, and have one
     or more statements.  */
  basic_block bb2 = get_real_block (fun);
  ASSERT_TRUE (bb2 != NULL);
  ASSERT_EQ (0, entry->flags & BB_RTL);
  ASSERT_TRUE (bb_seq (bb2) != NULL);
}

/* As above, but additionally verify the RTL insns are sane.  */

void
verify_three_block_rtl_cfg (function *fun)
{
  verify_three_block_cfg (fun);

  /* The "fake" basic blocks should be flagged as RTL, but with no
     insns.  */
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (fun);
  ASSERT_TRUE (entry != NULL);
  ASSERT_EQ (BB_RTL, entry->flags & BB_RTL);
  ASSERT_EQ (NULL, BB_HEAD (entry));

  basic_block exit = EXIT_BLOCK_PTR_FOR_FN (fun);
  ASSERT_TRUE (exit != NULL);
  ASSERT_EQ (BB_RTL, exit->flags & BB_RTL);
  ASSERT_EQ (NULL, BB_HEAD (exit));

  /* The "real" basic block should be flagged as RTL, and have one
     or more insns.  */
  basic_block bb2 = get_real_block (fun);
  ASSERT_TRUE (bb2 != NULL);
  ASSERT_EQ (BB_RTL, bb2->flags & BB_RTL);
  ASSERT_TRUE (BB_HEAD (bb2) != NULL);
}

/* Test converting our trivial function:
     int test_fn (void) { return 42; }
   to gimple form.  */

static void
test_gimplification ()
{
  tree fndecl = build_trivial_generic_function ();

  /* Convert to gimple: */
  gimplify_function_tree (fndecl);

  /* Verify that we got gimple out of it.  */

  /* The function is now in GIMPLE form but the CFG has not been
     built yet.  */

  /* We should have a struct function for the decl.  */
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);
  ASSERT_EQ (fndecl, fun->decl);

  /* We expect a GIMPLE_BIND, with two gimple statements within it:
       tmp = 42;
       return tmp;  */

  gimple_seq seq_fn_body = gimple_body (fndecl);
  ASSERT_TRUE (seq_fn_body != NULL);
  gimple *bind_stmt = gimple_seq_first_stmt (seq_fn_body);
  ASSERT_EQ (GIMPLE_BIND, gimple_code (bind_stmt));
  ASSERT_EQ (NULL, bind_stmt->next);

  gimple_seq seq_bind_body = gimple_bind_body (as_a <gbind *> (bind_stmt));

  /* Verify that we have the 2 statements we expect.  */
  ASSERT_TRUE (seq_bind_body != NULL);
  gimple *stmt1 = gimple_seq_first_stmt (seq_bind_body);
  ASSERT_TRUE (stmt1 != NULL);
  ASSERT_EQ (GIMPLE_ASSIGN, gimple_code (stmt1));
  gimple *stmt2 = stmt1->next;
  ASSERT_TRUE (stmt2 != NULL);
  ASSERT_EQ (stmt1, stmt2->prev);
  ASSERT_EQ (GIMPLE_RETURN, gimple_code (stmt2));
}

/* Test of building a CFG for a function in high gimple form.  */

static void
test_building_cfg ()
{
  /* Construct a trivial function, and gimplify it: */
  tree fndecl = build_trivial_high_gimple_function ();
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);

  /* Build a CFG.  */
  build_cfg (fndecl);

  /* The CFG-building code constructs a 4-block cfg (with
     ENTRY and EXIT):
       test_fn ()
       {
         <bb 2>:
	 D.65 = 42;

	 <bb 3>:
	 return D.65;
       }
     and then ought to merge blocks 2 and 3 in cleanup_tree_cfg.

     Hence we should end up with a simple 3-block cfg, the two "fake" ones,
     and a "real" one:
       [ENTRY] -> [block2] -> [EXIT]
     with code like this:
	 test_fn ()
	 {
	   <bb 2>:
	   D.56 = 42;
	   return D.56;
	 }
  */
  verify_three_block_gimple_cfg (fun);

  /* Verify the statements within the "real" block.  */
  basic_block bb2 = get_real_block (fun);
  gimple *stmt_a = gimple_seq_first_stmt (bb_seq (bb2));
  ASSERT_EQ (GIMPLE_ASSIGN, gimple_code (stmt_a));
  gimple *stmt_b = stmt_a->next;
  ASSERT_EQ (GIMPLE_RETURN, gimple_code (stmt_b));
  ASSERT_EQ (NULL, stmt_b->next);
}

/* Test of conversion of gimple to SSA form.  */

static void
test_conversion_to_ssa ()
{
  /* As above, construct a trivial function, gimplify it, and build a CFG: */
  tree fndecl = build_trivial_high_gimple_function ();
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);
  build_cfg (fndecl);

  convert_to_ssa (fndecl);

  verify_three_block_gimple_cfg (fun);

  /* For out trivial test function we should now have something like
     this:
       test_fn ()
       {
	 <bb 2>:
	 _1 = 42;
	 return _1;
       }
  */
  basic_block bb2 = get_real_block (fun);
  gimple *stmt_a = gimple_seq_first_stmt (bb_seq (bb2));
  ASSERT_EQ (GIMPLE_ASSIGN, gimple_code (stmt_a));

  gimple *stmt_b = stmt_a->next;
  ASSERT_EQ (GIMPLE_RETURN, gimple_code (stmt_b));
  ASSERT_EQ (NULL, stmt_b->next);

  greturn *return_stmt = as_a <greturn *> (stmt_b);
  ASSERT_EQ (SSA_NAME, TREE_CODE (gimple_return_retval (return_stmt)));
}

/* Test of expansion from gimple-ssa to RTL.  */

static void
test_expansion_to_rtl ()
{
  /* As above, construct a trivial function, gimplify it, build a CFG,
     and convert to SSA: */
  tree fndecl = build_trivial_high_gimple_function ();
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);
  build_cfg (fndecl);
  convert_to_ssa (fndecl);

  /* We need a cgraph_node for it.  */
  cgraph_node::get_create (fndecl);
  /* Normally, cgraph_node::expand () would call
     init_function_start (and a bunch of other stuff),
     and invoke the expand pass, but it also runs
     all of the other passes.  So just do the minimum
     needed to get from gimple-SSA to RTL.  */
  rtl_opt_pass *expand_pass = make_pass_expand (g);
  push_cfun (fun);
  init_function_start (fndecl);
  expand_pass->execute (fun);
  pop_cfun ();
  delete expand_pass;

  /* On x86_64, I get this:
       (note 3 1 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
       (note 2 3 5 2 NOTE_INSN_FUNCTION_BEG)
       (insn 5 2 6 2 (set (reg:SI 87 [ D.59 ])
			  (const_int 42 [0x2a])) -1 (nil))
       (insn 6 5 10 2 (set (reg:SI 88 [ <retval> ])
			   (reg:SI 87 [ D.59 ])) -1 (nil))
       (insn 10 6 11 2 (set (reg/i:SI 0 ax)
			    (reg:SI 88 [ <retval> ])) -1 (nil))
       (insn 11 10 0 2 (use (reg/i:SI 0 ax)) -1 (nil))

     On cr16-elf I get this:
       (note 4 1 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
       (insn 2 4 3 2 (set (reg:SI 24)
	    (reg/f:SI 16 virtual-incoming-args)) -1
	  (nil))
       (note 3 2 6 2 NOTE_INSN_FUNCTION_BEG)
       (insn 6 3 7 2 (set (reg:HI 22 [ _1 ])
	    (const_int 42 [0x2a])) -1
	 (nil))
       (insn 7 6 11 2 (set (reg:HI 23 [ <retval> ])
	   (reg:HI 22 [ _1 ])) -1
	 (nil))
       (insn 11 7 12 2 (set (reg/i:HI 0 r0)
	   (reg:HI 23 [ <retval> ])) -1
	 (nil))
       (insn 12 11 0 2 (use (reg/i:HI 0 r0)) -1
	 (nil)).  */
  verify_three_block_rtl_cfg (fun);

  /* Verify as much of the RTL as we can whilst avoiding
     target-specific behavior.  */
  basic_block bb2 = get_real_block (fun);

  /* Expect a NOTE_INSN_BASIC_BLOCK... */
  rtx_insn *insn = BB_HEAD (bb2);
  ASSERT_TRUE (insn != NULL);
  ASSERT_EQ (NOTE, insn->code);
  ASSERT_EQ (NOTE_INSN_BASIC_BLOCK, NOTE_KIND (insn));
  ASSERT_EQ (bb2, NOTE_BASIC_BLOCK (insn));

  /* ...etc; any further checks are likely to over-specify things
     and run us into target dependencies.  */

  /* Verify that print_rtl_function is sane.  */
  named_temp_file tmp_out (".rtl");
  FILE *outfile = fopen (tmp_out.get_filename (), "w");
  print_rtx_function (outfile, fun, true);
  fclose (outfile);

  char *dump = read_file (SELFTEST_LOCATION, tmp_out.get_filename ());
  ASSERT_STR_CONTAINS (dump, "(function \"test_fn\"\n");
  ASSERT_STR_CONTAINS (dump, "  (insn-chain\n");
  ASSERT_STR_CONTAINS (dump, "    (block 2\n");
  ASSERT_STR_CONTAINS (dump, "      (edge-from entry (flags \"FALLTHRU\"))\n");
  ASSERT_STR_CONTAINS (dump, "      (cinsn "); /* ...etc.  */
  ASSERT_STR_CONTAINS (dump, "      (edge-to exit (flags \"FALLTHRU\"))\n");
  ASSERT_STR_CONTAINS (dump, "    ) ;; block 2\n");
  ASSERT_STR_CONTAINS (dump, "  ) ;; insn-chain\n");
  ASSERT_STR_CONTAINS (dump, "  (crtl\n");
  ASSERT_STR_CONTAINS (dump, "  ) ;; crtl\n");
  ASSERT_STR_CONTAINS (dump, ") ;; function \"test_fn\"\n");

  free (dump);
}

/* Run all of the selftests within this file.  */

void
function_tests_c_tests ()
{
  test_fndecl_int_void ();
  test_fndecl_float_intchar ();
  test_gimplification ();
  test_building_cfg ();
  test_conversion_to_ssa ();
  test_expansion_to_rtl ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
