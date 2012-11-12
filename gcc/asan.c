/* AddressSanitizer, a fast memory error detector.
   Copyright (C) 2011 Free Software Foundation, Inc.
   Contributed by Kostya Serebryany <kcc@google.com>

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
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "flags.h"
#include "function.h"
#include "tree-inline.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "diagnostic.h"
#include "demangle.h"
#include "langhooks.h"
#include "ggc.h"
#include "cgraph.h"
#include "gimple.h"
#include "asan.h"
#include "gimple-pretty-print.h"

/*
 AddressSanitizer finds out-of-bounds and use-after-free bugs 
 with <2x slowdown on average.

 The tool consists of two parts:
 instrumentation module (this file) and a run-time library.
 The instrumentation module adds a run-time check before every memory insn.
   For a 8- or 16- byte load accessing address X:
     ShadowAddr = (X >> 3) + Offset
     ShadowValue = *(char*)ShadowAddr;  // *(short*) for 16-byte access.
     if (ShadowValue)
       __asan_report_load8(X);
   For a load of N bytes (N=1, 2 or 4) from address X:
     ShadowAddr = (X >> 3) + Offset
     ShadowValue = *(char*)ShadowAddr;
     if (ShadowValue)
       if ((X & 7) + N - 1 > ShadowValue)
         __asan_report_loadN(X);
 Stores are instrumented similarly, but using __asan_report_storeN functions.
 A call too __asan_init() is inserted to the list of module CTORs.

 The run-time library redefines malloc (so that redzone are inserted around
 the allocated memory) and free (so that reuse of free-ed memory is delayed),
 provides __asan_report* and __asan_init functions.

 Read more:
 http://code.google.com/p/address-sanitizer/wiki/AddressSanitizerAlgorithm

 Future work:
 The current implementation supports only detection of out-of-bounds and
 use-after-free bugs in heap.
 In order to support out-of-bounds for stack and globals we will need
 to create redzones for stack and global object and poison them.
*/

/* The shadow address is computed as (X>>asan_scale) + (1<<asan_offset_log).
 We may want to add command line flags to change these values.  */

static const int asan_scale = 3;
static const int asan_offset_log_32 = 29;
static const int asan_offset_log_64 = 44;
static int asan_offset_log;


/* Construct a function tree for __asan_report_{load,store}{1,2,4,8,16}.
   IS_STORE is either 1 (for a store) or 0 (for a load).
   SIZE_IN_BYTES is one of 1, 2, 4, 8, 16.  */

static tree
report_error_func (int is_store, int size_in_bytes)
{
  tree fn_type;
  tree def;
  char name[100];

  sprintf (name, "__asan_report_%s%d\n",
           is_store ? "store" : "load", size_in_bytes);
  fn_type = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  def = build_fn_decl (name, fn_type);
  TREE_NOTHROW (def) = 1;
  TREE_THIS_VOLATILE (def) = 1;  /* Attribute noreturn. Surprise!  */
  DECL_ATTRIBUTES (def) = tree_cons (get_identifier ("leaf"), 
                                     NULL, DECL_ATTRIBUTES (def));
  DECL_ASSEMBLER_NAME (def);
  return def;
}

/* Construct a function tree for __asan_init().  */

static tree
asan_init_func (void)
{
  tree fn_type;
  tree def;

  fn_type = build_function_type_list (void_type_node, NULL_TREE);
  def = build_fn_decl ("__asan_init", fn_type);
  TREE_NOTHROW (def) = 1;
  DECL_ASSEMBLER_NAME (def);
  return def;
}


/* Instrument the memory access instruction BASE.
   Insert new statements before ITER.
   LOCATION is source code location.
   IS_STORE is either 1 (for a store) or 0 (for a load).
   SIZE_IN_BYTES is one of 1, 2, 4, 8, 16.  */

static void
build_check_stmt (tree base,
                  gimple_stmt_iterator *iter,
                  location_t location, int is_store, int size_in_bytes)
{
  gimple_stmt_iterator gsi;
  basic_block cond_bb, then_bb, join_bb;
  edge e;
  tree cond, t, u;
  tree base_addr;
  tree shadow_value;
  gimple g;
  gimple_seq seq, stmts;
  tree shadow_type = size_in_bytes == 16 ?
      short_integer_type_node : char_type_node;
  tree shadow_ptr_type = build_pointer_type (shadow_type);
  tree uintptr_type = lang_hooks.types.type_for_mode (ptr_mode,
                                                      /*unsignedp=*/true);

  /* We first need to split the current basic block, and start altering
     the CFG.  This allows us to insert the statements we're about to
     construct into the right basic blocks.  */

  cond_bb = gimple_bb (gsi_stmt (*iter));
  gsi = *iter;
  gsi_prev (&gsi);
  if (!gsi_end_p (gsi))
    e = split_block (cond_bb, gsi_stmt (gsi));
  else
    e = split_block_after_labels (cond_bb);
  cond_bb = e->src;
  join_bb = e->dest;

  /* A recap at this point: join_bb is the basic block at whose head
     is the gimple statement for which this check expression is being
     built.  cond_bb is the (possibly new, synthetic) basic block the
     end of which will contain the cache-lookup code, and a
     conditional that jumps to the cache-miss code or, much more
     likely, over to join_bb.  */

  /* Create the bb that contains the crash block.  */
  then_bb = create_empty_bb (cond_bb);
  make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  make_single_succ_edge (then_bb, join_bb, EDGE_FALLTHRU);

  /* Mark the pseudo-fallthrough edge from cond_bb to join_bb.  */
  e = find_edge (cond_bb, join_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->count = cond_bb->count;
  e->probability = REG_BR_PROB_BASE;

  /* Update dominance info.  Note that bb_join's data was
     updated by split_block.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);
      set_immediate_dominator (CDI_DOMINATORS, join_bb, cond_bb);
    }

  base_addr = create_tmp_reg (uintptr_type, "__asan_base_addr");

  seq = NULL; 
  t = fold_convert_loc (location, uintptr_type,
                        unshare_expr (base));
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  g = gimple_build_assign (base_addr, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* Build (base_addr >> asan_scale) + (1 << asan_offset_log).  */

  t = build2 (RSHIFT_EXPR, uintptr_type, base_addr,
              build_int_cst (uintptr_type, asan_scale));
  t = build2 (PLUS_EXPR, uintptr_type, t,
              build2 (LSHIFT_EXPR, uintptr_type,
                      build_int_cst (uintptr_type, 1),
                      build_int_cst (uintptr_type, asan_offset_log)
                     ));
  t = build1 (INDIRECT_REF, shadow_type,
              build1 (VIEW_CONVERT_EXPR, shadow_ptr_type, t));
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  shadow_value = create_tmp_reg (shadow_type, "__asan_shadow");
  g = gimple_build_assign (shadow_value, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);
  t = build2 (NE_EXPR, boolean_type_node, shadow_value,
              build_int_cst (shadow_type, 0));
  if (size_in_bytes < 8)
    {

      /* Slow path for 1-, 2- and 4- byte accesses.
         Build ((base_addr & 7) + (size_in_bytes - 1)) >= shadow_value.  */

      u = build2 (BIT_AND_EXPR, uintptr_type,
                  base_addr,
                  build_int_cst (uintptr_type, 7));
      u = build1 (CONVERT_EXPR, shadow_type, u);
      u = build2 (PLUS_EXPR, shadow_type, u,
                  build_int_cst (shadow_type, size_in_bytes - 1));
      u = build2 (GE_EXPR, uintptr_type, u, shadow_value);
    }
  else
      u = build_int_cst (boolean_type_node, 1);
  t = build2 (TRUTH_AND_EXPR, boolean_type_node, t, u);
  t = force_gimple_operand (t, &stmts, false, NULL_TREE);
  gimple_seq_add_seq (&seq, stmts);
  cond = create_tmp_reg (boolean_type_node, "__asan_crash_cond");
  g = gimple_build_assign  (cond, t);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);
  g = gimple_build_cond (NE_EXPR, cond, boolean_false_node, NULL_TREE,
                         NULL_TREE);
  gimple_set_location (g, location);
  gimple_seq_add_stmt (&seq, g);

  /* Generate call to the run-time library (e.g. __asan_report_load8).  */

  gsi = gsi_last_bb (cond_bb);
  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);
  seq = NULL; 
  g = gimple_build_call (report_error_func (is_store, size_in_bytes),
                         1, base_addr);
  gimple_seq_add_stmt (&seq, g);

  /* Insert the check code in the THEN block.  */

  gsi = gsi_start_bb (then_bb);
  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);

  *iter = gsi_start_bb (join_bb);
}

/* If T represents a memory access, add instrumentation code before ITER.
   LOCATION is source code location.
   IS_STORE is either 1 (for a store) or 0 (for a load).  */

static void
instrument_derefs (gimple_stmt_iterator *iter, tree t,
                  location_t location, int is_store)
{
  tree type, base;
  int size_in_bytes;

  type = TREE_TYPE (t);
  if (type == error_mark_node)
    return;
  switch (TREE_CODE (t))
    {
    case ARRAY_REF:
    case COMPONENT_REF:
    case INDIRECT_REF:
    case MEM_REF:
      break;
    default:
      return;
    }
  size_in_bytes = tree_low_cst (TYPE_SIZE (type), 0) / BITS_PER_UNIT;
  if (size_in_bytes != 1 && size_in_bytes != 2 &&
      size_in_bytes != 4 && size_in_bytes != 8 && size_in_bytes != 16)
      return;
  {
    /* For now just avoid instrumenting bit field acceses.
     Fixing it is doable, but expected to be messy.  */

    HOST_WIDE_INT bitsize, bitpos;
    tree offset;
    enum machine_mode mode;
    int volatilep = 0, unsignedp = 0;
    get_inner_reference (t, &bitsize, &bitpos, &offset,
                         &mode, &unsignedp, &volatilep, false);
    if (bitpos != 0 || bitsize != size_in_bytes * BITS_PER_UNIT)
        return;
  }

  base = build_addr (t, current_function_decl);
  build_check_stmt (base, iter, location, is_store, size_in_bytes);
}

/* asan: this looks too complex. Can this be done simpler? */
/* Transform
   1) Memory references.
   2) BUILTIN_ALLOCA calls.
*/

static void
transform_statements (void)
{
  basic_block bb;
  gimple_stmt_iterator i;
  int saved_last_basic_block = last_basic_block;
  enum gimple_rhs_class grhs_class;

  FOR_EACH_BB (bb)
    {
      if (bb->index >= saved_last_basic_block) continue;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
          gimple s = gsi_stmt (i);
          if (gimple_code (s) != GIMPLE_ASSIGN)
              continue;
          instrument_derefs (&i, gimple_assign_lhs (s),
                             gimple_location (s), 1);
          instrument_derefs (&i, gimple_assign_rhs1 (s),
                             gimple_location (s), 0);
          grhs_class = get_gimple_rhs_class (gimple_assign_rhs_code (s));
          if (grhs_class == GIMPLE_BINARY_RHS)
            instrument_derefs (&i, gimple_assign_rhs2 (s),
                               gimple_location (s), 0);
        }
    }
}

/* Module-level instrumentation.
   - Insert __asan_init() into the list of CTORs.
   - TODO: insert redzones around globals.
 */

void
asan_finish_file (void)
{
  tree ctor_statements = NULL_TREE;
  append_to_statement_list (build_call_expr (asan_init_func (), 0),
                            &ctor_statements);
  cgraph_build_static_cdtor ('I', ctor_statements,
                             MAX_RESERVED_INIT_PRIORITY - 1);
}

/* Instrument the current function.  */

static unsigned int
asan_instrument (void)
{
  struct gimplify_ctx gctx;
  tree uintptr_type = lang_hooks.types.type_for_mode (ptr_mode, true);
  int is_64 = tree_low_cst (TYPE_SIZE (uintptr_type), 0) == 64;
  asan_offset_log = is_64 ? asan_offset_log_64 : asan_offset_log_32;
  push_gimplify_context (&gctx);
  transform_statements ();
  pop_gimplify_context (NULL);
  return 0;
}

static bool
gate_asan (void)
{
  return flag_asan != 0;
}

struct gimple_opt_pass pass_asan =
{
 {
  GIMPLE_PASS,
  "asan",                               /* name  */
  OPTGROUP_NONE,                        /* optinfo_flags */
  gate_asan,                            /* gate  */
  asan_instrument,                      /* execute  */
  NULL,                                 /* sub  */
  NULL,                                 /* next  */
  0,                                    /* static_pass_number  */
  TV_NONE,                              /* tv_id  */
  PROP_ssa | PROP_cfg | PROP_gimple_leh,/* properties_required  */
  0,                                    /* properties_provided  */
  0,                                    /* properties_destroyed  */
  0,                                    /* todo_flags_start  */
  TODO_verify_flow | TODO_verify_stmts
  | TODO_update_ssa    /* todo_flags_finish  */
 }
};
