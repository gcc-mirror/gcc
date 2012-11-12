/* AddressSanitizer, a fast memory error detector.
   Copyright (C) 2012 Free Software Foundation, Inc.
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
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "asan.h"
#include "gimple-pretty-print.h"
#include "target.h"

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

/* Pointer types to 1 resp. 2 byte integers in shadow memory.  A separate
   alias set is used for all shadow memory accesses.  */
static GTY(()) tree shadow_ptr_types[2];

/* Construct a function tree for __asan_report_{load,store}{1,2,4,8,16}.
   IS_STORE is either 1 (for a store) or 0 (for a load).
   SIZE_IN_BYTES is one of 1, 2, 4, 8, 16.  */

static tree
report_error_func (bool is_store, int size_in_bytes)
{
  tree fn_type;
  tree def;
  char name[100];

  sprintf (name, "__asan_report_%s%d",
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


#define PROB_VERY_UNLIKELY	(REG_BR_PROB_BASE / 2000 - 1)
#define PROB_ALWAYS		(REG_BR_PROB_BASE)

/* Instrument the memory access instruction BASE.
   Insert new statements before ITER.
   LOCATION is source code location.
   IS_STORE is either 1 (for a store) or 0 (for a load).
   SIZE_IN_BYTES is one of 1, 2, 4, 8, 16.  */

static void
build_check_stmt (tree base,
                  gimple_stmt_iterator *iter,
                  location_t location, bool is_store, int size_in_bytes)
{
  gimple_stmt_iterator gsi;
  basic_block cond_bb, then_bb, else_bb;
  edge e;
  tree t, base_addr, shadow;
  gimple g;
  tree shadow_ptr_type = shadow_ptr_types[size_in_bytes == 16 ? 1 : 0];
  tree shadow_type = TREE_TYPE (shadow_ptr_type);
  tree uintptr_type
    = build_nonstandard_integer_type (TYPE_PRECISION (TREE_TYPE (base)), 1);

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
  else_bb = e->dest;

  /* A recap at this point: else_bb is the basic block at whose head
     is the gimple statement for which this check expression is being
     built.  cond_bb is the (possibly new, synthetic) basic block the
     end of which will contain the cache-lookup code, and a
     conditional that jumps to the cache-miss code or, much more
     likely, over to else_bb.  */

  /* Create the bb that contains the crash block.  */
  then_bb = create_empty_bb (cond_bb);
  e = make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  e->probability = PROB_VERY_UNLIKELY;
  make_single_succ_edge (then_bb, else_bb, EDGE_FALLTHRU);

  /* Mark the pseudo-fallthrough edge from cond_bb to else_bb.  */
  e = find_edge (cond_bb, else_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->count = cond_bb->count;
  e->probability = PROB_ALWAYS - PROB_VERY_UNLIKELY;

  /* Update dominance info.  Note that bb_join's data was
     updated by split_block.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);
      set_immediate_dominator (CDI_DOMINATORS, else_bb, cond_bb);
    }

  base = unshare_expr (base);

  gsi = gsi_last_bb (cond_bb);
  g = gimple_build_assign_with_ops (TREE_CODE (base),
				    make_ssa_name (TREE_TYPE (base), NULL),
				    base, NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  g = gimple_build_assign_with_ops (NOP_EXPR,
				    make_ssa_name (uintptr_type, NULL),
				    gimple_assign_lhs (g), NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  base_addr = gimple_assign_lhs (g);

  /* Build
     (base_addr >> ASAN_SHADOW_SHIFT) + targetm.asan_shadow_offset ().  */

  t = build_int_cst (uintptr_type, ASAN_SHADOW_SHIFT);
  g = gimple_build_assign_with_ops (RSHIFT_EXPR,
				    make_ssa_name (uintptr_type, NULL),
				    base_addr, t);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  t = build_int_cst (uintptr_type, targetm.asan_shadow_offset ());
  g = gimple_build_assign_with_ops (PLUS_EXPR,
				    make_ssa_name (uintptr_type, NULL),
				    gimple_assign_lhs (g), t);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  g = gimple_build_assign_with_ops (NOP_EXPR,
				    make_ssa_name (shadow_ptr_type, NULL),
				    gimple_assign_lhs (g), NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  t = build2 (MEM_REF, shadow_type, gimple_assign_lhs (g),
	      build_int_cst (shadow_ptr_type, 0));
  g = gimple_build_assign_with_ops (MEM_REF,
				    make_ssa_name (shadow_type, NULL),
				    t, NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
  shadow = gimple_assign_lhs (g);

  if (size_in_bytes < 8)
    {
      /* Slow path for 1, 2 and 4 byte accesses.
	 Test (shadow != 0)
	      & ((base_addr & 7) + (size_in_bytes - 1)) >= shadow).  */
      g = gimple_build_assign_with_ops (NE_EXPR,
					make_ssa_name (boolean_type_node,
						       NULL),
					shadow,
					build_int_cst (shadow_type, 0));
      gimple_set_location (g, location);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);
      t = gimple_assign_lhs (g);

      g = gimple_build_assign_with_ops (BIT_AND_EXPR,
					make_ssa_name (uintptr_type,
						       NULL),
					base_addr,
					build_int_cst (uintptr_type, 7));
      gimple_set_location (g, location);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);

      g = gimple_build_assign_with_ops (NOP_EXPR,
					make_ssa_name (shadow_type,
						       NULL),
					gimple_assign_lhs (g), NULL_TREE);
      gimple_set_location (g, location);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);

      if (size_in_bytes > 1)
	{
	  g = gimple_build_assign_with_ops (PLUS_EXPR,
					    make_ssa_name (shadow_type,
							   NULL),
					    gimple_assign_lhs (g),
					    build_int_cst (shadow_type,
							   size_in_bytes - 1));
	  gimple_set_location (g, location);
	  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
	}

      g = gimple_build_assign_with_ops (GE_EXPR,
					make_ssa_name (boolean_type_node,
						       NULL),
					gimple_assign_lhs (g),
					shadow);
      gimple_set_location (g, location);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);

      g = gimple_build_assign_with_ops (BIT_AND_EXPR,
					make_ssa_name (boolean_type_node,
						       NULL),
					t, gimple_assign_lhs (g));
      gimple_set_location (g, location);
      gsi_insert_after (&gsi, g, GSI_NEW_STMT);
      t = gimple_assign_lhs (g);
    }
  else
    t = shadow;

  g = gimple_build_cond (NE_EXPR, t, build_int_cst (TREE_TYPE (t), 0),
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  /* Generate call to the run-time library (e.g. __asan_report_load8).  */
  gsi = gsi_start_bb (then_bb);
  g = gimple_build_call (report_error_func (is_store, size_in_bytes),
			 1, base_addr);
  gimple_set_location (g, location);
  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

  *iter = gsi_start_bb (else_bb);
}

/* If T represents a memory access, add instrumentation code before ITER.
   LOCATION is source code location.
   IS_STORE is either 1 (for a store) or 0 (for a load).  */

static void
instrument_derefs (gimple_stmt_iterator *iter, tree t,
                  location_t location, bool is_store)
{
  tree type, base;
  HOST_WIDE_INT size_in_bytes;

  type = TREE_TYPE (t);
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

  size_in_bytes = int_size_in_bytes (type);
  if ((size_in_bytes & (size_in_bytes - 1)) != 0
      || (unsigned HOST_WIDE_INT) size_in_bytes - 1 >= 16)
    return;

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

  base = build_fold_addr_expr (t);
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

  FOR_EACH_BB (bb)
    {
      if (bb->index >= saved_last_basic_block) continue;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
          gimple s = gsi_stmt (i);
          if (!gimple_assign_single_p (s))
	    continue;
          instrument_derefs (&i, gimple_assign_lhs (s),
                             gimple_location (s), true);
          instrument_derefs (&i, gimple_assign_rhs1 (s),
                             gimple_location (s), false);
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

/* Initialize shadow_ptr_types array.  */

static void
asan_init_shadow_ptr_types (void)
{
  alias_set_type set = new_alias_set ();
  shadow_ptr_types[0] = build_distinct_type_copy (signed_char_type_node);
  TYPE_ALIAS_SET (shadow_ptr_types[0]) = set;
  shadow_ptr_types[0] = build_pointer_type (shadow_ptr_types[0]);
  shadow_ptr_types[1] = build_distinct_type_copy (short_integer_type_node);
  TYPE_ALIAS_SET (shadow_ptr_types[1]) = set;
  shadow_ptr_types[1] = build_pointer_type (shadow_ptr_types[1]);
}

/* Instrument the current function.  */

static unsigned int
asan_instrument (void)
{
  if (shadow_ptr_types[0] == NULL_TREE)
    asan_init_shadow_ptr_types ();
  transform_statements ();
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
  | TODO_update_ssa			/* todo_flags_finish  */
 }
};

static bool
gate_asan_O0 (void)
{
  return flag_asan != 0 && !optimize;
}

struct gimple_opt_pass pass_asan_O0 =
{
 {
  GIMPLE_PASS,
  "asan0",				/* name  */
  OPTGROUP_NONE,                        /* optinfo_flags */
  gate_asan_O0,				/* gate  */
  asan_instrument,			/* execute  */
  NULL,					/* sub  */
  NULL,					/* next  */
  0,					/* static_pass_number  */
  TV_NONE,				/* tv_id  */
  PROP_ssa | PROP_cfg | PROP_gimple_leh,/* properties_required  */
  0,					/* properties_provided  */
  0,					/* properties_destroyed  */
  0,					/* todo_flags_start  */
  TODO_verify_flow | TODO_verify_stmts
  | TODO_update_ssa			/* todo_flags_finish  */
 }
};

#include "gt-asan.h"
