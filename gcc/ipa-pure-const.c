/* Callgraph based analysis of static variables.
   Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

/* This file marks functions as being either const (TREE_READONLY) or
   pure (DECL_PURE_P).  It can also set a variant of these that
   are allowed to loop indefinitely (DECL_LOOPING_CONST_PURE_P).

   This must be run after inlining decisions have been made since
   otherwise, the local sets will not contain information that is
   consistent with post inlined state.  The global sets are not prone
   to this problem since they are by definition transitive.  */

/* The code in this module is called by the ipa pass manager. It
   should be one of the later passes since it's information is used by
   the rest of the compilation. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "ggc.h"
#include "ipa-utils.h"
#include "gimple.h"
#include "cgraph.h"
#include "output.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "target.h"
#include "lto-streamer.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"

static struct pointer_set_t *visited_nodes;

/* Lattice values for const and pure functions.  Everything starts out
   being const, then may drop to pure and then neither depending on
   what is found.  */
enum pure_const_state_e
{
  IPA_CONST,
  IPA_PURE,
  IPA_NEITHER
};

/* Holder for the const_state.  There is one of these per function
   decl.  */
struct funct_state_d
{
  /* See above.  */
  enum pure_const_state_e pure_const_state;
  /* What user set here; we can be always sure about this.  */
  enum pure_const_state_e state_previously_known;
  bool looping_previously_known;

  /* True if the function could possibly infinite loop.  There are a
     lot of ways that this could be determined.  We are pretty
     conservative here.  While it is possible to cse pure and const
     calls, it is not legal to have dce get rid of the call if there
     is a possibility that the call could infinite loop since this is
     a behavioral change.  */
  bool looping;

  bool can_throw;
};

typedef struct funct_state_d * funct_state;

/* The storage of the funct_state is abstracted because there is the
   possibility that it may be desirable to move this to the cgraph
   local info.  */

/* Array, indexed by cgraph node uid, of function states.  */

DEF_VEC_P (funct_state);
DEF_VEC_ALLOC_P (funct_state, heap);
static VEC (funct_state, heap) *funct_state_vec;

/* Holders of ipa cgraph hooks: */
static struct cgraph_node_hook_list *function_insertion_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;

/* Init the function state.  */

static void
finish_state (void)
{
  free (funct_state_vec);
}


/* Return the function state from NODE.  */

static inline funct_state
get_function_state (struct cgraph_node *node)
{
  if (!funct_state_vec
      || VEC_length (funct_state, funct_state_vec) <= (unsigned int)node->uid)
    return NULL;
  return VEC_index (funct_state, funct_state_vec, node->uid);
}

/* Set the function state S for NODE.  */

static inline void
set_function_state (struct cgraph_node *node, funct_state s)
{
  if (!funct_state_vec
      || VEC_length (funct_state, funct_state_vec) <= (unsigned int)node->uid)
     VEC_safe_grow_cleared (funct_state, heap, funct_state_vec, node->uid + 1);
  VEC_replace (funct_state, funct_state_vec, node->uid, s);
}

/* Check to see if the use (or definition when CHECKING_WRITE is true)
   variable T is legal in a function that is either pure or const.  */

static inline void
check_decl (funct_state local,
	    tree t, bool checking_write)
{
  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
        fprintf (dump_file, "    Volatile operand is not const/pure");
      return;
    }

  /* Do not care about a local automatic that is not static.  */
  if (!TREE_STATIC (t) && !DECL_EXTERNAL (t))
    return;

  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  if (DECL_PRESERVE_P (t))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
        fprintf (dump_file, "    Used static/global variable is not const/pure\n");
      return;
    }

  /* Since we have dealt with the locals and params cases above, if we
     are CHECKING_WRITE, this cannot be a pure or constant
     function.  */
  if (checking_write)
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
        fprintf (dump_file, "    static/global memory write is not const/pure\n");
      return;
    }

  if (DECL_EXTERNAL (t) || TREE_PUBLIC (t))
    {
      /* Readonly reads are safe.  */
      if (TREE_READONLY (t) && !TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (t)))
	return; /* Read of a constant, do not change the function state.  */
      else
	{
          if (dump_file)
            fprintf (dump_file, "    global memory read is not const\n");
	  /* Just a regular read.  */
	  if (local->pure_const_state == IPA_CONST)
	    local->pure_const_state = IPA_PURE;
	}
    }
  else
    {
      /* Compilation level statics can be read if they are readonly
	 variables.  */
      if (TREE_READONLY (t))
	return;

      if (dump_file)
	fprintf (dump_file, "    static memory read is not const\n");
      /* Just a regular read.  */
      if (local->pure_const_state == IPA_CONST)
	local->pure_const_state = IPA_PURE;
    }
}


/* Check to see if the use (or definition when CHECKING_WRITE is true)
   variable T is legal in a function that is either pure or const.  */

static inline void
check_op (funct_state local, tree t, bool checking_write)
{
  t = get_base_address (t);
  if (t && TREE_THIS_VOLATILE (t))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
	fprintf (dump_file, "    Volatile indirect ref is not const/pure\n");
      return;
    }
  else if (t
  	   && INDIRECT_REF_P (t)
	   && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
	   && !ptr_deref_may_alias_global_p (TREE_OPERAND (t, 0)))
    {
      if (dump_file)
	fprintf (dump_file, "    Indirect ref to local memory is OK\n");
      return;
    }
  else if (checking_write)
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
	fprintf (dump_file, "    Indirect ref write is not const/pure\n");
      return;
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, "    Indirect ref read is not const\n");
      if (local->pure_const_state == IPA_CONST)
	local->pure_const_state = IPA_PURE;
    }
}

/* Check the parameters of a function call to CALL_EXPR to see if
   there are any references in the parameters that are not allowed for
   pure or const functions.  Also check to see if this is either an
   indirect call, a call outside the compilation unit, or has special
   attributes that may also effect the purity.  The CALL_EXPR node for
   the entire call expression.  */

static void
check_call (funct_state local, gimple call, bool ipa)
{
  int flags = gimple_call_flags (call);
  tree callee_t = gimple_call_fndecl (call);
  bool possibly_throws = stmt_could_throw_p (call);
  bool possibly_throws_externally = (possibly_throws
  				     && stmt_can_throw_external (call));

  if (possibly_throws)
    {
      unsigned int i;
      for (i = 0; i < gimple_num_ops (call); i++)
        if (gimple_op (call, i)
	    && tree_could_throw_p (gimple_op (call, i)))
	  {
	    if (possibly_throws && flag_non_call_exceptions)
	      {
		if (dump_file)
		  fprintf (dump_file, "    operand can throw; looping\n");
		local->looping = true;
	      }
	    if (possibly_throws_externally)
	      {
		if (dump_file)
		  fprintf (dump_file, "    operand can throw externally\n");
		local->can_throw = true;
	      }
	  }
    }

  /* The const and pure flags are set by a variety of places in the
     compiler (including here).  If someone has already set the flags
     for the callee, (such as for some of the builtins) we will use
     them, otherwise we will compute our own information.

     Const and pure functions have less clobber effects than other
     functions so we process these first.  Otherwise if it is a call
     outside the compilation unit or an indirect call we punt.  This
     leaves local calls which will be processed by following the call
     graph.  */
  if (callee_t)
    {
      /* When bad things happen to bad functions, they cannot be const
	 or pure.  */
      if (setjmp_call_p (callee_t))
	{
	  if (dump_file)
	    fprintf (dump_file, "    setjmp is not const/pure\n");
          local->looping = true;
	  local->pure_const_state = IPA_NEITHER;
	}

      if (DECL_BUILT_IN_CLASS (callee_t) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (callee_t))
	  {
	  case BUILT_IN_LONGJMP:
	  case BUILT_IN_NONLOCAL_GOTO:
	    if (dump_file)
	      fprintf (dump_file, "    longjmp and nonlocal goto is not const/pure\n");
	    local->pure_const_state = IPA_NEITHER;
            local->looping = true;
	    break;
	  default:
	    break;
	  }
    }

  /* When not in IPA mode, we can still handle self recursion.  */
  if (!ipa && callee_t == current_function_decl)
    local->looping = true;
  /* Either calle is unknown or we are doing local analysis.
     Look to see if there are any bits available for the callee (such as by
     declaration or because it is builtin) and process solely on the basis of
     those bits. */
  else if (!ipa || !callee_t)
    {
      if (possibly_throws && flag_non_call_exceptions)
        {
	  if (dump_file)
	    fprintf (dump_file, "    can throw; looping\n");
          local->looping = true;
	}
      if (possibly_throws_externally)
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "    can throw externally to lp %i\n",
	      	       lookup_stmt_eh_lp (call));
	      if (callee_t)
		fprintf (dump_file, "     callee:%s\n",
			 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (callee_t)));
	    }
          local->can_throw = true;
	}
      if (flags & ECF_CONST)
	{
          if (callee_t && DECL_LOOPING_CONST_OR_PURE_P (callee_t))
            local->looping = true;
	 }
      else if (flags & ECF_PURE)
	{
          if (callee_t && DECL_LOOPING_CONST_OR_PURE_P (callee_t))
            local->looping = true;
	  if (dump_file)
	    fprintf (dump_file, "    pure function call in not const\n");
	  if (local->pure_const_state == IPA_CONST)
	    local->pure_const_state = IPA_PURE;
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "    uknown function call is not const/pure\n");
	  local->pure_const_state = IPA_NEITHER;
          local->looping = true;
	}
    }
  /* Direct functions calls are handled by IPA propagation.  */
}

/* Wrapper around check_decl for loads.  */

static bool
check_load (gimple stmt ATTRIBUTE_UNUSED, tree op, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, false);
  else
    check_op ((funct_state)data, op, false);
  return false;
}

/* Wrapper around check_decl for stores.  */

static bool
check_store (gimple stmt ATTRIBUTE_UNUSED, tree op, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, true);
  else
    check_op ((funct_state)data, op, true);
  return false;
}

/* Look into pointer pointed to by GSIP and figure out what interesting side
   effects it has.  */
static void
check_stmt (gimple_stmt_iterator *gsip, funct_state local, bool ipa)
{
  gimple stmt = gsi_stmt (*gsip);
  unsigned int i = 0;

  if (is_gimple_debug (stmt))
    return;

  if (dump_file)
    {
      fprintf (dump_file, "  scanning: ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  if (gimple_has_volatile_ops (stmt))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
	fprintf (dump_file, "    Volatile stmt is not const/pure\n");
    }

  /* Look for loads and stores.  */
  walk_stmt_load_store_ops (stmt, local, check_load, check_store);

  if (gimple_code (stmt) != GIMPLE_CALL
      && stmt_could_throw_p (stmt))
    {
      if (flag_non_call_exceptions)
	{
	  if (dump_file)
	    fprintf (dump_file, "    can throw; looping");
	  local->looping = true;
	}
      if (stmt_can_throw_external (stmt))
	{
	  if (dump_file)
	    fprintf (dump_file, "    can throw externally");
	  local->can_throw = true;
	}
    }
  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      check_call (local, stmt, ipa);
      break;
    case GIMPLE_LABEL:
      if (DECL_NONLOCAL (gimple_label_label (stmt)))
	/* Target of long jump. */
	{
          if (dump_file)
            fprintf (dump_file, "    nonlocal label is not const/pure");
	  local->pure_const_state = IPA_NEITHER;
	}
      break;
    case GIMPLE_ASM:
      for (i = 0; i < gimple_asm_nclobbers (stmt); i++)
	{
	  tree op = gimple_asm_clobber_op (stmt, i);
	  if (strcmp (TREE_STRING_POINTER (TREE_VALUE (op)), "memory") == 0)
	    {
              if (dump_file)
                fprintf (dump_file, "    memory asm clobber is not const/pure");
	      /* Abandon all hope, ye who enter here. */
	      local->pure_const_state = IPA_NEITHER;
	    }
	}
      if (gimple_asm_volatile_p (stmt))
	{
	  if (dump_file)
	    fprintf (dump_file, "    volatile is not const/pure");
	  /* Abandon all hope, ye who enter here. */
	  local->pure_const_state = IPA_NEITHER;
          local->looping = true;
	}
      return;
    default:
      break;
    }
}


/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static funct_state
analyze_function (struct cgraph_node *fn, bool ipa)
{
  tree decl = fn->decl;
  tree old_decl = current_function_decl;
  funct_state l;
  basic_block this_block;

  l = XCNEW (struct funct_state_d);
  l->pure_const_state = IPA_CONST;
  l->state_previously_known = IPA_NEITHER;
  l->looping_previously_known = true;
  l->looping = false;
  l->can_throw = false;

  if (dump_file)
    {
      fprintf (dump_file, "\n\n local analysis of %s\n ",
	       cgraph_node_name (fn));
    }

  push_cfun (DECL_STRUCT_FUNCTION (decl));
  current_function_decl = decl;

  FOR_EACH_BB (this_block)
    {
      gimple_stmt_iterator gsi;
      struct walk_stmt_info wi;

      memset (&wi, 0, sizeof(wi));
      for (gsi = gsi_start_bb (this_block);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  check_stmt (&gsi, l, ipa);
	  if (l->pure_const_state == IPA_NEITHER && l->looping && l->can_throw)
	    goto end;
	}
    }

end:
  if (l->pure_const_state != IPA_NEITHER)
    {
      /* Const functions cannot have back edges (an
	 indication of possible infinite loop side
	 effect.  */
      if (mark_dfs_back_edges ())
        {
	  /* Preheaders are needed for SCEV to work.
	     Simple lateches and recorded exits improve chances that loop will
	     proved to be finite in testcases such as in loop-15.c and loop-24.c  */
	  loop_optimizer_init (LOOPS_NORMAL
			       | LOOPS_HAVE_RECORDED_EXITS);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    flow_loops_dump (dump_file, NULL, 0);
	  if (mark_irreducible_loops ())
	    {
	      if (dump_file)
	        fprintf (dump_file, "    has irreducible loops\n");
	      l->looping = true;
	    }
	  else
	    {
	      loop_iterator li;
	      struct loop *loop;
	      scev_initialize ();
	      FOR_EACH_LOOP (li, loop, 0)
		if (!finite_loop_p (loop))
		  {
		    if (dump_file)
		      fprintf (dump_file, "    can not prove finiteness of loop %i\n", loop->num);
		    l->looping =true;
		    break;
		  }
	      scev_finalize ();
	    }
          loop_optimizer_finalize ();
	}
    }

  if (TREE_READONLY (decl))
    {
      l->pure_const_state = IPA_CONST;
      l->state_previously_known = IPA_CONST;
      if (!DECL_LOOPING_CONST_OR_PURE_P (decl))
        l->looping = false, l->looping_previously_known = false;
    }
  if (DECL_PURE_P (decl))
    {
      if (l->pure_const_state != IPA_CONST)
        l->pure_const_state = IPA_PURE;
      l->state_previously_known = IPA_PURE;
      if (!DECL_LOOPING_CONST_OR_PURE_P (decl))
        l->looping = false, l->looping_previously_known = false;
    }
  if (TREE_NOTHROW (decl))
    l->can_throw = false;

  pop_cfun ();
  current_function_decl = old_decl;
  if (dump_file)
    {
      if (l->looping)
        fprintf (dump_file, "Function is locally looping.\n");
      if (l->can_throw)
        fprintf (dump_file, "Function is locally throwing.\n");
      if (l->pure_const_state == IPA_CONST)
        fprintf (dump_file, "Function is locally const.\n");
      if (l->pure_const_state == IPA_PURE)
        fprintf (dump_file, "Function is locally pure.\n");
    }
  return l;
}

/* Called when new function is inserted to callgraph late.  */
static void
add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
 if (cgraph_function_body_availability (node) < AVAIL_OVERWRITABLE)
   return;
  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  visited_nodes = pointer_set_create ();
  set_function_state (node, analyze_function (node, true));
  pointer_set_destroy (visited_nodes);
  visited_nodes = NULL;
}

/* Called when new clone is inserted to callgraph late.  */

static void
duplicate_node_data (struct cgraph_node *src, struct cgraph_node *dst,
	 	     void *data ATTRIBUTE_UNUSED)
{
  if (get_function_state (src))
    {
      funct_state l = XNEW (struct funct_state_d);
      gcc_assert (!get_function_state (dst));
      memcpy (l, get_function_state (src), sizeof (*l));
      set_function_state (dst, l);
    }
}

/* Called when new clone is inserted to callgraph late.  */

static void
remove_node_data (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (get_function_state (node))
    {
      free (get_function_state (node));
      set_function_state (node, NULL);
    }
}


static void
register_hooks (void)
{
  static bool init_p = false;

  if (init_p)
    return;

  init_p = true;

  node_removal_hook_holder =
      cgraph_add_node_removal_hook (&remove_node_data, NULL);
  node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&duplicate_node_data, NULL);
  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&add_new_function, NULL);
}


/* Analyze each function in the cgraph to see if it is locally PURE or
   CONST.  */

static void
generate_summary (void)
{
  struct cgraph_node *node;

  register_hooks ();

  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  visited_nodes = pointer_set_create ();

  /* Process all of the functions.

     We process AVAIL_OVERWRITABLE functions.  We can not use the results
     by default, but the info can be used at LTO with -fwhole-program or
     when function got clonned and the clone is AVAILABLE.  */

  for (node = cgraph_nodes; node; node = node->next)
    if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      set_function_state (node, analyze_function (node, true));

  pointer_set_destroy (visited_nodes);
  visited_nodes = NULL;
}


/* Serialize the ipa info for lto.  */

static void
pure_const_write_summary (cgraph_node_set set)
{
  struct cgraph_node *node;
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_pure_const);
  unsigned int count = 0;
  cgraph_node_set_iterator csi;

  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (node->analyzed && get_function_state (node) != NULL)
	count++;
    }

  lto_output_uleb128_stream (ob->main_stream, count);

  /* Process all of the functions.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (node->analyzed && get_function_state (node) != NULL)
	{
	  struct bitpack_d *bp;
	  funct_state fs;
	  int node_ref;
	  lto_cgraph_encoder_t encoder;

	  fs = get_function_state (node);

	  encoder = ob->decl_state->cgraph_node_encoder;
	  node_ref = lto_cgraph_encoder_encode (encoder, node);
	  lto_output_uleb128_stream (ob->main_stream, node_ref);

	  /* Note that flags will need to be read in the opposite
	     order as we are pushing the bitflags into FLAGS.  */
	  bp = bitpack_create ();
	  bp_pack_value (bp, fs->pure_const_state, 2);
	  bp_pack_value (bp, fs->state_previously_known, 2);
	  bp_pack_value (bp, fs->looping_previously_known, 1);
	  bp_pack_value (bp, fs->looping, 1);
	  bp_pack_value (bp, fs->can_throw, 1);
	  lto_output_bitpack (ob->main_stream, bp);
	  bitpack_delete (bp);
	}
    }

  lto_destroy_simple_output_block (ob);
}


/* Deserialize the ipa info for lto.  */

static void
pure_const_read_summary (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  register_hooks ();
  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      struct lto_input_block *ib
	= lto_create_simple_input_block (file_data,
					 LTO_section_ipa_pure_const,
					 &data, &len);
      if (ib)
	{
	  unsigned int i;
	  unsigned int count = lto_input_uleb128 (ib);

	  for (i = 0; i < count; i++)
	    {
	      unsigned int index;
	      struct cgraph_node *node;
	      struct bitpack_d *bp;
	      funct_state fs;
	      lto_cgraph_encoder_t encoder;

	      fs = XCNEW (struct funct_state_d);
	      index = lto_input_uleb128 (ib);
	      encoder = file_data->cgraph_node_encoder;
	      node = lto_cgraph_encoder_deref (encoder, index);
	      set_function_state (node, fs);

	      /* Note that the flags must be read in the opposite
		 order in which they were written (the bitflags were
		 pushed into FLAGS).  */
	      bp = lto_input_bitpack (ib);
	      fs->pure_const_state
			= (enum pure_const_state_e) bp_unpack_value (bp, 2);
	      fs->state_previously_known
			= (enum pure_const_state_e) bp_unpack_value (bp, 2);
	      fs->looping_previously_known = bp_unpack_value (bp, 1);
	      fs->looping = bp_unpack_value (bp, 1);
	      fs->can_throw = bp_unpack_value (bp, 1);
	      bitpack_delete (bp);
	    }

	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_pure_const,
					  ib, data, len);
	}
    }
}


static bool
ignore_edge (struct cgraph_edge *e)
{
  return (!e->can_throw_external);
}

/* Return true if NODE is self recursive function.  */

static bool
self_recursive_p (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    if (e->callee == node)
      return true;
  return false;
}

/* Produce the global information by preforming a transitive closure
   on the local information that was produced by generate_summary.
   Note that there is no function_transform pass since this only
   updates the function_decl.  */

static unsigned int
propagate (void)
{
  struct cgraph_node *node;
  struct cgraph_node *w;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  int order_pos;
  int i;
  struct ipa_dfs_info * w_info;

  cgraph_remove_function_insertion_hook (function_insertion_hook_holder);
  cgraph_remove_node_duplication_hook (node_duplication_hook_holder);
  cgraph_remove_node_removal_hook (node_removal_hook_holder);
  order_pos = ipa_utils_reduced_inorder (order, true, false, NULL);
  if (dump_file)
    {
      dump_cgraph (dump_file);
      ipa_utils_print_order(dump_file, "reduced", order, order_pos);
    }

  /* Propagate the local information thru the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  for (i = 0; i < order_pos; i++ )
    {
      enum pure_const_state_e pure_const_state = IPA_CONST;
      bool looping = false;
      int count = 0;
      node = order[i];

      /* Find the worst state for any node in the cycle.  */
      w = node;
      while (w)
	{
	  struct cgraph_edge *e;
	  funct_state w_l = get_function_state (w);
	  if (pure_const_state < w_l->pure_const_state)
	    pure_const_state = w_l->pure_const_state;

	  if (w_l->looping)
	    looping = true;
	  if (cgraph_function_body_availability (w) == AVAIL_OVERWRITABLE)
	    {
	      looping |= w_l->looping_previously_known;
	      if (pure_const_state < w_l->state_previously_known)
	        pure_const_state = w_l->state_previously_known;
	    }

	  if (pure_const_state == IPA_NEITHER)
	    break;

	  count++;

	  if (count > 1)
	    looping = true;

	  for (e = w->callees; e; e = e->next_callee)
	    {
	      struct cgraph_node *y = e->callee;

	      if (cgraph_function_body_availability (y) > AVAIL_OVERWRITABLE)
		{
		  funct_state y_l = get_function_state (y);
		  if (pure_const_state < y_l->pure_const_state)
		    pure_const_state = y_l->pure_const_state;
		  if (pure_const_state == IPA_NEITHER)
		    break;
		  if (y_l->looping)
		    looping = true;
		}
	      else
	        {
		  int flags = flags_from_decl_or_type (y->decl);

		  if (flags & ECF_LOOPING_CONST_OR_PURE)
		    looping = true;
		  if (flags & ECF_CONST)
		    ;
		  else if ((flags & ECF_PURE) && pure_const_state == IPA_CONST)
		    pure_const_state = IPA_PURE;
		  else
		    pure_const_state = IPA_NEITHER, looping = true;

		}
	    }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}

      /* Copy back the region's pure_const_state which is shared by
	 all nodes in the region.  */
      w = node;
      while (w)
	{
	  funct_state w_l = get_function_state (w);
	  enum pure_const_state_e this_state = pure_const_state;
	  bool this_looping = looping;

	  if (w_l->state_previously_known != IPA_NEITHER
	      && this_state > w_l->state_previously_known)
            this_state = w_l->state_previously_known;
	  if (!this_looping && self_recursive_p (w))
	    this_looping = true;
	  if (!w_l->looping_previously_known)
	    this_looping = false;

	  /* All nodes within a cycle share the same info.  */
	  w_l->pure_const_state = this_state;
	  w_l->looping = this_looping;

	  switch (this_state)
	    {
	    case IPA_CONST:
	      if (!TREE_READONLY (w->decl) && dump_file)
		fprintf (dump_file, "Function found to be %sconst: %s\n",
			 this_looping ? "looping " : "",
			 cgraph_node_name (w));
	      cgraph_set_readonly_flag (w, true);
	      cgraph_set_looping_const_or_pure_flag (w, this_looping);
	      break;

	    case IPA_PURE:
	      if (!DECL_PURE_P (w->decl) && dump_file)
		fprintf (dump_file, "Function found to be %spure: %s\n",
			 this_looping ? "looping " : "",
			 cgraph_node_name (w));
	      cgraph_set_pure_flag (w, true);
	      cgraph_set_looping_const_or_pure_flag (w, this_looping);
	      break;

	    default:
	      break;
	    }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}
    }

  /* Cleanup. */
  for (node = cgraph_nodes; node; node = node->next)
    {
      /* Get rid of the aux information.  */
      if (node->aux)
	{
	  w_info = (struct ipa_dfs_info *) node->aux;
	  free (node->aux);
	  node->aux = NULL;
	}
    }
  order_pos = ipa_utils_reduced_inorder (order, true, false, ignore_edge);
  if (dump_file)
    {
      dump_cgraph (dump_file);
      ipa_utils_print_order(dump_file, "reduced for nothrow", order, order_pos);
    }
  /* Propagate the local information thru the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  for (i = 0; i < order_pos; i++ )
    {
      bool can_throw = false;
      node = order[i];

      /* Find the worst state for any node in the cycle.  */
      w = node;
      while (w)
	{
	  struct cgraph_edge *e;
	  funct_state w_l = get_function_state (w);

	  if (w_l->can_throw
	      || cgraph_function_body_availability (w) == AVAIL_OVERWRITABLE)
	    can_throw = true;

	  if (can_throw)
	    break;

	  for (e = w->callees; e; e = e->next_callee)
	    {
	      struct cgraph_node *y = e->callee;

	      if (cgraph_function_body_availability (y) > AVAIL_OVERWRITABLE)
		{
		  funct_state y_l = get_function_state (y);

		  if (can_throw)
		    break;
		  if (y_l->can_throw && !TREE_NOTHROW (w->decl)
		      && e->can_throw_external)
		    can_throw = true;
		}
	      else if (e->can_throw_external && !TREE_NOTHROW (y->decl))
	        can_throw = true;
	    }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}

      /* Copy back the region's pure_const_state which is shared by
	 all nodes in the region.  */
      w = node;
      while (w)
	{
	  funct_state w_l = get_function_state (w);
	  if (!can_throw && !TREE_NOTHROW (w->decl))
	    {
	      struct cgraph_edge *e;
	      cgraph_set_nothrow_flag (w, true);
	      for (e = w->callers; e; e = e->next_caller)
	        e->can_throw_external = false;
	      if (dump_file)
		fprintf (dump_file, "Function found to be nothrow: %s\n",
			 cgraph_node_name (w));
	    }
	  else if (can_throw && !TREE_NOTHROW (w->decl))
	    w_l->can_throw = true;
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}
    }

  /* Cleanup. */
  for (node = cgraph_nodes; node; node = node->next)
    {
      /* Get rid of the aux information.  */
      if (node->aux)
	{
	  w_info = (struct ipa_dfs_info *) node->aux;
	  free (node->aux);
	  node->aux = NULL;
	}
      if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
	free (get_function_state (node));
    }

  free (order);
  VEC_free (funct_state, heap, funct_state_vec);
  finish_state ();
  return 0;
}

static bool
gate_pure_const (void)
{
  return (flag_ipa_pure_const
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

struct ipa_opt_pass_d pass_ipa_pure_const =
{
 {
  IPA_PASS,
  "pure-const",		                /* name */
  gate_pure_const,			/* gate */
  propagate,			        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_PURE_CONST,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 generate_summary,		        /* generate_summary */
 pure_const_write_summary,		/* write_summary */
 pure_const_read_summary,		/* read_summary */
 NULL,					/* function_read_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};

/* Simple local pass for pure const discovery reusing the analysis from
   ipa_pure_const.   This pass is effective when executed together with
   other optimization passes in early optimization pass queue.  */

static unsigned int
local_pure_const (void)
{
  bool changed = false;
  funct_state l;
  struct cgraph_node *node;

  /* Because we do not schedule pass_fixup_cfg over whole program after early optimizations
     we must not promote functions that are called by already processed functions.  */

  if (function_called_by_processed_nodes_p ())
    {
      if (dump_file)
        fprintf (dump_file, "Function called in recursive cycle; ignoring\n");
      return 0;
    }
  node = cgraph_node (current_function_decl);
  if (cgraph_function_body_availability (node) <= AVAIL_OVERWRITABLE)
    {
      if (dump_file)
        fprintf (dump_file, "Function has wrong visibility; ignoring\n");
      return 0;
    }

  l = analyze_function (node, false);

  switch (l->pure_const_state)
    {
    case IPA_CONST:
      if (!TREE_READONLY (current_function_decl))
	{
	  cgraph_set_readonly_flag (node, true);
	  cgraph_set_looping_const_or_pure_flag (node, l->looping);
	  changed = true;
	  if (dump_file)
	    fprintf (dump_file, "Function found to be %sconst: %s\n",
		     l->looping ? "looping " : "",
		     lang_hooks.decl_printable_name (current_function_decl,
						     2));
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (current_function_decl)
	       && !l->looping)
	{
	  cgraph_set_looping_const_or_pure_flag (node, false);
	  changed = true;
	  if (dump_file)
	    fprintf (dump_file, "Function found to be non-looping: %s\n",
		     lang_hooks.decl_printable_name (current_function_decl,
						     2));
	}
      break;

    case IPA_PURE:
      if (!TREE_READONLY (current_function_decl))
	{
	  cgraph_set_pure_flag (node, true);
	  cgraph_set_looping_const_or_pure_flag (node, l->looping);
	  changed = true;
	  if (dump_file)
	    fprintf (dump_file, "Function found to be %spure: %s\n",
		     l->looping ? "looping " : "",
		     lang_hooks.decl_printable_name (current_function_decl,
						     2));
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (current_function_decl)
	       && !l->looping)
	{
	  cgraph_set_looping_const_or_pure_flag (node, false);
	  changed = true;
	  if (dump_file)
	    fprintf (dump_file, "Function found to be non-looping: %s\n",
		     lang_hooks.decl_printable_name (current_function_decl,
						     2));
	}
      break;

    default:
      break;
    }
  if (!l->can_throw && !TREE_NOTHROW (current_function_decl))
    {
      struct cgraph_edge *e;

      cgraph_set_nothrow_flag (node, true);
      for (e = node->callers; e; e = e->next_caller)
	e->can_throw_external = false;
      changed = true;
      if (dump_file)
	fprintf (dump_file, "Function found to be nothrow: %s\n",
		 lang_hooks.decl_printable_name (current_function_decl,
						 2));
    }
  if (l)
    free (l);
  if (changed)
    return execute_fixup_cfg ();
  else
    return 0;
}

struct gimple_opt_pass pass_local_pure_const =
{
 {
  GIMPLE_PASS,
  "local-pure-const",	                /* name */
  gate_pure_const,			/* gate */
  local_pure_const,		        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_PURE_CONST,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};
