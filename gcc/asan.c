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
#include "expr.h"
#include "optabs.h"

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

 The current implementation supports detection of out-of-bounds and
 use-after-free in the heap, on the stack and for global variables.

 [Protection of stack variables]

 To understand how detection of out-of-bounds and use-after-free works
 for stack variables, lets look at this example on x86_64 where the
 stack grows downward:

     int
     foo ()
     {
       char a[23] = {0};
       int b[2] = {0};

       a[5] = 1;
       b[1] = 2;

       return a[5] + b[1];
     }

 For this function, the stack protected by asan will be organized as
 follows, from the top of the stack to the bottom:

 Slot 1/ [red zone of 32 bytes called 'RIGHT RedZone']

 Slot 2/ [8 bytes of red zone, that adds up to the space of 'a' to make
	  the next slot be 32 bytes aligned; this one is called Partial
	  Redzone; this 32 bytes alignment is an asan constraint]

 Slot 3/ [24 bytes for variable 'a']

 Slot 4/ [red zone of 32 bytes called 'Middle RedZone']

 Slot 5/ [24 bytes of Partial Red Zone (similar to slot 2]

 Slot 6/ [8 bytes for variable 'b']

 Slot 7/ [32 bytes of Red Zone at the bottom of the stack, called 'LEFT
	  RedZone']

 The 32 bytes of LEFT red zone at the bottom of the stack can be
 decomposed as such:

     1/ The first 8 bytes contain a magical asan number that is always
     0x41B58AB3.

     2/ The following 8 bytes contains a pointer to a string (to be
     parsed at runtime by the runtime asan library), which format is
     the following:

      "<function-name> <space> <num-of-variables-on-the-stack>
      (<32-bytes-aligned-offset-in-bytes-of-variable> <space>
      <length-of-var-in-bytes> ){n} "

	where '(...){n}' means the content inside the parenthesis occurs 'n'
	times, with 'n' being the number of variables on the stack.

      3/ The following 16 bytes of the red zone have no particular
      format.

 The shadow memory for that stack layout is going to look like this:

     - content of shadow memory 8 bytes for slot 7: 0xF1F1F1F1.
       The F1 byte pattern is a magic number called
       ASAN_STACK_MAGIC_LEFT and is a way for the runtime to know that
       the memory for that shadow byte is part of a the LEFT red zone
       intended to seat at the bottom of the variables on the stack.

     - content of shadow memory 8 bytes for slots 6 and 5:
       0xF4F4F400.  The F4 byte pattern is a magic number
       called ASAN_STACK_MAGIC_PARTIAL.  It flags the fact that the
       memory region for this shadow byte is a PARTIAL red zone
       intended to pad a variable A, so that the slot following
       {A,padding} is 32 bytes aligned.

       Note that the fact that the least significant byte of this
       shadow memory content is 00 means that 8 bytes of its
       corresponding memory (which corresponds to the memory of
       variable 'b') is addressable.

     - content of shadow memory 8 bytes for slot 4: 0xF2F2F2F2.
       The F2 byte pattern is a magic number called
       ASAN_STACK_MAGIC_MIDDLE.  It flags the fact that the memory
       region for this shadow byte is a MIDDLE red zone intended to
       seat between two 32 aligned slots of {variable,padding}.

     - content of shadow memory 8 bytes for slot 3 and 2:
       0xFFFFFFFFF4000000.  This represents is the concatenation of
       variable 'a' and the partial red zone following it, like what we
       had for variable 'b'.  The least significant 3 bytes being 00
       means that the 3 bytes of variable 'a' are addressable.

     - content of shadow memory 8 bytes for slot 1: 0xFFFFFFFFF3F3F3F3.
       The F3 byte pattern is a magic number called
       ASAN_STACK_MAGIC_RIGHT.  It flags the fact that the memory
       region for this shadow byte is a RIGHT red zone intended to seat
       at the top of the variables of the stack.

 Note that the real variable layout is done in expand_used_vars in
 cfgexpand.c.  As far as Address Sanitizer is concerned, it lays out
 stack variables as well as the different red zones, emits some
 prologue code to populate the shadow memory as to poison (mark as
 non-accessible) the regions of the red zones and mark the regions of
 stack variables as accessible, and emit some epilogue code to
 un-poison (mark as accessible) the regions of red zones right before
 the function exits.  */

alias_set_type asan_shadow_set = -1;

/* Pointer types to 1 resp. 2 byte integers in shadow memory.  A separate
   alias set is used for all shadow memory accesses.  */
static GTY(()) tree shadow_ptr_types[2];

/* Return a CONST_INT representing 4 subsequent shadow memory bytes.  */

static rtx
asan_shadow_cst (unsigned char shadow_bytes[4])
{
  int i;
  unsigned HOST_WIDE_INT val = 0;
  gcc_assert (WORDS_BIG_ENDIAN == BYTES_BIG_ENDIAN);
  for (i = 0; i < 4; i++)
    val |= (unsigned HOST_WIDE_INT) shadow_bytes[BYTES_BIG_ENDIAN ? 3 - i : i]
	   << (BITS_PER_UNIT * i);
  return GEN_INT (trunc_int_for_mode (val, SImode));
}

/* Insert code to protect stack vars.  The prologue sequence should be emitted
   directly, epilogue sequence returned.  BASE is the register holding the
   stack base, against which OFFSETS array offsets are relative to, OFFSETS
   array contains pairs of offsets in reverse order, always the end offset
   of some gap that needs protection followed by starting offset,
   and DECLS is an array of representative decls for each var partition.
   LENGTH is the length of the OFFSETS array, DECLS array is LENGTH / 2 - 1
   elements long (OFFSETS include gap before the first variable as well
   as gaps after each stack variable).  */

rtx
asan_emit_stack_protection (rtx base, HOST_WIDE_INT *offsets, tree *decls,
			    int length)
{
  rtx shadow_base, shadow_mem, ret, mem;
  unsigned char shadow_bytes[4];
  HOST_WIDE_INT base_offset = offsets[length - 1], offset, prev_offset;
  HOST_WIDE_INT last_offset, last_size;
  int l;
  unsigned char cur_shadow_byte = ASAN_STACK_MAGIC_LEFT;
  static pretty_printer pp;
  static bool pp_initialized;
  const char *buf;
  size_t len;
  tree str_cst;

  /* First of all, prepare the description string.  */
  if (!pp_initialized)
    {
      pp_construct (&pp, /* prefix */NULL, /* line-width */0);
      pp_initialized = true;
    }
  pp_clear_output_area (&pp);
  if (DECL_NAME (current_function_decl))
    pp_base_tree_identifier (&pp, DECL_NAME (current_function_decl));
  else
    pp_string (&pp, "<unknown>");
  pp_space (&pp);
  pp_decimal_int (&pp, length / 2 - 1);
  pp_space (&pp);
  for (l = length - 2; l; l -= 2)
    {
      tree decl = decls[l / 2 - 1];
      pp_wide_integer (&pp, offsets[l] - base_offset);
      pp_space (&pp);
      pp_wide_integer (&pp, offsets[l - 1] - offsets[l]);
      pp_space (&pp);
      if (DECL_P (decl) && DECL_NAME (decl))
	{
	  pp_decimal_int (&pp, IDENTIFIER_LENGTH (DECL_NAME (decl)));
	  pp_space (&pp);
	  pp_base_tree_identifier (&pp, DECL_NAME (decl));
	}
      else
	pp_string (&pp, "9 <unknown>");
      pp_space (&pp);
    }
  buf = pp_base_formatted_text (&pp);
  len = strlen (buf);
  str_cst = build_string (len + 1, buf);
  TREE_TYPE (str_cst)
    = build_array_type (char_type_node, build_index_type (size_int (len)));
  TREE_READONLY (str_cst) = 1;
  TREE_STATIC (str_cst) = 1;
  str_cst = build1 (ADDR_EXPR, build_pointer_type (char_type_node), str_cst);

  /* Emit the prologue sequence.  */
  base = expand_binop (Pmode, add_optab, base, GEN_INT (base_offset),
		       NULL_RTX, 1, OPTAB_DIRECT);
  mem = gen_rtx_MEM (ptr_mode, base);
  emit_move_insn (mem, GEN_INT (ASAN_STACK_FRAME_MAGIC));
  mem = adjust_address (mem, VOIDmode, GET_MODE_SIZE (ptr_mode));
  emit_move_insn (mem, expand_normal (str_cst));
  shadow_base = expand_binop (Pmode, lshr_optab, base,
			      GEN_INT (ASAN_SHADOW_SHIFT),
			      NULL_RTX, 1, OPTAB_DIRECT);
  shadow_base = expand_binop (Pmode, add_optab, shadow_base,
			      GEN_INT (targetm.asan_shadow_offset ()),
			      NULL_RTX, 1, OPTAB_DIRECT);
  gcc_assert (asan_shadow_set != -1
	      && (ASAN_RED_ZONE_SIZE >> ASAN_SHADOW_SHIFT) == 4);
  shadow_mem = gen_rtx_MEM (SImode, shadow_base);
  set_mem_alias_set (shadow_mem, asan_shadow_set);
  prev_offset = base_offset;
  for (l = length; l; l -= 2)
    {
      if (l == 2)
	cur_shadow_byte = ASAN_STACK_MAGIC_RIGHT;
      offset = offsets[l - 1];
      if ((offset - base_offset) & (ASAN_RED_ZONE_SIZE - 1))
	{
	  int i;
	  HOST_WIDE_INT aoff
	    = base_offset + ((offset - base_offset)
			     & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1));
	  shadow_mem = adjust_address (shadow_mem, VOIDmode,
				       (aoff - prev_offset)
				       >> ASAN_SHADOW_SHIFT);
	  prev_offset = aoff;
	  for (i = 0; i < 4; i++, aoff += (1 << ASAN_SHADOW_SHIFT))
	    if (aoff < offset)
	      {
		if (aoff < offset - (1 << ASAN_SHADOW_SHIFT) + 1)
		  shadow_bytes[i] = 0;
		else
		  shadow_bytes[i] = offset - aoff;
	      }
	    else
	      shadow_bytes[i] = ASAN_STACK_MAGIC_PARTIAL;
	  emit_move_insn (shadow_mem, asan_shadow_cst (shadow_bytes));
	  offset = aoff;
	}
      while (offset <= offsets[l - 2] - ASAN_RED_ZONE_SIZE)
	{
	  shadow_mem = adjust_address (shadow_mem, VOIDmode,
				       (offset - prev_offset)
				       >> ASAN_SHADOW_SHIFT);
	  prev_offset = offset;
	  memset (shadow_bytes, cur_shadow_byte, 4);
	  emit_move_insn (shadow_mem, asan_shadow_cst (shadow_bytes));
	  offset += ASAN_RED_ZONE_SIZE;
	}
      cur_shadow_byte = ASAN_STACK_MAGIC_MIDDLE;
    }
  do_pending_stack_adjust ();

  /* Construct epilogue sequence.  */
  start_sequence ();

  shadow_mem = gen_rtx_MEM (BLKmode, shadow_base);
  set_mem_alias_set (shadow_mem, asan_shadow_set);
  prev_offset = base_offset;
  last_offset = base_offset;
  last_size = 0;
  for (l = length; l; l -= 2)
    {
      offset = base_offset + ((offsets[l - 1] - base_offset)
			     & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1));
      if (last_offset + last_size != offset)
	{
	  shadow_mem = adjust_address (shadow_mem, VOIDmode,
				       (last_offset - prev_offset)
				       >> ASAN_SHADOW_SHIFT);
	  prev_offset = last_offset;
	  clear_storage (shadow_mem, GEN_INT (last_size >> ASAN_SHADOW_SHIFT),
			 BLOCK_OP_NORMAL);
	  last_offset = offset;
	  last_size = 0;
	}
      last_size += base_offset + ((offsets[l - 2] - base_offset)
				  & ~(ASAN_RED_ZONE_SIZE - HOST_WIDE_INT_1))
		   - offset;
    }
  if (last_size)
    {
      shadow_mem = adjust_address (shadow_mem, VOIDmode,
				   (last_offset - prev_offset)
				   >> ASAN_SHADOW_SHIFT);
      clear_storage (shadow_mem, GEN_INT (last_size >> ASAN_SHADOW_SHIFT),
		     BLOCK_OP_NORMAL);
    }

  do_pending_stack_adjust ();

  ret = get_insns ();
  end_sequence ();
  return ret;
}

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
  asan_shadow_set = new_alias_set ();
  shadow_ptr_types[0] = build_distinct_type_copy (signed_char_type_node);
  TYPE_ALIAS_SET (shadow_ptr_types[0]) = asan_shadow_set;
  shadow_ptr_types[0] = build_pointer_type (shadow_ptr_types[0]);
  shadow_ptr_types[1] = build_distinct_type_copy (short_integer_type_node);
  TYPE_ALIAS_SET (shadow_ptr_types[1]) = asan_shadow_set;
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
