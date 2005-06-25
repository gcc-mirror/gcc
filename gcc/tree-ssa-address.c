/* Memory address lowering and addressing mode selection.
   Copyright (C) 2004 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Utility functions for manipulation with TARGET_MEM_REFs -- tree expressions
   that directly map to addressing modes of the target.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "flags.h"
#include "tree-inline.h"
#include "insn-config.h"
#include "recog.h"
#include "expr.h"
#include "ggc.h"

/* TODO -- handling of symbols (according to Richard Hendersons
   comments, http://gcc.gnu.org/ml/gcc-patches/2005-04/msg00949.html):
   
   There are at least 5 different kinds of symbols that we can run up against:

     (1) binds_local_p, small data area.
     (2) binds_local_p, eg local statics
     (3) !binds_local_p, eg global variables
     (4) thread local, local_exec
     (5) thread local, !local_exec

   Now, (1) won't appear often in an array context, but it certainly can.
   All you have to do is set -GN high enough, or explicitly mark any
   random object __attribute__((section (".sdata"))).

   All of these affect whether or not a symbol is in fact a valid address.
   The only one tested here is (3).  And that result may very well
   be incorrect for (4) or (5).

   An incorrect result here does not cause incorrect results out the
   back end, because the expander in expr.c validizes the address.  However
   it would be nice to improve the handling here in order to produce more
   precise results.  */

/* A "template" for memory address, used to determine whether the address is
   valid for mode.  */

struct mem_addr_template GTY (())
{
  rtx ref;			/* The template.  */
  rtx * GTY ((skip)) step_p;	/* The point in template where the step should be
				   filled in.  */
  rtx * GTY ((skip)) off_p;	/* The point in template where the offset should
				   be filled in.  */
};

/* The templates.  Each of the five bits of the index corresponds to one
   component of TARGET_MEM_REF being present, see TEMPL_IDX.  */

static GTY (()) struct mem_addr_template templates[32];

#define TEMPL_IDX(SYMBOL, BASE, INDEX, STEP, OFFSET) \
  (((SYMBOL != 0) << 4) \
   | ((BASE != 0) << 3) \
   | ((INDEX != 0) << 2) \
   | ((STEP != 0) << 1) \
   | (OFFSET != 0))

/* Stores address for memory reference with parameters SYMBOL, BASE, INDEX,
   STEP and OFFSET to *ADDR.  Stores pointers to where step is placed to
   *STEP_P and offset to *OFFSET_P.  */

static void
gen_addr_rtx (rtx symbol, rtx base, rtx index, rtx step, rtx offset,
	      rtx *addr, rtx **step_p, rtx **offset_p)
{
  rtx act_elem;

  *addr = NULL_RTX;
  if (step_p)
    *step_p = NULL;
  if (offset_p)
    *offset_p = NULL;

  if (index)
    {
      act_elem = index;
      if (step)
	{
	  act_elem = gen_rtx_MULT (Pmode, act_elem, step);

	  if (step_p)
	    *step_p = &XEXP (act_elem, 1);
	}

      *addr = act_elem;
    }

  if (base)
    {
      if (*addr)
	*addr = gen_rtx_PLUS (Pmode, *addr, base);
      else
	*addr = base;
    }

  if (symbol)
    {
      act_elem = symbol;
      if (offset)
	{
	  act_elem = gen_rtx_CONST (Pmode,
				    gen_rtx_PLUS (Pmode, act_elem, offset));
	  if (offset_p)
	    *offset_p = &XEXP (XEXP (act_elem, 0), 1);
	}

      if (*addr)
	*addr = gen_rtx_PLUS (Pmode, *addr, act_elem);
      else
	*addr = act_elem;
    }
  else if (offset)
    {
      if (*addr)
	{
	  *addr = gen_rtx_PLUS (Pmode, *addr, offset);
	  if (offset_p)
	    *offset_p = &XEXP (*addr, 1);
	}
      else
	{
	  *addr = offset;
	  if (offset_p)
	    *offset_p = addr;
	}
    }

  if (!*addr)
    *addr = const0_rtx;
}

/* Returns address for TARGET_MEM_REF with parameters given by ADDR.
   If REALLY_EXPAND is false, just make fake registers instead 
   of really expanding the operands, and perform the expansion in-place
   by using one of the "templates".  */

rtx
addr_for_mem_ref (struct mem_address *addr, bool really_expand)
{
  rtx address, sym, bse, idx, st, off;
  static bool templates_initialized = false;
  struct mem_addr_template *templ;

  if (addr->step && !integer_onep (addr->step))
    st = immed_double_const (TREE_INT_CST_LOW (addr->step),
			     TREE_INT_CST_HIGH (addr->step), Pmode);
  else
    st = NULL_RTX;

  if (addr->offset && !integer_zerop (addr->offset))
    off = immed_double_const (TREE_INT_CST_LOW (addr->offset),
			      TREE_INT_CST_HIGH (addr->offset), Pmode);
  else
    off = NULL_RTX;

  if (!really_expand)
    {
      /* Reuse the templates for addresses, so that we do not waste memory.  */
      if (!templates_initialized)
	{
	  unsigned i;

	  templates_initialized = true;
	  sym = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup ("test_symbol"));
	  bse = gen_raw_REG (Pmode, LAST_VIRTUAL_REGISTER + 1);
	  idx = gen_raw_REG (Pmode, LAST_VIRTUAL_REGISTER + 2);

	  for (i = 0; i < 32; i++)
	    gen_addr_rtx ((i & 16 ? sym : NULL_RTX),
			  (i & 8 ? bse : NULL_RTX),
			  (i & 4 ? idx : NULL_RTX),
			  (i & 2 ? const0_rtx : NULL_RTX),
			  (i & 1 ? const0_rtx : NULL_RTX),
			  &templates[i].ref,
			  &templates[i].step_p,
			  &templates[i].off_p);
	}

      templ = templates + TEMPL_IDX (addr->symbol, addr->base, addr->index,
				     st, off);
      if (st)
	*templ->step_p = st;
      if (off)
	*templ->off_p = off;

      return templ->ref;
    }

  /* Otherwise really expand the expressions.  */
  sym = (addr->symbol
	 ? expand_expr (build_addr (addr->symbol, current_function_decl),
			NULL_RTX, Pmode, EXPAND_NORMAL)
	 : NULL_RTX);
  bse = (addr->base
	 ? expand_expr (addr->base, NULL_RTX, Pmode, EXPAND_NORMAL)
	 : NULL_RTX);
  idx = (addr->index
	 ? expand_expr (addr->index, NULL_RTX, Pmode, EXPAND_NORMAL)
	 : NULL_RTX);

  gen_addr_rtx (sym, bse, idx, st, off, &address, NULL, NULL);
  return address;
}

/* Returns address of MEM_REF in TYPE.  */

tree
tree_mem_ref_addr (tree type, tree mem_ref)
{
  tree addr = NULL_TREE;
  tree act_elem;
  tree step = TMR_STEP (mem_ref), offset = TMR_OFFSET (mem_ref);

  act_elem = TMR_INDEX (mem_ref);
  if (act_elem)
    {
      act_elem = fold_convert (type, act_elem);

      if (step)
	act_elem = fold_build2 (MULT_EXPR, type, act_elem,
				fold_convert (type, step));
      addr = act_elem;
    }

  act_elem = TMR_BASE (mem_ref);
  if (act_elem)
    {
      act_elem = fold_convert (type, act_elem);

      if (addr)
	addr = fold_build2 (PLUS_EXPR, type, addr, act_elem);
      else
	addr = act_elem;
    }

  act_elem = TMR_SYMBOL (mem_ref);
  if (act_elem)
    {
      act_elem = fold_convert (type, build_addr (act_elem,
						 current_function_decl));
      if (addr)
	addr = fold_build2 (PLUS_EXPR, type, addr, act_elem);
      else
	addr = act_elem;
    }

  if (!zero_p (offset))
    {
      act_elem = fold_convert (type, offset);

      if (addr)
	addr = fold_build2 (PLUS_EXPR, type, addr, act_elem);
      else
	addr = act_elem;
    }

  if (!addr)
    addr = build_int_cst (type, 0);

  return addr;
}

/* Returns true if a memory reference in MODE and with parameters given by
   ADDR is valid on the current target.  */

static bool
valid_mem_ref_p (enum machine_mode mode, struct mem_address *addr)
{
  rtx address;

  address = addr_for_mem_ref (addr, false);
  if (!address)
    return false;

  return memory_address_p (mode, address);
}

/* Checks whether a TARGET_MEM_REF with type TYPE and parameters given by ADDR
   is valid on the current target and if so, creates and returns the
   TARGET_MEM_REF.  */

static tree
create_mem_ref_raw (tree type, struct mem_address *addr)
{
  if (!valid_mem_ref_p (TYPE_MODE (type), addr))
    return NULL_TREE;

  if (addr->step && integer_onep (addr->step))
    addr->step = NULL_TREE;

  if (addr->offset && zero_p (addr->offset))
    addr->offset = NULL_TREE;

  return build7 (TARGET_MEM_REF, type,
		 addr->symbol, addr->base, addr->index,
		 addr->step, addr->offset, NULL, NULL);
}

/* Returns true if OBJ is an object whose address is a link time constant.  */

static bool
fixed_address_object_p (tree obj)
{
  return (TREE_CODE (obj) == VAR_DECL
	  && (TREE_STATIC (obj)
	      || DECL_EXTERNAL (obj)));
}

/* Adds COEF * ELT to PARTS.  TYPE is the type of the address we
   construct.  */

static void
add_to_parts (struct mem_address *parts, tree type, tree elt,
	      unsigned HOST_WIDE_INT coef)
{
  /* Check if this is a symbol.  */
  if (!parts->symbol
      && coef == 1
      && TREE_CODE (elt) == ADDR_EXPR
      && fixed_address_object_p (TREE_OPERAND (elt, 0)))
    {
      parts->symbol = TREE_OPERAND (elt, 0);
      return;
    }

  if (coef != 1)
    elt = fold_build2 (MULT_EXPR, type, fold_convert (type, elt),
		       build_int_cst_type (type, coef));
  else
    elt = fold_convert (type, elt);

  if (!parts->base)
    {
      parts->base = elt;
      return;
    }

  if (!parts->index)
    {
      parts->index = elt;
      return;
    }

  /* Add ELT to base.  */
  parts->base = fold_build2 (PLUS_EXPR, type, parts->base, elt);
}

/* Finds the most expensive multiplication in ADDR that can be
   expressed in an addressing mode and move the corresponding
   element(s) to PARTS.  TYPE is the type of the address we
   construct.  */

static void
most_expensive_mult_to_index (struct mem_address *parts, tree type,
			      struct affine_tree_combination *addr)
{
  unsigned HOST_WIDE_INT best_mult = 0;
  unsigned best_mult_cost = 0, acost;
  tree mult_elt = NULL_TREE, elt;
  unsigned i, j;

  for (i = 0; i < addr->n; i++)
    {
      if (addr->coefs[i] == 1
	  || !multiplier_allowed_in_address_p (addr->coefs[i]))
	continue;
      
      acost = multiply_by_cost (addr->coefs[i], Pmode);

      if (acost > best_mult_cost)
	{
	  best_mult_cost = acost;
	  best_mult = addr->coefs[i];
	}
    }

  if (!best_mult)
    return;

  for (i = j = 0; i < addr->n; i++)
    {
      if (addr->coefs[i] != best_mult)
	{
	  addr->coefs[j] = addr->coefs[i];
	  addr->elts[j] = addr->elts[i];
	  j++;
	  continue;
	}

      elt = fold_convert (type, addr->elts[i]);
      if (!mult_elt)
	mult_elt = elt;
      else
	mult_elt = fold_build2 (PLUS_EXPR, type, mult_elt, elt);
    }
  addr->n = j;

  parts->index = mult_elt;
  parts->step = build_int_cst_type (type, best_mult);
}

/* Splits address ADDR into PARTS.
   
   TODO -- be more clever about the distribution of the elements of ADDR
   to PARTS.  Some architectures do not support anything but single
   register in address, possibly with a small integer offset; while
   create_mem_ref will simplify the address to an acceptable shape
   later, it would be a small bit more efficient to know that asking
   for complicated addressing modes is useless.  */

static void
addr_to_parts (struct affine_tree_combination *addr, tree type,
	       struct mem_address *parts)
{
  unsigned i;

  parts->symbol = NULL_TREE;
  parts->base = NULL_TREE;
  parts->index = NULL_TREE;
  parts->step = NULL_TREE;

  if (addr->offset)
    parts->offset = build_int_cst_type (type, addr->offset);
  else
    parts->offset = NULL_TREE;

  /* First move the most expensive feasible multiplication
     to index.  */
  most_expensive_mult_to_index (parts, type, addr);

  /* Then try to process the remaining elements.  */
  for (i = 0; i < addr->n; i++)
    add_to_parts (parts, type, addr->elts[i], addr->coefs[i]);
  if (addr->rest)
    add_to_parts (parts, type, addr->rest, 1);
}

/* Force the PARTS to register.  */

static void
gimplify_mem_ref_parts (block_stmt_iterator *bsi, struct mem_address *parts)
{
  if (parts->base)
    parts->base = force_gimple_operand_bsi (bsi, parts->base,
					    true, NULL_TREE);
  if (parts->index)
    parts->index = force_gimple_operand_bsi (bsi, parts->index,
					     true, NULL_TREE);
}

/* Creates and returns a TARGET_MEM_REF for address ADDR.  If necessary
   computations are emitted in front of BSI.  TYPE is the mode
   of created memory reference.  */

tree
create_mem_ref (block_stmt_iterator *bsi, tree type,
		struct affine_tree_combination *addr)
{
  tree mem_ref, tmp;
  tree addr_type = build_pointer_type (type);
  struct mem_address parts;

  addr_to_parts (addr, addr_type, &parts);
  gimplify_mem_ref_parts (bsi, &parts);
  mem_ref = create_mem_ref_raw (type, &parts);
  if (mem_ref)
    return mem_ref;

  /* The expression is too complicated.  Try making it simpler.  */

  if (parts.step && !integer_onep (parts.step))
    {
      /* Move the multiplication to index.  */
      gcc_assert (parts.index);
      parts.index = force_gimple_operand_bsi (bsi,
					      build2 (MULT_EXPR, addr_type,
						      parts.index, parts.step),
					      true, NULL_TREE);
      parts.step = NULL_TREE;
  
      mem_ref = create_mem_ref_raw (type, &parts);
      if (mem_ref)
	return mem_ref;
    }

  if (parts.symbol)
    {
      tmp = build_addr (parts.symbol, current_function_decl);
    
      /* Add the symbol to base, eventually forcing it to register.  */
      if (parts.base)
	parts.base = force_gimple_operand_bsi (bsi,
					       build2 (PLUS_EXPR, addr_type,
						       parts.base, tmp),
					       true, NULL_TREE);
      else
	parts.base = tmp;
      parts.symbol = NULL_TREE;

      mem_ref = create_mem_ref_raw (type, &parts);
      if (mem_ref)
	return mem_ref;
    }

  if (parts.base)
    {
      /* Add base to index.  */
      if (parts.index)
	parts.index = force_gimple_operand_bsi (bsi,
						build2 (PLUS_EXPR, addr_type,
							parts.base,
							parts.index),
						true, NULL_TREE);
      else
	parts.index = parts.base;
      parts.base = NULL_TREE;

      mem_ref = create_mem_ref_raw (type, &parts);
      if (mem_ref)
	return mem_ref;
    }

  if (parts.offset && !integer_zerop (parts.offset))
    {
      /* Try adding offset to index.  */
      if (parts.index)
	parts.index = force_gimple_operand_bsi (bsi, 
						build2 (PLUS_EXPR, addr_type,
							parts.index,
							parts.offset),
						true, NULL_TREE);
      else
	parts.index = parts.offset, bsi;

      parts.offset = NULL_TREE;

      mem_ref = create_mem_ref_raw (type, &parts);
      if (mem_ref)
	return mem_ref;
    }

  /* Verify that the address is in the simplest possible shape
     (only a register).  If we cannot create such a memory reference,
     something is really wrong.  */
  gcc_assert (parts.symbol == NULL_TREE);
  gcc_assert (parts.base == NULL_TREE);
  gcc_assert (!parts.step || integer_onep (parts.step));
  gcc_assert (!parts.offset || integer_zerop (parts.offset));
  gcc_unreachable ();
}

/* Copies components of the address from OP to ADDR.  */

void
get_address_description (tree op, struct mem_address *addr)
{
  addr->symbol = TMR_SYMBOL (op);
  addr->base = TMR_BASE (op);
  addr->index = TMR_INDEX (op);
  addr->step = TMR_STEP (op);
  addr->offset = TMR_OFFSET (op);
}

/* Copies the additional information attached to target_mem_ref FROM to TO.  */

void
copy_mem_ref_info (tree to, tree from)
{
  /* Copy the annotation, to preserve the aliasing information.  */
  TMR_TAG (to) = TMR_TAG (from);

  /* And the info about the original reference.  */
  TMR_ORIGINAL (to) = TMR_ORIGINAL (from);
}

/* Move constants in target_mem_ref REF to offset.  Returns the new target
   mem ref if anything changes, NULL_TREE otherwise.  */

tree
maybe_fold_tmr (tree ref)
{
  struct mem_address addr;
  bool changed = false;
  tree ret, off;

  get_address_description (ref, &addr);

  if (addr.base && TREE_CODE (addr.base) == INTEGER_CST)
    {
      if (addr.offset)
	addr.offset = fold_binary_to_constant (PLUS_EXPR, ptr_type_node,
					       addr.offset, addr.base);
      else
	addr.offset = addr.base;

      addr.base = NULL_TREE;
      changed = true;
    }

  if (addr.index && TREE_CODE (addr.index) == INTEGER_CST)
    {
      off = addr.index;
      if (addr.step)
	{
	  off = fold_binary_to_constant (MULT_EXPR, ptr_type_node,
					 off, addr.step);
	  addr.step = NULL_TREE;
	}

      if (addr.offset)
	{
	  addr.offset = fold_binary_to_constant (PLUS_EXPR, ptr_type_node,
						 addr.offset, off);
	}
      else
	addr.offset = off;

      addr.index = NULL_TREE;
      changed = true;
    }

  if (!changed)
    return NULL_TREE;
  
  ret = create_mem_ref_raw (TREE_TYPE (ref), &addr);
  if (!ret)
    return NULL_TREE;

  copy_mem_ref_info (ret, ref);
  return ret;
}

/* Dump PARTS to FILE.  */

extern void dump_mem_address (FILE *, struct mem_address *);
void
dump_mem_address (FILE *file, struct mem_address *parts)
{
  if (parts->symbol)
    {
      fprintf (file, "symbol: ");
      print_generic_expr (file, parts->symbol, TDF_SLIM);
      fprintf (file, "\n");
    }
  if (parts->base)
    {
      fprintf (file, "base: ");
      print_generic_expr (file, parts->base, TDF_SLIM);
      fprintf (file, "\n");
    }
  if (parts->index)
    {
      fprintf (file, "index: ");
      print_generic_expr (file, parts->index, TDF_SLIM);
      fprintf (file, "\n");
    }
  if (parts->step)
    {
      fprintf (file, "step: ");
      print_generic_expr (file, parts->step, TDF_SLIM);
      fprintf (file, "\n");
    }
  if (parts->offset)
    {
      fprintf (file, "offset: ");
      print_generic_expr (file, parts->offset, TDF_SLIM);
      fprintf (file, "\n");
    }
}

#include "gt-tree-ssa-address.h"
