/* Subroutines used to remove unnecessary doubleword swaps
   for p8 little-endian VSX code.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "df.h"
#include "tm_p.h"
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "tree-pass.h"
#include "rtx-vector-builder.h"

/* Analyze vector computations and remove unnecessary doubleword
   swaps (xxswapdi instructions).  This pass is performed only
   for little-endian VSX code generation.

   For this specific case, loads and stores of 4x32 and 2x64 vectors
   are inefficient.  These are implemented using the lvx2dx and
   stvx2dx instructions, which invert the order of doublewords in
   a vector register.  Thus the code generation inserts an xxswapdi
   after each such load, and prior to each such store.  (For spill
   code after register assignment, an additional xxswapdi is inserted
   following each store in order to return a hard register to its
   unpermuted value.)

   The extra xxswapdi instructions reduce performance.  This can be
   particularly bad for vectorized code.  The purpose of this pass
   is to reduce the number of xxswapdi instructions required for
   correctness.

   The primary insight is that much code that operates on vectors
   does not care about the relative order of elements in a register,
   so long as the correct memory order is preserved.  If we have
   a computation where all input values are provided by lvxd2x/xxswapdi
   sequences, all outputs are stored using xxswapdi/stvxd2x sequences,
   and all intermediate computations are pure SIMD (independent of
   element order), then all the xxswapdi's associated with the loads
   and stores may be removed.

   This pass uses some of the infrastructure and logical ideas from
   the "web" pass in web.c.  We create maximal webs of computations
   fitting the description above using union-find.  Each such web is
   then optimized by removing its unnecessary xxswapdi instructions.

   The pass is placed prior to global optimization so that we can
   perform the optimization in the safest and simplest way possible;
   that is, by replacing each xxswapdi insn with a register copy insn.
   Subsequent forward propagation will remove copies where possible.

   There are some operations sensitive to element order for which we
   can still allow the operation, provided we modify those operations.
   These include CONST_VECTORs, for which we must swap the first and
   second halves of the constant vector; and SUBREGs, for which we
   must adjust the byte offset to account for the swapped doublewords.
   A remaining opportunity would be non-immediate-form splats, for
   which we should adjust the selected lane of the input.  We should
   also make code generation adjustments for sum-across operations,
   since this is a common vectorizer reduction.

   Because we run prior to the first split, we can see loads and stores
   here that match *vsx_le_perm_{load,store}_<mode>.  These are vanilla
   vector loads and stores that have not yet been split into a permuting
   load/store and a swap.  (One way this can happen is with a builtin
   call to vec_vsx_{ld,st}.)  We can handle these as well, but rather
   than deleting a swap, we convert the load/store into a permuting
   load/store (which effectively removes the swap).  */

/* Notes on Permutes

   We do not currently handle computations that contain permutes.  There
   is a general transformation that can be performed correctly, but it
   may introduce more expensive code than it replaces.  To handle these
   would require a cost model to determine when to perform the optimization.
   This commentary records how this could be done if desired.

   The most general permute is something like this (example for V16QI):

   (vec_select:V16QI (vec_concat:V32QI (op1:V16QI) (op2:V16QI))
                     (parallel [(const_int a0) (const_int a1)
                                 ...
                                (const_int a14) (const_int a15)]))

   where a0,...,a15 are in [0,31] and select elements from op1 and op2
   to produce in the result.

   Regardless of mode, we can convert the PARALLEL to a mask of 16
   byte-element selectors.  Let's call this M, with M[i] representing
   the ith byte-element selector value.  Then if we swap doublewords
   throughout the computation, we can get correct behavior by replacing
   M with M' as follows:

    M'[i] = { (M[i]+8)%16      : M[i] in [0,15]
            { ((M[i]+8)%16)+16 : M[i] in [16,31]

   This seems promising at first, since we are just replacing one mask
   with another.  But certain masks are preferable to others.  If M
   is a mask that matches a vmrghh pattern, for example, M' certainly
   will not.  Instead of a single vmrghh, we would generate a load of
   M' and a vperm.  So we would need to know how many xxswapd's we can
   remove as a result of this transformation to determine if it's
   profitable; and preferably the logic would need to be aware of all
   the special preferable masks.

   Another form of permute is an UNSPEC_VPERM, in which the mask is
   already in a register.  In some cases, this mask may be a constant
   that we can discover with ud-chains, in which case the above
   transformation is ok.  However, the common usage here is for the
   mask to be produced by an UNSPEC_LVSL, in which case the mask 
   cannot be known at compile time.  In such a case we would have to
   generate several instructions to compute M' as above at run time,
   and a cost model is needed again.

   However, when the mask M for an UNSPEC_VPERM is loaded from the
   constant pool, we can replace M with M' as above at no cost
   beyond adding a constant pool entry.  */

/* This is based on the union-find logic in web.c.  web_entry_base is
   defined in df.h.  */
class swap_web_entry : public web_entry_base
{
 public:
  /* Pointer to the insn.  */
  rtx_insn *insn;
  /* Set if insn contains a mention of a vector register.  All other
     fields are undefined if this field is unset.  */
  unsigned int is_relevant : 1;
  /* Set if insn is a load.  */
  unsigned int is_load : 1;
  /* Set if insn is a store.  */
  unsigned int is_store : 1;
  /* Set if insn is a doubleword swap.  This can either be a register swap
     or a permuting load or store (test is_load and is_store for this).  */
  unsigned int is_swap : 1;
  /* Set if the insn has a live-in use of a parameter register.  */
  unsigned int is_live_in : 1;
  /* Set if the insn has a live-out def of a return register.  */
  unsigned int is_live_out : 1;
  /* Set if the insn contains a subreg reference of a vector register.  */
  unsigned int contains_subreg : 1;
  /* Set if the insn contains a 128-bit integer operand.  */
  unsigned int is_128_int : 1;
  /* Set if this is a call-insn.  */
  unsigned int is_call : 1;
  /* Set if this insn does not perform a vector operation for which
     element order matters, or if we know how to fix it up if it does.
     Undefined if is_swap is set.  */
  unsigned int is_swappable : 1;
  /* A nonzero value indicates what kind of special handling for this
     insn is required if doublewords are swapped.  Undefined if
     is_swappable is not set.  */
  unsigned int special_handling : 4;
  /* Set if the web represented by this entry cannot be optimized.  */
  unsigned int web_not_optimizable : 1;
  /* Set if this insn should be deleted.  */
  unsigned int will_delete : 1;
};

enum special_handling_values {
  SH_NONE = 0,
  SH_CONST_VECTOR,
  SH_SUBREG,
  SH_NOSWAP_LD,
  SH_NOSWAP_ST,
  SH_EXTRACT,
  SH_SPLAT,
  SH_XXPERMDI,
  SH_CONCAT,
  SH_VPERM
};

/* Union INSN with all insns containing definitions that reach USE.
   Detect whether USE is live-in to the current function.  */
static void
union_defs (swap_web_entry *insn_entry, rtx insn, df_ref use)
{
  struct df_link *link = DF_REF_CHAIN (use);

  if (!link)
    insn_entry[INSN_UID (insn)].is_live_in = 1;

  while (link)
    {
      if (DF_REF_IS_ARTIFICIAL (link->ref))
	insn_entry[INSN_UID (insn)].is_live_in = 1;

      if (DF_REF_INSN_INFO (link->ref))
	{
	  rtx def_insn = DF_REF_INSN (link->ref);
	  (void)unionfind_union (insn_entry + INSN_UID (insn),
				 insn_entry + INSN_UID (def_insn));
	}

      link = link->next;
    }
}

/* Union INSN with all insns containing uses reached from DEF.
   Detect whether DEF is live-out from the current function.  */
static void
union_uses (swap_web_entry *insn_entry, rtx insn, df_ref def)
{
  struct df_link *link = DF_REF_CHAIN (def);

  if (!link)
    insn_entry[INSN_UID (insn)].is_live_out = 1;

  while (link)
    {
      /* This could be an eh use or some other artificial use;
	 we treat these all the same (killing the optimization).  */
      if (DF_REF_IS_ARTIFICIAL (link->ref))
	insn_entry[INSN_UID (insn)].is_live_out = 1;

      if (DF_REF_INSN_INFO (link->ref))
	{
	  rtx use_insn = DF_REF_INSN (link->ref);
	  (void)unionfind_union (insn_entry + INSN_UID (insn),
				 insn_entry + INSN_UID (use_insn));
	}

      link = link->next;
    }
}

/* Return 1 iff INSN is a load insn, including permuting loads that
   represent an lvxd2x instruction; else return 0.  */
static unsigned int
insn_is_load_p (rtx insn)
{
  rtx body = PATTERN (insn);

  if (GET_CODE (body) == SET)
    {
      if (GET_CODE (SET_SRC (body)) == MEM)
	return 1;

      if (GET_CODE (SET_SRC (body)) == VEC_SELECT
	  && GET_CODE (XEXP (SET_SRC (body), 0)) == MEM)
	return 1;

      return 0;
    }

  if (GET_CODE (body) != PARALLEL)
    return 0;

  rtx set = XVECEXP (body, 0, 0);

  if (GET_CODE (set) == SET && GET_CODE (SET_SRC (set)) == MEM)
    return 1;

  return 0;
}

/* Return 1 iff INSN is a store insn, including permuting stores that
   represent an stvxd2x instruction; else return 0.  */
static unsigned int
insn_is_store_p (rtx insn)
{
  rtx body = PATTERN (insn);
  if (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == MEM)
    return 1;
  if (GET_CODE (body) != PARALLEL)
    return 0;
  rtx set = XVECEXP (body, 0, 0);
  if (GET_CODE (set) == SET && GET_CODE (SET_DEST (set)) == MEM)
    return 1;
  return 0;
}

/* Return 1 iff INSN swaps doublewords.  This may be a reg-reg swap,
   a permuting load, or a permuting store.  */
static unsigned int
insn_is_swap_p (rtx insn)
{
  rtx body = PATTERN (insn);
  if (GET_CODE (body) != SET)
    return 0;
  rtx rhs = SET_SRC (body);
  if (GET_CODE (rhs) != VEC_SELECT)
    return 0;
  rtx parallel = XEXP (rhs, 1);
  if (GET_CODE (parallel) != PARALLEL)
    return 0;
  unsigned int len = XVECLEN (parallel, 0);
  if (len != 2 && len != 4 && len != 8 && len != 16)
    return 0;
  for (unsigned int i = 0; i < len / 2; ++i)
    {
      rtx op = XVECEXP (parallel, 0, i);
      if (GET_CODE (op) != CONST_INT || INTVAL (op) != len / 2 + i)
	return 0;
    }
  for (unsigned int i = len / 2; i < len; ++i)
    {
      rtx op = XVECEXP (parallel, 0, i);
      if (GET_CODE (op) != CONST_INT || INTVAL (op) != i - len / 2)
	return 0;
    }
  return 1;
}

/* Return true iff EXPR represents the sum of two registers.  */
bool
rs6000_sum_of_two_registers_p (const_rtx expr)
{
  if (GET_CODE (expr) == PLUS)
    {
      const_rtx operand1 = XEXP (expr, 0);
      const_rtx operand2 = XEXP (expr, 1);
      return (REG_P (operand1) && REG_P (operand2));
    }
  return false;
}

/* Return true iff EXPR represents an address expression that masks off
   the low-order 4 bits in the style of an lvx or stvx rtl pattern.  */
bool
rs6000_quadword_masked_address_p (const_rtx expr)
{
  if (GET_CODE (expr) == AND)
    {
      const_rtx operand1 = XEXP (expr, 0);
      const_rtx operand2 = XEXP (expr, 1);
      if ((REG_P (operand1) || rs6000_sum_of_two_registers_p (operand1))
	  && CONST_SCALAR_INT_P (operand2) && INTVAL (operand2) == -16)
	return true;
    }
  return false;
}

/* Return TRUE if INSN represents a swap of a swapped load from memory
   and the memory address is quad-word aligned.  */
static bool
quad_aligned_load_p (swap_web_entry *insn_entry, rtx_insn *insn)
{
  unsigned uid = INSN_UID (insn);
  if (!insn_entry[uid].is_swap || insn_entry[uid].is_load)
    return false;

  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);

  /* Since insn is known to represent a swap instruction, we know it
     "uses" only one input variable.  */
  df_ref use = DF_INSN_INFO_USES (insn_info);

  /* Figure out where this input variable is defined.  */
  struct df_link *def_link = DF_REF_CHAIN (use);

  /* If there is no definition or the definition is artificial or there are
     multiple definitions, punt.  */
  if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref)
      || def_link->next)
    return false;

  rtx def_insn = DF_REF_INSN (def_link->ref);
  unsigned uid2 = INSN_UID (def_insn);
  /* We're looking for a load-with-swap insn.  If this is not that,
     return false.  */
  if (!insn_entry[uid2].is_load || !insn_entry[uid2].is_swap)
    return false;

  /* If the source of the rtl def is not a set from memory, return
     false.  */
  rtx body = PATTERN (def_insn);
  if (GET_CODE (body) != SET
      || GET_CODE (SET_SRC (body)) != VEC_SELECT
      || GET_CODE (XEXP (SET_SRC (body), 0)) != MEM)
    return false;

  rtx mem = XEXP (SET_SRC (body), 0);
  rtx base_reg = XEXP (mem, 0);
  return ((REG_P (base_reg) || rs6000_sum_of_two_registers_p (base_reg))
	  && MEM_ALIGN (mem) >= 128) ? true : false;
}

/* Return TRUE if INSN represents a store-with-swap of a swapped value
   and the memory address is quad-word aligned.  */
static bool
quad_aligned_store_p (swap_web_entry *insn_entry, rtx_insn *insn)
{
  unsigned uid = INSN_UID (insn);
  if (!insn_entry[uid].is_swap || !insn_entry[uid].is_store)
    return false;

  rtx body = PATTERN (insn);
  rtx dest_address = XEXP (SET_DEST (body), 0);
  rtx swap_reg = XEXP (SET_SRC (body), 0);

  /* If the base address for the memory expression is not represented
     by a single register and is not the sum of two registers, punt.  */
  if (!REG_P (dest_address) && !rs6000_sum_of_two_registers_p (dest_address))
    return false;

  /* Confirm that the value to be stored is produced by a swap
     instruction.  */
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref use;
  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);

      /* If this is not the definition of the candidate swap register,
	 then skip it.  I am interested in a different definition.  */
      if (!rtx_equal_p (DF_REF_REG (use), swap_reg))
	continue;

      /* If there is no def or the def is artifical or there are
	 multiple defs, punt.  */
      if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref)
	  || def_link->next)
	return false;

      rtx def_insn = DF_REF_INSN (def_link->ref);
      unsigned uid2 = INSN_UID (def_insn);

      /* If this source value is not a simple swap, return false */
      if (!insn_entry[uid2].is_swap || insn_entry[uid2].is_load
	  || insn_entry[uid2].is_store)
	return false;

      /* I've processed the use that I care about, so break out of
	 this loop.  */
      break;
    }

  /* At this point, we know the source data comes from a swap.  The
     remaining question is whether the memory address is aligned.  */
  rtx set = single_set (insn);
  if (set)
    {
      rtx dest = SET_DEST (set);
      if (MEM_P (dest))
	return (MEM_ALIGN (dest) >= 128);
    }
  return false;
}

/* Return 1 iff UID, known to reference a swap, is both fed by a load
   and a feeder of a store.  */
static unsigned int
swap_feeds_both_load_and_store (swap_web_entry *insn_entry)
{
  rtx insn = insn_entry->insn;
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref def, use;
  struct df_link *link = 0;
  rtx_insn *load = 0, *store = 0;
  bool fed_by_load = 0;
  bool feeds_store = 0;

  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      link = DF_REF_CHAIN (use);
      load = DF_REF_INSN (link->ref);
      if (insn_is_load_p (load) && insn_is_swap_p (load))
	fed_by_load = 1;
    }

  FOR_EACH_INSN_INFO_DEF (def, insn_info)
    {
      link = DF_REF_CHAIN (def);
      store = DF_REF_INSN (link->ref);
      if (insn_is_store_p (store) && insn_is_swap_p (store))
	feeds_store = 1;
    }

  return fed_by_load && feeds_store;
}

/* Return TRUE if insn is a swap fed by a load from the constant pool.  */
static bool
const_load_sequence_p (swap_web_entry *insn_entry, rtx insn)
{
  unsigned uid = INSN_UID (insn);
  if (!insn_entry[uid].is_swap || insn_entry[uid].is_load)
    return false;

  const_rtx tocrel_base;

  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref use;

  /* Iterate over the definitions that are used by this insn.  Since
     this is known to be a swap insn, expect only one used definnition.  */
  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);

      /* If there is no def or the def is artificial or there are
	 multiple defs, punt.  */
      if (!def_link || !def_link->ref || DF_REF_IS_ARTIFICIAL (def_link->ref)
	  || def_link->next)
	return false;

      rtx def_insn = DF_REF_INSN (def_link->ref);
      unsigned uid2 = INSN_UID (def_insn);
      /* If this is not a load or is not a swap, return false.  */
      if (!insn_entry[uid2].is_load || !insn_entry[uid2].is_swap)
	return false;

      /* If the source of the rtl def is not a set from memory, return
	 false.  */
      rtx body = PATTERN (def_insn);
      if (GET_CODE (body) != SET
	  || GET_CODE (SET_SRC (body)) != VEC_SELECT
	  || GET_CODE (XEXP (SET_SRC (body), 0)) != MEM)
	return false;

      rtx mem = XEXP (SET_SRC (body), 0);
      rtx base_reg = XEXP (mem, 0);
      /* If the base address for the memory expression is not
	 represented by a register, punt.  */
      if (!REG_P (base_reg))
	return false;

      df_ref base_use;
      insn_info = DF_INSN_INFO_GET (def_insn);
      FOR_EACH_INSN_INFO_USE (base_use, insn_info)
	{
	  /* If base_use does not represent base_reg, look for another
	     use.  */
	  if (!rtx_equal_p (DF_REF_REG (base_use), base_reg))
	    continue;

	  struct df_link *base_def_link = DF_REF_CHAIN (base_use);
	  if (!base_def_link || base_def_link->next)
	    return false;

	  /* Constants held on the stack are not "true" constants
	     because their values are not part of the static load
	     image.  If this constant's base reference is a stack
	     or frame pointer, it is seen as an artificial
	     reference.  */
	  if (DF_REF_IS_ARTIFICIAL (base_def_link->ref))
	    return false;

	  rtx tocrel_insn = DF_REF_INSN (base_def_link->ref);
	  rtx tocrel_body = PATTERN (tocrel_insn);
	  rtx base, offset;
	  if (GET_CODE (tocrel_body) != SET)
	    return false;
	  /* There is an extra level of indirection for small/large
	     code models.  */
	  rtx tocrel_expr = SET_SRC (tocrel_body);
	  if (GET_CODE (tocrel_expr) == MEM)
	    tocrel_expr = XEXP (tocrel_expr, 0);
	  if (!toc_relative_expr_p (tocrel_expr, false, &tocrel_base, NULL))
	    return false;
	  split_const (XVECEXP (tocrel_base, 0, 0), &base, &offset);

	  if (GET_CODE (base) != SYMBOL_REF || !CONSTANT_POOL_ADDRESS_P (base))
	    return false;
	  else
	    {
	      /* FIXME: The conditions under which
	          ((GET_CODE (const_vector) == SYMBOL_REF) &&
	           !CONSTANT_POOL_ADDRESS_P (const_vector))
	         are not well understood.  This code prevents
	         an internal compiler error which will occur in
	         replace_swapped_load_constant () if we were to return
	         true.  Some day, we should figure out how to properly
	         handle this condition in
	         replace_swapped_load_constant () and then we can
	         remove this special test.  */
	      rtx const_vector = get_pool_constant (base);
	      if (GET_CODE (const_vector) == SYMBOL_REF
		  && CONSTANT_POOL_ADDRESS_P (const_vector))
		const_vector = get_pool_constant (const_vector);
	      if (GET_CODE (const_vector) != CONST_VECTOR)
		return false;
	    }
	}
    }
  return true;
}

/* Return TRUE iff OP matches a V2DF reduction pattern.  See the
   definition of vsx_reduc_<VEC_reduc_name>_v2df in vsx.md.  */
static bool
v2df_reduction_p (rtx op)
{
  if (GET_MODE (op) != V2DFmode)
    return false;
  
  enum rtx_code code = GET_CODE (op);
  if (code != PLUS && code != SMIN && code != SMAX)
    return false;

  rtx concat = XEXP (op, 0);
  if (GET_CODE (concat) != VEC_CONCAT)
    return false;

  rtx select0 = XEXP (concat, 0);
  rtx select1 = XEXP (concat, 1);
  if (GET_CODE (select0) != VEC_SELECT || GET_CODE (select1) != VEC_SELECT)
    return false;

  rtx reg0 = XEXP (select0, 0);
  rtx reg1 = XEXP (select1, 0);
  if (!rtx_equal_p (reg0, reg1) || !REG_P (reg0))
    return false;

  rtx parallel0 = XEXP (select0, 1);
  rtx parallel1 = XEXP (select1, 1);
  if (GET_CODE (parallel0) != PARALLEL || GET_CODE (parallel1) != PARALLEL)
    return false;

  if (!rtx_equal_p (XVECEXP (parallel0, 0, 0), const1_rtx)
      || !rtx_equal_p (XVECEXP (parallel1, 0, 0), const0_rtx))
    return false;

  return true;
}

/* Return 1 iff OP is an operand that will not be affected by having
   vector doublewords swapped in memory.  */
static unsigned int
rtx_is_swappable_p (rtx op, unsigned int *special)
{
  enum rtx_code code = GET_CODE (op);
  int i, j;
  rtx parallel;

  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CLOBBER:
    case REG:
      return 1;

    case VEC_CONCAT:
    case ASM_INPUT:
    case ASM_OPERANDS:
      return 0;

    case CONST_VECTOR:
      {
	*special = SH_CONST_VECTOR;
	return 1;
      }

    case VEC_DUPLICATE:
      /* Opportunity: If XEXP (op, 0) has the same mode as the result,
	 and XEXP (op, 1) is a PARALLEL with a single QImode const int,
	 it represents a vector splat for which we can do special
	 handling.  */
      if (GET_CODE (XEXP (op, 0)) == CONST_INT)
	return 1;
      else if (REG_P (XEXP (op, 0))
	       && GET_MODE_INNER (GET_MODE (op)) == GET_MODE (XEXP (op, 0)))
	/* This catches V2DF and V2DI splat, at a minimum.  */
	return 1;
      else if (GET_CODE (XEXP (op, 0)) == TRUNCATE
	       && REG_P (XEXP (XEXP (op, 0), 0))
	       && GET_MODE_INNER (GET_MODE (op)) == GET_MODE (XEXP (op, 0)))
	/* This catches splat of a truncated value.  */
	return 1;
      else if (GET_CODE (XEXP (op, 0)) == VEC_SELECT)
	/* If the duplicated item is from a select, defer to the select
	   processing to see if we can change the lane for the splat.  */
	return rtx_is_swappable_p (XEXP (op, 0), special);
      else
	return 0;

    case VEC_SELECT:
      /* A vec_extract operation is ok if we change the lane.  */
      if (GET_CODE (XEXP (op, 0)) == REG
	  && GET_MODE_INNER (GET_MODE (XEXP (op, 0))) == GET_MODE (op)
	  && GET_CODE ((parallel = XEXP (op, 1))) == PARALLEL
	  && XVECLEN (parallel, 0) == 1
	  && GET_CODE (XVECEXP (parallel, 0, 0)) == CONST_INT)
	{
	  *special = SH_EXTRACT;
	  return 1;
	}
      /* An XXPERMDI is ok if we adjust the lanes.  Note that if the
	 XXPERMDI is a swap operation, it will be identified by
	 insn_is_swap_p and therefore we won't get here.  */
      else if (GET_CODE (XEXP (op, 0)) == VEC_CONCAT
	       && (GET_MODE (XEXP (op, 0)) == V4DFmode
		   || GET_MODE (XEXP (op, 0)) == V4DImode)
	       && GET_CODE ((parallel = XEXP (op, 1))) == PARALLEL
	       && XVECLEN (parallel, 0) == 2
	       && GET_CODE (XVECEXP (parallel, 0, 0)) == CONST_INT
	       && GET_CODE (XVECEXP (parallel, 0, 1)) == CONST_INT)
	{
	  *special = SH_XXPERMDI;
	  return 1;
	}
      else if (v2df_reduction_p (op))
	return 1;
      else
	return 0;

    case UNSPEC:
      {
	/* Various operations are unsafe for this optimization, at least
	   without significant additional work.  Permutes are obviously
	   problematic, as both the permute control vector and the ordering
	   of the target values are invalidated by doubleword swapping.
	   Vector pack and unpack modify the number of vector lanes.
	   Merge-high/low will not operate correctly on swapped operands.
	   Vector shifts across element boundaries are clearly uncool,
	   as are vector select and concatenate operations.  Vector
	   sum-across instructions define one operand with a specific
	   order-dependent element, so additional fixup code would be
	   needed to make those work.  Vector set and non-immediate-form
	   vector splat are element-order sensitive.  A few of these
	   cases might be workable with special handling if required.
	   Adding cost modeling would be appropriate in some cases.  */
	int val = XINT (op, 1);
	switch (val)
	  {
	  default:
	    break;
	  case UNSPEC_VBPERMQ:
	  case UNSPEC_VMRGH_DIRECT:
	  case UNSPEC_VMRGL_DIRECT:
	  case UNSPEC_VPACK_SIGN_SIGN_SAT:
	  case UNSPEC_VPACK_SIGN_UNS_SAT:
	  case UNSPEC_VPACK_UNS_UNS_MOD:
	  case UNSPEC_VPACK_UNS_UNS_MOD_DIRECT:
	  case UNSPEC_VPACK_UNS_UNS_SAT:
	  case UNSPEC_VPERM:
	  case UNSPEC_VPERM_UNS:
	  case UNSPEC_VPERMHI:
	  case UNSPEC_VPERMSI:
	  case UNSPEC_VPERMXOR:
	  case UNSPEC_VPKPX:
	  case UNSPEC_VSLDOI:
	  case UNSPEC_VSLO:
	  case UNSPEC_VSRO:
	  case UNSPEC_VSUM2SWS:
	  case UNSPEC_VSUM4S:
	  case UNSPEC_VSUM4UBS:
	  case UNSPEC_VSUMSWS:
	  case UNSPEC_VSUMSWS_DIRECT:
	  case UNSPEC_VSX_CONCAT:
	  case UNSPEC_VSX_CVDPSPN:
	  case UNSPEC_VSX_CVSPDP:
	  case UNSPEC_VSX_CVSPDPN:
	  case UNSPEC_VSX_EXTRACT:
	  case UNSPEC_VSX_SET:
	  case UNSPEC_VSX_SLDWI:
	  case UNSPEC_VSX_VSLO:
	  case UNSPEC_VUNPACK_HI_SIGN:
	  case UNSPEC_VUNPACK_HI_SIGN_DIRECT:
	  case UNSPEC_VUNPACK_LO_SIGN:
	  case UNSPEC_VUNPACK_LO_SIGN_DIRECT:
	  case UNSPEC_VUPKHPX:
	  case UNSPEC_VUPKHS_V4SF:
	  case UNSPEC_VUPKHU_V4SF:
	  case UNSPEC_VUPKLPX:
	  case UNSPEC_VUPKLS_V4SF:
	  case UNSPEC_VUPKLU_V4SF:
	    return 0;
	  case UNSPEC_VSPLT_DIRECT:
	  case UNSPEC_VSX_XXSPLTD:
	    *special = SH_SPLAT;
	    return 1;
	  case UNSPEC_REDUC_PLUS:
	  case UNSPEC_REDUC:
	    return 1;
	  }
      }

    default:
      break;
    }

  const char *fmt = GET_RTX_FORMAT (code);
  int ok = 1;

  for (i = 0; i < GET_RTX_LENGTH (code); ++i)
    if (fmt[i] == 'e' || fmt[i] == 'u')
      {
	unsigned int special_op = SH_NONE;
	ok &= rtx_is_swappable_p (XEXP (op, i), &special_op);
	if (special_op == SH_NONE)
	  continue;
	/* Ensure we never have two kinds of special handling
	   for the same insn.  */
	if (*special != SH_NONE && *special != special_op)
	  return 0;
	*special = special_op;
      }
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (op, i); ++j)
	{
	  unsigned int special_op = SH_NONE;
	  ok &= rtx_is_swappable_p (XVECEXP (op, i, j), &special_op);
	  if (special_op == SH_NONE)
	    continue;
	  /* Ensure we never have two kinds of special handling
	     for the same insn.  */
	  if (*special != SH_NONE && *special != special_op)
	    return 0;
	  *special = special_op;
	}

  return ok;
}

/* Return 1 iff INSN is an operand that will not be affected by
   having vector doublewords swapped in memory (in which case
   *SPECIAL is unchanged), or that can be modified to be correct
   if vector doublewords are swapped in memory (in which case
   *SPECIAL is changed to a value indicating how).  */
static unsigned int
insn_is_swappable_p (swap_web_entry *insn_entry, rtx insn,
		     unsigned int *special)
{
  /* Calls are always bad.  */
  if (GET_CODE (insn) == CALL_INSN)
    return 0;

  /* Loads and stores seen here are not permuting, but we can still
     fix them up by converting them to permuting ones.  Exceptions:
     UNSPEC_LVE, UNSPEC_LVX, and UNSPEC_STVX, which have a PARALLEL
     body instead of a SET; and UNSPEC_STVE, which has an UNSPEC
     for the SET source.  Also we must now make an exception for lvx
     and stvx when they are not in the UNSPEC_LVX/STVX form (with the
     explicit "& -16") since this leads to unrecognizable insns.  */
  rtx body = PATTERN (insn);
  int i = INSN_UID (insn);

  if (insn_entry[i].is_load)
    {
      if (GET_CODE (body) == SET)
	{
	  rtx rhs = SET_SRC (body);
	  /* Even without a swap, the RHS might be a vec_select for, say,
	     a byte-reversing load.  */
	  if (GET_CODE (rhs) != MEM)
	    return 0;
	  if (GET_CODE (XEXP (rhs, 0)) == AND)
	    return 0;

	  *special = SH_NOSWAP_LD;
	  return 1;
	}
      else
	return 0;
    }

  if (insn_entry[i].is_store)
    {
      if (GET_CODE (body) == SET
	  && GET_CODE (SET_SRC (body)) != UNSPEC
	  && GET_CODE (SET_SRC (body)) != VEC_SELECT)
	{
	  rtx lhs = SET_DEST (body);
	  /* Even without a swap, the RHS might be a vec_select for, say,
	     a byte-reversing store.  */
	  if (GET_CODE (lhs) != MEM)
	    return 0;
	  if (GET_CODE (XEXP (lhs, 0)) == AND)
	    return 0;
	  
	  *special = SH_NOSWAP_ST;
	  return 1;
	}
      else
	return 0;
    }

  /* A convert to single precision can be left as is provided that
     all of its uses are in xxspltw instructions that splat BE element
     zero.  */
  if (GET_CODE (body) == SET
      && GET_CODE (SET_SRC (body)) == UNSPEC
      && XINT (SET_SRC (body), 1) == UNSPEC_VSX_CVDPSPN)
    {
      df_ref def;
      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);

      FOR_EACH_INSN_INFO_DEF (def, insn_info)
	{
	  struct df_link *link = DF_REF_CHAIN (def);
	  if (!link)
	    return 0;

	  for (; link; link = link->next) {
	    rtx use_insn = DF_REF_INSN (link->ref);
	    rtx use_body = PATTERN (use_insn);
	    if (GET_CODE (use_body) != SET
		|| GET_CODE (SET_SRC (use_body)) != UNSPEC
		|| XINT (SET_SRC (use_body), 1) != UNSPEC_VSX_XXSPLTW
		|| XVECEXP (SET_SRC (use_body), 0, 1) != const0_rtx)
	      return 0;
	  }
	}

      return 1;
    }

  /* A concatenation of two doublewords is ok if we reverse the
     order of the inputs.  */
  if (GET_CODE (body) == SET
      && GET_CODE (SET_SRC (body)) == VEC_CONCAT
      && (GET_MODE (SET_SRC (body)) == V2DFmode
	  || GET_MODE (SET_SRC (body)) == V2DImode))
    {
      *special = SH_CONCAT;
      return 1;
    }

  /* V2DF reductions are always swappable.  */
  if (GET_CODE (body) == PARALLEL)
    {
      rtx expr = XVECEXP (body, 0, 0);
      if (GET_CODE (expr) == SET
	  && v2df_reduction_p (SET_SRC (expr)))
	return 1;
    }

  /* An UNSPEC_VPERM is ok if the mask operand is loaded from the
     constant pool.  */
  if (GET_CODE (body) == SET
      && GET_CODE (SET_SRC (body)) == UNSPEC
      && XINT (SET_SRC (body), 1) == UNSPEC_VPERM
      && XVECLEN (SET_SRC (body), 0) == 3
      && GET_CODE (XVECEXP (SET_SRC (body), 0, 2)) == REG)
    {
      rtx mask_reg = XVECEXP (SET_SRC (body), 0, 2);
      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
      df_ref use;
      FOR_EACH_INSN_INFO_USE (use, insn_info)
	if (rtx_equal_p (DF_REF_REG (use), mask_reg))
	  {
	    struct df_link *def_link = DF_REF_CHAIN (use);
	    /* Punt if multiple definitions for this reg.  */
	    if (def_link && !def_link->next &&
		const_load_sequence_p (insn_entry,
				       DF_REF_INSN (def_link->ref)))
	      {
		*special = SH_VPERM;
		return 1;
	      }
	  }
    }

  /* Otherwise check the operands for vector lane violations.  */
  return rtx_is_swappable_p (body, special);
}

enum chain_purpose { FOR_LOADS, FOR_STORES };

/* Return true if the UD or DU chain headed by LINK is non-empty,
   and every entry on the chain references an insn that is a
   register swap.  Furthermore, if PURPOSE is FOR_LOADS, each such
   register swap must have only permuting loads as reaching defs.
   If PURPOSE is FOR_STORES, each such register swap must have only
   register swaps or permuting stores as reached uses.  */
static bool
chain_contains_only_swaps (swap_web_entry *insn_entry, struct df_link *link,
			   enum chain_purpose purpose)
{
  if (!link)
    return false;

  for (; link; link = link->next)
    {
      if (!ALTIVEC_OR_VSX_VECTOR_MODE (GET_MODE (DF_REF_REG (link->ref))))
	continue;

      if (DF_REF_IS_ARTIFICIAL (link->ref))
	return false;

      rtx reached_insn = DF_REF_INSN (link->ref);
      unsigned uid = INSN_UID (reached_insn);
      struct df_insn_info *insn_info = DF_INSN_INFO_GET (reached_insn);

      if (!insn_entry[uid].is_swap || insn_entry[uid].is_load
	  || insn_entry[uid].is_store)
	return false;

      if (purpose == FOR_LOADS)
	{
	  df_ref use;
	  FOR_EACH_INSN_INFO_USE (use, insn_info)
	    {
	      struct df_link *swap_link = DF_REF_CHAIN (use);

	      while (swap_link)
		{
		  if (DF_REF_IS_ARTIFICIAL (link->ref))
		    return false;

		  rtx swap_def_insn = DF_REF_INSN (swap_link->ref);
		  unsigned uid2 = INSN_UID (swap_def_insn);

		  /* Only permuting loads are allowed.  */
		  if (!insn_entry[uid2].is_swap || !insn_entry[uid2].is_load)
		    return false;

		  swap_link = swap_link->next;
		}
	    }
	}
      else if (purpose == FOR_STORES)
	{
	  df_ref def;
	  FOR_EACH_INSN_INFO_DEF (def, insn_info)
	    {
	      struct df_link *swap_link = DF_REF_CHAIN (def);

	      while (swap_link)
		{
		  if (DF_REF_IS_ARTIFICIAL (link->ref))
		    return false;

		  rtx swap_use_insn = DF_REF_INSN (swap_link->ref);
		  unsigned uid2 = INSN_UID (swap_use_insn);

		  /* Permuting stores or register swaps are allowed.  */
		  if (!insn_entry[uid2].is_swap || insn_entry[uid2].is_load)
		    return false;

		  swap_link = swap_link->next;
		}
	    }
	}
    }

  return true;
}

/* Mark the xxswapdi instructions associated with permuting loads and
   stores for removal.  Note that we only flag them for deletion here,
   as there is a possibility of a swap being reached from multiple
   loads, etc.  */
static void
mark_swaps_for_removal (swap_web_entry *insn_entry, unsigned int i)
{
  rtx insn = insn_entry[i].insn;
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);

  if (insn_entry[i].is_load)
    {
      df_ref def;
      FOR_EACH_INSN_INFO_DEF (def, insn_info)
	{
	  struct df_link *link = DF_REF_CHAIN (def);

	  /* We know by now that these are swaps, so we can delete
	     them confidently.  */
	  while (link)
	    {
	      rtx use_insn = DF_REF_INSN (link->ref);
	      insn_entry[INSN_UID (use_insn)].will_delete = 1;
	      link = link->next;
	    }
	}
    }
  else if (insn_entry[i].is_store)
    {
      df_ref use;
      FOR_EACH_INSN_INFO_USE (use, insn_info)
	{
	  /* Ignore uses for addressability.  */
	  machine_mode mode = GET_MODE (DF_REF_REG (use));
	  if (!ALTIVEC_OR_VSX_VECTOR_MODE (mode))
	    continue;

	  struct df_link *link = DF_REF_CHAIN (use);

	  /* We know by now that these are swaps, so we can delete
	     them confidently.  */
	  while (link)
	    {
	      rtx def_insn = DF_REF_INSN (link->ref);
	      insn_entry[INSN_UID (def_insn)].will_delete = 1;
	      link = link->next;
	    }
	}
    }
}

/* *OP_PTR is either a CONST_VECTOR or an expression containing one.
   Swap the first half of the vector with the second in the first
   case.  Recurse to find it in the second.  */
static void
swap_const_vector_halves (rtx *op_ptr)
{
  int i;
  rtx op = *op_ptr;
  enum rtx_code code = GET_CODE (op);
  if (GET_CODE (op) == CONST_VECTOR)
    {
      int units = GET_MODE_NUNITS (GET_MODE (op));
      rtx_vector_builder builder (GET_MODE (op), units, 1);
      for (i = 0; i < units / 2; ++i)
	builder.quick_push (CONST_VECTOR_ELT (op, i + units / 2));
      for (i = 0; i < units / 2; ++i)
	builder.quick_push (CONST_VECTOR_ELT (op, i));
      *op_ptr = builder.build ();
    }
  else
    {
      int j;
      const char *fmt = GET_RTX_FORMAT (code);
      for (i = 0; i < GET_RTX_LENGTH (code); ++i)
	if (fmt[i] == 'e' || fmt[i] == 'u')
	  swap_const_vector_halves (&XEXP (op, i));
	else if (fmt[i] == 'E')
	  for (j = 0; j < XVECLEN (op, i); ++j)
	    swap_const_vector_halves (&XVECEXP (op, i, j));
    }
}

/* Find all subregs of a vector expression that perform a narrowing,
   and adjust the subreg index to account for doubleword swapping.  */
static void
adjust_subreg_index (rtx op)
{
  enum rtx_code code = GET_CODE (op);
  if (code == SUBREG
      && (GET_MODE_SIZE (GET_MODE (op))
	  < GET_MODE_SIZE (GET_MODE (XEXP (op, 0)))))
    {
      unsigned int index = SUBREG_BYTE (op);
      if (index < 8)
	index += 8;
      else
	index -= 8;
      SUBREG_BYTE (op) = index;
    }

  const char *fmt = GET_RTX_FORMAT (code);
  int i,j;
  for (i = 0; i < GET_RTX_LENGTH (code); ++i)
    if (fmt[i] == 'e' || fmt[i] == 'u')
      adjust_subreg_index (XEXP (op, i));
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (op, i); ++j)
	adjust_subreg_index (XVECEXP (op, i, j));
}

/* Convert the non-permuting load INSN to a permuting one.  */
static void
permute_load (rtx_insn *insn)
{
  rtx body = PATTERN (insn);
  rtx mem_op = SET_SRC (body);
  rtx tgt_reg = SET_DEST (body);
  machine_mode mode = GET_MODE (tgt_reg);
  int n_elts = GET_MODE_NUNITS (mode);
  int half_elts = n_elts / 2;
  rtx par = gen_rtx_PARALLEL (mode, rtvec_alloc (n_elts));
  int i, j;
  for (i = 0, j = half_elts; i < half_elts; ++i, ++j)
    XVECEXP (par, 0, i) = GEN_INT (j);
  for (i = half_elts, j = 0; j < half_elts; ++i, ++j)
    XVECEXP (par, 0, i) = GEN_INT (j);
  rtx sel = gen_rtx_VEC_SELECT (mode, mem_op, par);
  SET_SRC (body) = sel;
  INSN_CODE (insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (insn);

  if (dump_file)
    fprintf (dump_file, "Replacing load %d with permuted load\n",
	     INSN_UID (insn));
}

/* Convert the non-permuting store INSN to a permuting one.  */
static void
permute_store (rtx_insn *insn)
{
  rtx body = PATTERN (insn);
  rtx src_reg = SET_SRC (body);
  machine_mode mode = GET_MODE (src_reg);
  int n_elts = GET_MODE_NUNITS (mode);
  int half_elts = n_elts / 2;
  rtx par = gen_rtx_PARALLEL (mode, rtvec_alloc (n_elts));
  int i, j;
  for (i = 0, j = half_elts; i < half_elts; ++i, ++j)
    XVECEXP (par, 0, i) = GEN_INT (j);
  for (i = half_elts, j = 0; j < half_elts; ++i, ++j)
    XVECEXP (par, 0, i) = GEN_INT (j);
  rtx sel = gen_rtx_VEC_SELECT (mode, src_reg, par);
  SET_SRC (body) = sel;
  INSN_CODE (insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (insn);

  if (dump_file)
    fprintf (dump_file, "Replacing store %d with permuted store\n",
	     INSN_UID (insn));
}

/* Given OP that contains a vector extract operation, adjust the index
   of the extracted lane to account for the doubleword swap.  */
static void
adjust_extract (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  if (GET_CODE (pattern) == PARALLEL)
    pattern = XVECEXP (pattern, 0, 0);
  rtx src = SET_SRC (pattern);
  /* The vec_select may be wrapped in a vec_duplicate for a splat, so
     account for that.  */
  rtx sel = GET_CODE (src) == VEC_DUPLICATE ? XEXP (src, 0) : src;
  rtx par = XEXP (sel, 1);
  int half_elts = GET_MODE_NUNITS (GET_MODE (XEXP (sel, 0))) >> 1;
  int lane = INTVAL (XVECEXP (par, 0, 0));
  lane = lane >= half_elts ? lane - half_elts : lane + half_elts;
  XVECEXP (par, 0, 0) = GEN_INT (lane);
  INSN_CODE (insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (insn);

  if (dump_file)
    fprintf (dump_file, "Changing lane for extract %d\n", INSN_UID (insn));
}

/* Given OP that contains a vector direct-splat operation, adjust the index
   of the source lane to account for the doubleword swap.  */
static void
adjust_splat (rtx_insn *insn)
{
  rtx body = PATTERN (insn);
  rtx unspec = XEXP (body, 1);
  int half_elts = GET_MODE_NUNITS (GET_MODE (unspec)) >> 1;
  int lane = INTVAL (XVECEXP (unspec, 0, 1));
  lane = lane >= half_elts ? lane - half_elts : lane + half_elts;
  XVECEXP (unspec, 0, 1) = GEN_INT (lane);
  INSN_CODE (insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (insn);

  if (dump_file)
    fprintf (dump_file, "Changing lane for splat %d\n", INSN_UID (insn));
}

/* Given OP that contains an XXPERMDI operation (that is not a doubleword
   swap), reverse the order of the source operands and adjust the indices
   of the source lanes to account for doubleword reversal.  */
static void
adjust_xxpermdi (rtx_insn *insn)
{
  rtx set = PATTERN (insn);
  rtx select = XEXP (set, 1);
  rtx concat = XEXP (select, 0);
  rtx src0 = XEXP (concat, 0);
  XEXP (concat, 0) = XEXP (concat, 1);
  XEXP (concat, 1) = src0;
  rtx parallel = XEXP (select, 1);
  int lane0 = INTVAL (XVECEXP (parallel, 0, 0));
  int lane1 = INTVAL (XVECEXP (parallel, 0, 1));
  int new_lane0 = 3 - lane1;
  int new_lane1 = 3 - lane0;
  XVECEXP (parallel, 0, 0) = GEN_INT (new_lane0);
  XVECEXP (parallel, 0, 1) = GEN_INT (new_lane1);
  INSN_CODE (insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (insn);

  if (dump_file)
    fprintf (dump_file, "Changing lanes for xxpermdi %d\n", INSN_UID (insn));
}

/* Given OP that contains a VEC_CONCAT operation of two doublewords,
   reverse the order of those inputs.  */
static void
adjust_concat (rtx_insn *insn)
{
  rtx set = PATTERN (insn);
  rtx concat = XEXP (set, 1);
  rtx src0 = XEXP (concat, 0);
  XEXP (concat, 0) = XEXP (concat, 1);
  XEXP (concat, 1) = src0;
  INSN_CODE (insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (insn);

  if (dump_file)
    fprintf (dump_file, "Reversing inputs for concat %d\n", INSN_UID (insn));
}

/* Given an UNSPEC_VPERM insn, modify the mask loaded from the
   constant pool to reflect swapped doublewords.  */
static void
adjust_vperm (rtx_insn *insn)
{
  /* We previously determined that the UNSPEC_VPERM was fed by a
     swap of a swapping load of a TOC-relative constant pool symbol.
     Find the MEM in the swapping load and replace it with a MEM for
     the adjusted mask constant.  */
  rtx set = PATTERN (insn);
  rtx mask_reg = XVECEXP (SET_SRC (set), 0, 2);

  /* Find the swap.  */
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref use;
  rtx_insn *swap_insn = 0;
  FOR_EACH_INSN_INFO_USE (use, insn_info)
    if (rtx_equal_p (DF_REF_REG (use), mask_reg))
      {
	struct df_link *def_link = DF_REF_CHAIN (use);
	gcc_assert (def_link && !def_link->next);
	swap_insn = DF_REF_INSN (def_link->ref);
	break;
      }
  gcc_assert (swap_insn);
  
  /* Find the load.  */
  insn_info = DF_INSN_INFO_GET (swap_insn);
  rtx_insn *load_insn = 0;
  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      gcc_assert (def_link && !def_link->next);
      load_insn = DF_REF_INSN (def_link->ref);
      break;
    }
  gcc_assert (load_insn);

  /* Find the TOC-relative symbol access.  */
  insn_info = DF_INSN_INFO_GET (load_insn);
  rtx_insn *tocrel_insn = 0;
  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);
      gcc_assert (def_link && !def_link->next);
      tocrel_insn = DF_REF_INSN (def_link->ref);
      break;
    }
  gcc_assert (tocrel_insn);

  /* Find the embedded CONST_VECTOR.  We have to call toc_relative_expr_p
     to set tocrel_base; otherwise it would be unnecessary as we've
     already established it will return true.  */
  rtx base, offset;
  const_rtx tocrel_base;
  rtx tocrel_expr = SET_SRC (PATTERN (tocrel_insn));
  /* There is an extra level of indirection for small/large code models.  */
  if (GET_CODE (tocrel_expr) == MEM)
    tocrel_expr = XEXP (tocrel_expr, 0);
  if (!toc_relative_expr_p (tocrel_expr, false, &tocrel_base, NULL))
    gcc_unreachable ();
  split_const (XVECEXP (tocrel_base, 0, 0), &base, &offset);
  rtx const_vector = get_pool_constant (base);
  /* With the extra indirection, get_pool_constant will produce the
     real constant from the reg_equal expression, so get the real
     constant.  */
  if (GET_CODE (const_vector) == SYMBOL_REF)
    const_vector = get_pool_constant (const_vector);
  gcc_assert (GET_CODE (const_vector) == CONST_VECTOR);

  /* Create an adjusted mask from the initial mask.  */
  unsigned int new_mask[16], i, val;
  for (i = 0; i < 16; ++i) {
    val = INTVAL (XVECEXP (const_vector, 0, i));
    if (val < 16)
      new_mask[i] = (val + 8) % 16;
    else
      new_mask[i] = ((val + 8) % 16) + 16;
  }

  /* Create a new CONST_VECTOR and a MEM that references it.  */
  rtx vals = gen_rtx_PARALLEL (V16QImode, rtvec_alloc (16));
  for (i = 0; i < 16; ++i)
    XVECEXP (vals, 0, i) = GEN_INT (new_mask[i]);
  rtx new_const_vector = gen_rtx_CONST_VECTOR (V16QImode, XVEC (vals, 0));
  rtx new_mem = force_const_mem (V16QImode, new_const_vector);
  /* This gives us a MEM whose base operand is a SYMBOL_REF, which we
     can't recognize.  Force the SYMBOL_REF into a register.  */
  if (!REG_P (XEXP (new_mem, 0))) {
    rtx base_reg = force_reg (Pmode, XEXP (new_mem, 0));
    XEXP (new_mem, 0) = base_reg;
    /* Move the newly created insn ahead of the load insn.  */
    rtx_insn *force_insn = get_last_insn ();
    remove_insn (force_insn);
    rtx_insn *before_load_insn = PREV_INSN (load_insn);
    add_insn_after (force_insn, before_load_insn, BLOCK_FOR_INSN (load_insn));
    df_insn_rescan (before_load_insn);
    df_insn_rescan (force_insn);
  }

  /* Replace the MEM in the load instruction and rescan it.  */
  XEXP (SET_SRC (PATTERN (load_insn)), 0) = new_mem;
  INSN_CODE (load_insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (load_insn);

  if (dump_file)
    fprintf (dump_file, "Adjusting mask for vperm %d\n", INSN_UID (insn));
}

/* The insn described by INSN_ENTRY[I] can be swapped, but only
   with special handling.  Take care of that here.  */
static void
handle_special_swappables (swap_web_entry *insn_entry, unsigned i)
{
  rtx_insn *insn = insn_entry[i].insn;
  rtx body = PATTERN (insn);

  switch (insn_entry[i].special_handling)
    {
    default:
      gcc_unreachable ();
    case SH_CONST_VECTOR:
      {
	/* A CONST_VECTOR will only show up somewhere in the RHS of a SET.  */
	gcc_assert (GET_CODE (body) == SET);
	swap_const_vector_halves (&SET_SRC (body));
	if (dump_file)
	  fprintf (dump_file, "Swapping constant halves in insn %d\n", i);
	break;
      }
    case SH_SUBREG:
      /* A subreg of the same size is already safe.  For subregs that
	 select a smaller portion of a reg, adjust the index for
	 swapped doublewords.  */
      adjust_subreg_index (body);
      if (dump_file)
	fprintf (dump_file, "Adjusting subreg in insn %d\n", i);
      break;
    case SH_NOSWAP_LD:
      /* Convert a non-permuting load to a permuting one.  */
      permute_load (insn);
      break;
    case SH_NOSWAP_ST:
      /* Convert a non-permuting store to a permuting one.  */
      permute_store (insn);
      break;
    case SH_EXTRACT:
      /* Change the lane on an extract operation.  */
      adjust_extract (insn);
      break;
    case SH_SPLAT:
      /* Change the lane on a direct-splat operation.  */
      adjust_splat (insn);
      break;
    case SH_XXPERMDI:
      /* Change the lanes on an XXPERMDI operation.  */
      adjust_xxpermdi (insn);
      break;
    case SH_CONCAT:
      /* Reverse the order of a concatenation operation.  */
      adjust_concat (insn);
      break;
    case SH_VPERM:
      /* Change the mask loaded from the constant pool for a VPERM.  */
      adjust_vperm (insn);
      break;
    }
}

/* Find the insn from the Ith table entry, which is known to be a
   register swap Y = SWAP(X).  Replace it with a copy Y = X.  */
static void
replace_swap_with_copy (swap_web_entry *insn_entry, unsigned i)
{
  rtx_insn *insn = insn_entry[i].insn;
  rtx body = PATTERN (insn);
  rtx src_reg = XEXP (SET_SRC (body), 0);
  rtx copy = gen_rtx_SET (SET_DEST (body), src_reg);
  rtx_insn *new_insn = emit_insn_before (copy, insn);
  set_block_for_insn (new_insn, BLOCK_FOR_INSN (insn));
  df_insn_rescan (new_insn);

  if (dump_file)
    {
      unsigned int new_uid = INSN_UID (new_insn);
      fprintf (dump_file, "Replacing swap %d with copy %d\n", i, new_uid);
    }

  df_insn_delete (insn);
  remove_insn (insn);
  insn->set_deleted ();
}

/* Make NEW_MEM_EXP's attributes and flags resemble those of
   ORIGINAL_MEM_EXP.  */
static void
mimic_memory_attributes_and_flags (rtx new_mem_exp, const_rtx original_mem_exp)
{
  RTX_FLAG (new_mem_exp, jump) = RTX_FLAG (original_mem_exp, jump);
  RTX_FLAG (new_mem_exp, call) = RTX_FLAG (original_mem_exp, call);
  RTX_FLAG (new_mem_exp, unchanging) = RTX_FLAG (original_mem_exp, unchanging);
  RTX_FLAG (new_mem_exp, volatil) = RTX_FLAG (original_mem_exp, volatil);
  RTX_FLAG (new_mem_exp, frame_related) =
    RTX_FLAG (original_mem_exp, frame_related);

  /* The following fields may not be used with MEM subexpressions */
  RTX_FLAG (new_mem_exp, in_struct) = RTX_FLAG (original_mem_exp, in_struct);
  RTX_FLAG (new_mem_exp, return_val) = RTX_FLAG (original_mem_exp, return_val);

  struct mem_attrs original_attrs = *get_mem_attrs(original_mem_exp);

  alias_set_type set = original_attrs.alias;
  set_mem_alias_set (new_mem_exp, set);

  addr_space_t addrspace = original_attrs.addrspace;
  set_mem_addr_space (new_mem_exp, addrspace);

  unsigned int align = original_attrs.align;
  set_mem_align (new_mem_exp, align);

  tree expr = original_attrs.expr;
  set_mem_expr (new_mem_exp, expr);

  if (original_attrs.offset_known_p)
    {
      HOST_WIDE_INT offset = original_attrs.offset;
      set_mem_offset (new_mem_exp, offset);
    }
  else
    clear_mem_offset (new_mem_exp);

  if (original_attrs.size_known_p)
    {
      HOST_WIDE_INT size = original_attrs.size;
      set_mem_size (new_mem_exp, size);
    }
  else
    clear_mem_size (new_mem_exp);
}

/* Generate an rtx expression to represent use of the stvx insn to store
   the value represented by register SRC_EXP into the memory at address
   DEST_EXP, with vector mode MODE.  */
rtx
rs6000_gen_stvx (enum machine_mode mode, rtx dest_exp, rtx src_exp)
{
  rtx stvx;

  if (mode == V16QImode)
    stvx = gen_altivec_stvx_v16qi (src_exp, dest_exp);
  else if (mode == V8HImode)
    stvx = gen_altivec_stvx_v8hi (src_exp, dest_exp);
#ifdef HAVE_V8HFmode
  else if (mode == V8HFmode)
    stvx = gen_altivec_stvx_v8hf (src_exp, dest_exp);
#endif
  else if (mode == V4SImode)
    stvx = gen_altivec_stvx_v4si (src_exp, dest_exp);
  else if (mode == V4SFmode)
    stvx = gen_altivec_stvx_v4sf (src_exp, dest_exp);
  else if (mode == V2DImode)
    stvx = gen_altivec_stvx_v2di (src_exp, dest_exp);
  else if (mode == V2DFmode)
    stvx = gen_altivec_stvx_v2df (src_exp, dest_exp);
  else if (mode == V1TImode)
    stvx = gen_altivec_stvx_v1ti (src_exp, dest_exp);
  else
    /* KFmode, TFmode, other modes not expected in this context.  */
    gcc_unreachable ();

  rtx new_mem_exp = SET_DEST (PATTERN (stvx));
  mimic_memory_attributes_and_flags (new_mem_exp, dest_exp);
  return stvx;
}

/* Given that STORE_INSN represents an aligned store-with-swap of a
   swapped value, replace the store with an aligned store (without
   swap) and replace the swap with a copy insn.  */
static void
replace_swapped_aligned_store (swap_web_entry *insn_entry,
			       rtx_insn *store_insn)
{
  unsigned uid = INSN_UID (store_insn);
  gcc_assert (insn_entry[uid].is_swap && insn_entry[uid].is_store);

  rtx body = PATTERN (store_insn);
  rtx dest_address = XEXP (SET_DEST (body), 0);
  rtx swap_reg = XEXP (SET_SRC (body), 0);
  gcc_assert (REG_P (dest_address)
	      || rs6000_sum_of_two_registers_p (dest_address));

  /* Find the swap instruction that provides the value to be stored by
   * this store-with-swap instruction. */
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (store_insn);
  df_ref use;
  rtx_insn *swap_insn = NULL;
  unsigned uid2 = 0;
  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      struct df_link *def_link = DF_REF_CHAIN (use);

      /* if this is not the definition of the candidate swap register,
	 then skip it.  I am only interested in the swap insnd.  */
      if (!rtx_equal_p (DF_REF_REG (use), swap_reg))
	continue;

      /* If there is no def or the def is artifical or there are
	 multiple defs, we should not be here.  */
      gcc_assert (def_link && def_link->ref && !def_link->next
		  && !DF_REF_IS_ARTIFICIAL (def_link->ref));

      swap_insn = DF_REF_INSN (def_link->ref);
      uid2 = INSN_UID (swap_insn);

      /* If this source value is not a simple swap, we should not be here.  */
      gcc_assert (insn_entry[uid2].is_swap && !insn_entry[uid2].is_load
		  && !insn_entry[uid2].is_store);

      /* We've processed the use we care about, so break out of
	 this loop.  */
      break;
    }

  /* At this point, swap_insn and uid2 represent the swap instruction
     that feeds the store.  */
  gcc_assert (swap_insn);
  rtx set = single_set (store_insn);
  gcc_assert (set);
  rtx dest_exp = SET_DEST (set);
  rtx src_exp = XEXP (SET_SRC (body), 0);
  enum machine_mode mode = GET_MODE (dest_exp);
  gcc_assert (MEM_P (dest_exp));
  gcc_assert (MEM_ALIGN (dest_exp) >= 128);

  /* Replace the copy with a new insn.  */
  rtx stvx;
  stvx = rs6000_gen_stvx (mode, dest_exp, src_exp);

  rtx_insn *new_insn = emit_insn_before (stvx, store_insn);
  rtx new_body = PATTERN (new_insn);

  gcc_assert ((GET_CODE (new_body) == SET)
	      && (GET_CODE (SET_DEST (new_body)) == MEM));

  set_block_for_insn (new_insn, BLOCK_FOR_INSN (store_insn));
  df_insn_rescan (new_insn);

  df_insn_delete (store_insn);
  remove_insn (store_insn);
  store_insn->set_deleted ();

  /* Replace the swap with a copy.  */
  uid2 = INSN_UID (swap_insn);
  mark_swaps_for_removal (insn_entry, uid2);
  replace_swap_with_copy (insn_entry, uid2);
}

/* Generate an rtx expression to represent use of the lvx insn to load
   from memory SRC_EXP into register DEST_EXP with vector mode MODE. */
rtx
rs6000_gen_lvx (enum machine_mode mode, rtx dest_exp, rtx src_exp)
{
  rtx lvx;

  if (mode == V16QImode)
    lvx = gen_altivec_lvx_v16qi (dest_exp, src_exp);
  else if (mode == V8HImode)
    lvx = gen_altivec_lvx_v8hi (dest_exp, src_exp);
#ifdef HAVE_V8HFmode
  else if (mode == V8HFmode)
    lvx = gen_altivec_lvx_v8hf (dest_exp, src_exp);
#endif
  else if (mode == V4SImode)
    lvx = gen_altivec_lvx_v4si (dest_exp, src_exp);
  else if (mode == V4SFmode)
    lvx = gen_altivec_lvx_v4sf (dest_exp, src_exp);
  else if (mode == V2DImode)
    lvx = gen_altivec_lvx_v2di (dest_exp, src_exp);
  else if (mode == V2DFmode)
    lvx = gen_altivec_lvx_v2df (dest_exp, src_exp);
  else if (mode == V1TImode)
    lvx = gen_altivec_lvx_v1ti (dest_exp, src_exp);
  else
    /* KFmode, TFmode, other modes not expected in this context.  */
    gcc_unreachable ();

  rtx new_mem_exp = SET_SRC (PATTERN (lvx));
  mimic_memory_attributes_and_flags (new_mem_exp, src_exp);

  return lvx;
}

/* Given that SWAP_INSN represents a swap of an aligned
   load-with-swap, replace the load with an aligned load (without
   swap) and replace the swap with a copy insn.  */
static void
replace_swapped_aligned_load (swap_web_entry *insn_entry, rtx swap_insn)
{
  /* Find the load.  */
  unsigned uid = INSN_UID (swap_insn);
  /* Only call this if quad_aligned_load_p (swap_insn).  */
  gcc_assert (insn_entry[uid].is_swap && !insn_entry[uid].is_load);
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (swap_insn);

  /* Since insn is known to represent a swap instruction, we know it
     "uses" only one input variable.  */
  df_ref use = DF_INSN_INFO_USES (insn_info);

  /* Figure out where this input variable is defined.  */
  struct df_link *def_link = DF_REF_CHAIN (use);
  gcc_assert (def_link && !def_link->next);
  gcc_assert (def_link && def_link->ref &&
	      !DF_REF_IS_ARTIFICIAL (def_link->ref) && !def_link->next);

  rtx_insn *def_insn = DF_REF_INSN (def_link->ref);
  unsigned uid2 = INSN_UID (def_insn);

  /* We're expecting a load-with-swap insn.  */
  gcc_assert (insn_entry[uid2].is_load && insn_entry[uid2].is_swap);

  /* We expect this to be a set to memory, with source representing a
     swap (indicated by code VEC_SELECT).  */
  rtx body = PATTERN (def_insn);
  gcc_assert ((GET_CODE (body) == SET)
	      && (GET_CODE (SET_SRC (body)) == VEC_SELECT)
	      && (GET_CODE (XEXP (SET_SRC (body), 0)) == MEM));

  rtx src_exp = XEXP (SET_SRC (body), 0);
  enum machine_mode mode = GET_MODE (src_exp);
  rtx lvx = rs6000_gen_lvx (mode, SET_DEST (body), src_exp);

  rtx_insn *new_insn = emit_insn_before (lvx, def_insn);
  rtx new_body = PATTERN (new_insn);

  gcc_assert ((GET_CODE (new_body) == SET)
	      && (GET_CODE (SET_SRC (new_body)) == MEM));

  set_block_for_insn (new_insn, BLOCK_FOR_INSN (def_insn));
  df_insn_rescan (new_insn);

  df_insn_delete (def_insn);
  remove_insn (def_insn);
  def_insn->set_deleted ();

  /* Replace the swap with a copy.  */
  mark_swaps_for_removal (insn_entry, uid);
  replace_swap_with_copy (insn_entry, uid);
}

/* Given that SWAP_INSN represents a swap of a load of a constant
   vector value, replace with a single instruction that loads a
   swapped variant of the original constant.

   The "natural" representation of a byte array in memory is the same
   for big endian and little endian.

   unsigned char byte_array[] =
     { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f };

   However, when loaded into a vector register, the representation
   depends on endian conventions.

   In big-endian mode, the register holds:

     MSB                                            LSB
     [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f ]

   In little-endian mode, the register holds:

     MSB                                            LSB
     [ f, e, d, c, b, a, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ]

   Word arrays require different handling.  Consider the word array:

   unsigned int word_array[] =
     { 0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f };

   The in-memory representation depends on endian configuration.  The
   equivalent array, declared as a byte array, in memory would be:

   unsigned char big_endian_word_array_data[] =
     { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f }

   unsigned char little_endian_word_array_data[] =
     { 3, 2, 1, 0, 7, 6, 5, 4, b, a, 9, 8, f, e, d, c }

   In big-endian mode, the register holds:

     MSB                                            LSB
     [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f ]

   In little-endian mode, the register holds:

     MSB                                            LSB
     [ c, d, e, f, 8, 9, a, b, 4, 5, 6, 7, 0, 1, 2, 3 ]


  Similar transformations apply to the vector of half-word and vector
  of double-word representations.

  For now, don't handle vectors of quad-precision values.  Just return.
  A better solution is to fix the code generator to emit lvx/stvx for
  those.  */
static void
replace_swapped_load_constant (swap_web_entry *insn_entry, rtx swap_insn)
{
  /* Find the load.  */
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (swap_insn);
  rtx_insn *load_insn;
  df_ref use  = DF_INSN_INFO_USES (insn_info);
  struct df_link *def_link = DF_REF_CHAIN (use);
  gcc_assert (def_link && !def_link->next);

  load_insn = DF_REF_INSN (def_link->ref);
  gcc_assert (load_insn);

  /* Find the TOC-relative symbol access.  */
  insn_info = DF_INSN_INFO_GET (load_insn);
  use = DF_INSN_INFO_USES (insn_info);

  def_link = DF_REF_CHAIN (use);
  gcc_assert (def_link && !def_link->next);

  rtx_insn *tocrel_insn = DF_REF_INSN (def_link->ref);
  gcc_assert (tocrel_insn);

  /* Find the embedded CONST_VECTOR.  We have to call toc_relative_expr_p
     to set tocrel_base; otherwise it would be unnecessary as we've
     already established it will return true.  */
  rtx base, offset;
  rtx tocrel_expr = SET_SRC (PATTERN (tocrel_insn));
  const_rtx tocrel_base;

  /* There is an extra level of indirection for small/large code models.  */
  if (GET_CODE (tocrel_expr) == MEM)
    tocrel_expr = XEXP (tocrel_expr, 0);

  if (!toc_relative_expr_p (tocrel_expr, false, &tocrel_base, NULL))
    gcc_unreachable ();

  split_const (XVECEXP (tocrel_base, 0, 0), &base, &offset);
  rtx const_vector = get_pool_constant (base);

  /* With the extra indirection, get_pool_constant will produce the
     real constant from the reg_equal expression, so get the real
     constant.  */
  if (GET_CODE (const_vector) == SYMBOL_REF)
    const_vector = get_pool_constant (const_vector);
  gcc_assert (GET_CODE (const_vector) == CONST_VECTOR);

  rtx new_mem;
  enum machine_mode mode = GET_MODE (const_vector);

  /* Create an adjusted constant from the original constant.  */
  if (mode == V1TImode)
    /* Leave this code as is.  */
    return;
  else if (mode == V16QImode)
    {
      rtx vals = gen_rtx_PARALLEL (mode, rtvec_alloc (16));
      int i;

      for (i = 0; i < 16; i++)
	XVECEXP (vals, 0, ((i+8) % 16)) = XVECEXP (const_vector, 0, i);
      rtx new_const_vector = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
      new_mem = force_const_mem (mode, new_const_vector);
    }
  else if ((mode == V8HImode)
#ifdef HAVE_V8HFmode
	   || (mode == V8HFmode)
#endif
	   )
    {
      rtx vals = gen_rtx_PARALLEL (mode, rtvec_alloc (8));
      int i;

      for (i = 0; i < 8; i++)
	XVECEXP (vals, 0, ((i+4) % 8)) = XVECEXP (const_vector, 0, i);
      rtx new_const_vector = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
      new_mem = force_const_mem (mode, new_const_vector);
    }
  else if ((mode == V4SImode) || (mode == V4SFmode))
    {
      rtx vals = gen_rtx_PARALLEL (mode, rtvec_alloc (4));
      int i;

      for (i = 0; i < 4; i++)
	XVECEXP (vals, 0, ((i+2) % 4)) = XVECEXP (const_vector, 0, i);
      rtx new_const_vector = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
      new_mem = force_const_mem (mode, new_const_vector);
    }
  else if ((mode == V2DImode) || (mode == V2DFmode))
    {
      rtx vals = gen_rtx_PARALLEL (mode, rtvec_alloc (2));
      int i;

      for (i = 0; i < 2; i++)
	XVECEXP (vals, 0, ((i+1) % 2)) = XVECEXP (const_vector, 0, i);
      rtx new_const_vector = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
      new_mem = force_const_mem (mode, new_const_vector);
    }
  else
    {
      /* We do not expect other modes to be constant-load-swapped.  */
      gcc_unreachable ();
    }

  /* This gives us a MEM whose base operand is a SYMBOL_REF, which we
     can't recognize.  Force the SYMBOL_REF into a register.  */
  if (!REG_P (XEXP (new_mem, 0))) {
    rtx base_reg = force_reg (Pmode, XEXP (new_mem, 0));
    XEXP (new_mem, 0) = base_reg;

    /* Move the newly created insn ahead of the load insn.  */
    /* The last insn is the the insn that forced new_mem into a register.  */
    rtx_insn *force_insn = get_last_insn ();
    /* Remove this insn from the end of the instruction sequence.  */
    remove_insn (force_insn);
    rtx_insn *before_load_insn = PREV_INSN (load_insn);

    /* And insert this insn back into the sequence before the previous
       load insn so this new expression will be available when the
       existing load is modified to load the swapped constant.  */
    add_insn_after (force_insn, before_load_insn, BLOCK_FOR_INSN (load_insn));
    df_insn_rescan (before_load_insn);
    df_insn_rescan (force_insn);
  }

  /* Replace the MEM in the load instruction and rescan it.  */
  XEXP (SET_SRC (PATTERN (load_insn)), 0) = new_mem;
  INSN_CODE (load_insn) = -1; /* Force re-recognition.  */
  df_insn_rescan (load_insn);

  unsigned int uid = INSN_UID (swap_insn);
  mark_swaps_for_removal (insn_entry, uid);
  replace_swap_with_copy (insn_entry, uid);
}

/* Dump the swap table to DUMP_FILE.  */
static void
dump_swap_insn_table (swap_web_entry *insn_entry)
{
  int e = get_max_uid ();
  fprintf (dump_file, "\nRelevant insns with their flag settings\n\n");

  for (int i = 0; i < e; ++i)
    if (insn_entry[i].is_relevant)
      {
	swap_web_entry *pred_entry = (swap_web_entry *)insn_entry[i].pred ();
	fprintf (dump_file, "%6d %6d  ", i,
		 pred_entry && pred_entry->insn
		 ? INSN_UID (pred_entry->insn) : 0);
	if (insn_entry[i].is_load)
	  fputs ("load ", dump_file);
	if (insn_entry[i].is_store)
	  fputs ("store ", dump_file);
	if (insn_entry[i].is_swap)
	  fputs ("swap ", dump_file);
	if (insn_entry[i].is_live_in)
	  fputs ("live-in ", dump_file);
	if (insn_entry[i].is_live_out)
	  fputs ("live-out ", dump_file);
	if (insn_entry[i].contains_subreg)
	  fputs ("subreg ", dump_file);
	if (insn_entry[i].is_128_int)
	  fputs ("int128 ", dump_file);
	if (insn_entry[i].is_call)
	  fputs ("call ", dump_file);
	if (insn_entry[i].is_swappable)
	  {
	    fputs ("swappable ", dump_file);
	    if (insn_entry[i].special_handling == SH_CONST_VECTOR)
	      fputs ("special:constvec ", dump_file);
	    else if (insn_entry[i].special_handling == SH_SUBREG)
	      fputs ("special:subreg ", dump_file);
	    else if (insn_entry[i].special_handling == SH_NOSWAP_LD)
	      fputs ("special:load ", dump_file);
	    else if (insn_entry[i].special_handling == SH_NOSWAP_ST)
	      fputs ("special:store ", dump_file);
	    else if (insn_entry[i].special_handling == SH_EXTRACT)
	      fputs ("special:extract ", dump_file);
	    else if (insn_entry[i].special_handling == SH_SPLAT)
	      fputs ("special:splat ", dump_file);
	    else if (insn_entry[i].special_handling == SH_XXPERMDI)
	      fputs ("special:xxpermdi ", dump_file);
	    else if (insn_entry[i].special_handling == SH_CONCAT)
	      fputs ("special:concat ", dump_file);
	    else if (insn_entry[i].special_handling == SH_VPERM)
	      fputs ("special:vperm ", dump_file);
	  }
	if (insn_entry[i].web_not_optimizable)
	  fputs ("unoptimizable ", dump_file);
	if (insn_entry[i].will_delete)
	  fputs ("delete ", dump_file);
	fputs ("\n", dump_file);
      }
  fputs ("\n", dump_file);
}

/* Return RTX with its address canonicalized to (reg) or (+ reg reg).
   Here RTX is an (& addr (const_int -16)).  Always return a new copy
   to avoid problems with combine.  */
static rtx
alignment_with_canonical_addr (rtx align)
{
  rtx canon;
  rtx addr = XEXP (align, 0);

  if (REG_P (addr))
    canon = addr;

  else if (GET_CODE (addr) == PLUS)
    {
      rtx addrop0 = XEXP (addr, 0);
      rtx addrop1 = XEXP (addr, 1);

      if (!REG_P (addrop0))
	addrop0 = force_reg (GET_MODE (addrop0), addrop0);

      if (!REG_P (addrop1))
	addrop1 = force_reg (GET_MODE (addrop1), addrop1);

      canon = gen_rtx_PLUS (GET_MODE (addr), addrop0, addrop1);
    }

  else
    canon = force_reg (GET_MODE (addr), addr);

  return gen_rtx_AND (GET_MODE (align), canon, GEN_INT (-16));
}

/* Check whether an rtx is an alignment mask, and if so, return 
   a fully-expanded rtx for the masking operation.  */
static rtx
alignment_mask (rtx_insn *insn)
{
  rtx body = PATTERN (insn);

  if (GET_CODE (body) != SET
      || GET_CODE (SET_SRC (body)) != AND
      || !REG_P (XEXP (SET_SRC (body), 0)))
    return 0;

  rtx mask = XEXP (SET_SRC (body), 1);

  if (GET_CODE (mask) == CONST_INT)
    {
      if (INTVAL (mask) == -16)
	return alignment_with_canonical_addr (SET_SRC (body));
      else
	return 0;
    }

  if (!REG_P (mask))
    return 0;

  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  df_ref use;
  rtx real_mask = 0;

  FOR_EACH_INSN_INFO_USE (use, insn_info)
    {
      if (!rtx_equal_p (DF_REF_REG (use), mask))
	continue;

      struct df_link *def_link = DF_REF_CHAIN (use);
      if (!def_link || def_link->next)
	return 0;

      rtx_insn *const_insn = DF_REF_INSN (def_link->ref);
      rtx const_body = PATTERN (const_insn);
      if (GET_CODE (const_body) != SET)
	return 0;

      real_mask = SET_SRC (const_body);

      if (GET_CODE (real_mask) != CONST_INT
	  || INTVAL (real_mask) != -16)
	return 0;
    }

  if (real_mask == 0)
    return 0;

  return alignment_with_canonical_addr (SET_SRC (body));
}

/* Given INSN that's a load or store based at BASE_REG, look for a
   feeding computation that aligns its address on a 16-byte boundary.
   Return the rtx and its containing AND_INSN.  */
static rtx
find_alignment_op (rtx_insn *insn, rtx base_reg, rtx_insn **and_insn)
{
  df_ref base_use;
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
  rtx and_operation = 0;

  FOR_EACH_INSN_INFO_USE (base_use, insn_info)
    {
      if (!rtx_equal_p (DF_REF_REG (base_use), base_reg))
	continue;

      struct df_link *base_def_link = DF_REF_CHAIN (base_use);
      if (!base_def_link || base_def_link->next)
	break;

      /* With stack-protector code enabled, and possibly in other
	 circumstances, there may not be an associated insn for 
	 the def.  */
      if (DF_REF_IS_ARTIFICIAL (base_def_link->ref))
	break;

      *and_insn = DF_REF_INSN (base_def_link->ref);
      and_operation = alignment_mask (*and_insn);
      if (and_operation != 0)
	break;
    }

  return and_operation;
}

struct del_info { bool replace; rtx_insn *replace_insn; };

/* If INSN is the load for an lvx pattern, put it in canonical form.  */
static void
recombine_lvx_pattern (rtx_insn *insn, del_info *to_delete)
{
  rtx body = PATTERN (insn);
  gcc_assert (GET_CODE (body) == SET
	      && GET_CODE (SET_SRC (body)) == VEC_SELECT
	      && GET_CODE (XEXP (SET_SRC (body), 0)) == MEM);

  rtx mem = XEXP (SET_SRC (body), 0);
  rtx base_reg = XEXP (mem, 0);

  rtx_insn *and_insn;
  rtx and_operation = find_alignment_op (insn, base_reg, &and_insn);

  if (and_operation != 0)
    {
      df_ref def;
      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
      FOR_EACH_INSN_INFO_DEF (def, insn_info)
	{
	  struct df_link *link = DF_REF_CHAIN (def);
	  if (!link || link->next)
	    break;

	  rtx_insn *swap_insn = DF_REF_INSN (link->ref);
	  if (!insn_is_swap_p (swap_insn)
	      || insn_is_load_p (swap_insn)
	      || insn_is_store_p (swap_insn))
	    break;

	  /* Expected lvx pattern found.  Change the swap to
	     a copy, and propagate the AND operation into the
	     load.  */
	  to_delete[INSN_UID (swap_insn)].replace = true;
	  to_delete[INSN_UID (swap_insn)].replace_insn = swap_insn;

	  /* However, first we must be sure that we make the
	     base register from the AND operation available
	     in case the register has been overwritten.  Copy
	     the base register to a new pseudo and use that
	     as the base register of the AND operation in
	     the new LVX instruction.  */
	  rtx and_base = XEXP (and_operation, 0);
	  rtx new_reg = gen_reg_rtx (GET_MODE (and_base));
	  rtx copy = gen_rtx_SET (new_reg, and_base);
	  rtx_insn *new_insn = emit_insn_after (copy, and_insn);
	  set_block_for_insn (new_insn, BLOCK_FOR_INSN (and_insn));
	  df_insn_rescan (new_insn);

	  XEXP (mem, 0) = gen_rtx_AND (GET_MODE (and_base), new_reg,
				       XEXP (and_operation, 1));
	  SET_SRC (body) = mem;
	  INSN_CODE (insn) = -1; /* Force re-recognition.  */
	  df_insn_rescan (insn);
		  
	  if (dump_file)
	    fprintf (dump_file, "lvx opportunity found at %d\n",
		     INSN_UID (insn));
	}
    }
}

/* If INSN is the store for an stvx pattern, put it in canonical form.  */
static void
recombine_stvx_pattern (rtx_insn *insn, del_info *to_delete)
{
  rtx body = PATTERN (insn);
  gcc_assert (GET_CODE (body) == SET
	      && GET_CODE (SET_DEST (body)) == MEM
	      && GET_CODE (SET_SRC (body)) == VEC_SELECT);
  rtx mem = SET_DEST (body);
  rtx base_reg = XEXP (mem, 0);

  rtx_insn *and_insn;
  rtx and_operation = find_alignment_op (insn, base_reg, &and_insn);

  if (and_operation != 0)
    {
      rtx src_reg = XEXP (SET_SRC (body), 0);
      df_ref src_use;
      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
      FOR_EACH_INSN_INFO_USE (src_use, insn_info)
	{
	  if (!rtx_equal_p (DF_REF_REG (src_use), src_reg))
	    continue;

	  struct df_link *link = DF_REF_CHAIN (src_use);
	  if (!link || link->next)
	    break;

	  rtx_insn *swap_insn = DF_REF_INSN (link->ref);
	  if (!insn_is_swap_p (swap_insn)
	      || insn_is_load_p (swap_insn)
	      || insn_is_store_p (swap_insn))
	    break;

	  /* Expected stvx pattern found.  Change the swap to
	     a copy, and propagate the AND operation into the
	     store.  */
	  to_delete[INSN_UID (swap_insn)].replace = true;
	  to_delete[INSN_UID (swap_insn)].replace_insn = swap_insn;

	  /* However, first we must be sure that we make the
	     base register from the AND operation available
	     in case the register has been overwritten.  Copy
	     the base register to a new pseudo and use that
	     as the base register of the AND operation in
	     the new STVX instruction.  */
	  rtx and_base = XEXP (and_operation, 0);
	  rtx new_reg = gen_reg_rtx (GET_MODE (and_base));
	  rtx copy = gen_rtx_SET (new_reg, and_base);
	  rtx_insn *new_insn = emit_insn_after (copy, and_insn);
	  set_block_for_insn (new_insn, BLOCK_FOR_INSN (and_insn));
	  df_insn_rescan (new_insn);

	  XEXP (mem, 0) = gen_rtx_AND (GET_MODE (and_base), new_reg,
				       XEXP (and_operation, 1));
	  SET_SRC (body) = src_reg;
	  INSN_CODE (insn) = -1; /* Force re-recognition.  */
	  df_insn_rescan (insn);
		  
	  if (dump_file)
	    fprintf (dump_file, "stvx opportunity found at %d\n",
		     INSN_UID (insn));
	}
    }
}

/* Look for patterns created from builtin lvx and stvx calls, and
   canonicalize them to be properly recognized as such.  */
static void
recombine_lvx_stvx_patterns (function *fun)
{
  int i;
  basic_block bb;
  rtx_insn *insn;

  int num_insns = get_max_uid ();
  del_info *to_delete = XCNEWVEC (del_info, num_insns);

  FOR_ALL_BB_FN (bb, fun)
    FOR_BB_INSNS (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      if (insn_is_load_p (insn) && insn_is_swap_p (insn))
	recombine_lvx_pattern (insn, to_delete);
      else if (insn_is_store_p (insn) && insn_is_swap_p (insn))
	recombine_stvx_pattern (insn, to_delete);
    }

  /* Turning swaps into copies is delayed until now, to avoid problems
     with deleting instructions during the insn walk.  */
  for (i = 0; i < num_insns; i++)
    if (to_delete[i].replace)
      {
	rtx swap_body = PATTERN (to_delete[i].replace_insn);
	rtx src_reg = XEXP (SET_SRC (swap_body), 0);
	rtx copy = gen_rtx_SET (SET_DEST (swap_body), src_reg);
	rtx_insn *new_insn = emit_insn_before (copy,
					       to_delete[i].replace_insn);
	set_block_for_insn (new_insn,
			    BLOCK_FOR_INSN (to_delete[i].replace_insn));
	df_insn_rescan (new_insn);
	df_insn_delete (to_delete[i].replace_insn);
	remove_insn (to_delete[i].replace_insn);
	to_delete[i].replace_insn->set_deleted ();
      }
  
  free (to_delete);
}

/* Main entry point for this pass.  */
unsigned int
rs6000_analyze_swaps (function *fun)
{
  swap_web_entry *insn_entry;
  basic_block bb;
  rtx_insn *insn, *curr_insn = 0;

  /* Dataflow analysis for use-def chains.  */
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  /* Pre-pass to recombine lvx and stvx patterns so we don't lose info.  */
  recombine_lvx_stvx_patterns (fun);
  df_process_deferred_rescans ();

  /* Allocate structure to represent webs of insns.  */
  insn_entry = XCNEWVEC (swap_web_entry, get_max_uid ());

  /* Walk the insns to gather basic data.  */
  FOR_ALL_BB_FN (bb, fun)
    FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
    {
      unsigned int uid = INSN_UID (insn);
      if (NONDEBUG_INSN_P (insn))
	{
	  insn_entry[uid].insn = insn;

	  if (GET_CODE (insn) == CALL_INSN)
	    insn_entry[uid].is_call = 1;

	  /* Walk the uses and defs to see if we mention vector regs.
	     Record any constraints on optimization of such mentions.  */
	  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	  df_ref mention;
	  FOR_EACH_INSN_INFO_USE (mention, insn_info)
	    {
	      /* We use DF_REF_REAL_REG here to get inside any subregs.  */
	      machine_mode mode = GET_MODE (DF_REF_REAL_REG (mention));

	      /* If a use gets its value from a call insn, it will be
		 a hard register and will look like (reg:V4SI 3 3).
		 The df analysis creates two mentions for GPR3 and GPR4,
		 both DImode.  We must recognize this and treat it as a
		 vector mention to ensure the call is unioned with this
		 use.  */
	      if (mode == DImode && DF_REF_INSN_INFO (mention))
		{
		  rtx feeder = DF_REF_INSN (mention);
		  /* FIXME:  It is pretty hard to get from the df mention
		     to the mode of the use in the insn.  We arbitrarily
		     pick a vector mode here, even though the use might
		     be a real DImode.  We can be too conservative
		     (create a web larger than necessary) because of
		     this, so consider eventually fixing this.  */
		  if (GET_CODE (feeder) == CALL_INSN)
		    mode = V4SImode;
		}

	      if (ALTIVEC_OR_VSX_VECTOR_MODE (mode) || mode == TImode)
		{
		  insn_entry[uid].is_relevant = 1;
		  if (mode == TImode || mode == V1TImode
		      || FLOAT128_VECTOR_P (mode))
		    insn_entry[uid].is_128_int = 1;
		  if (DF_REF_INSN_INFO (mention))
		    insn_entry[uid].contains_subreg
		      = !rtx_equal_p (DF_REF_REG (mention),
				      DF_REF_REAL_REG (mention));
		  union_defs (insn_entry, insn, mention);
		}
	    }
	  FOR_EACH_INSN_INFO_DEF (mention, insn_info)
	    {
	      /* We use DF_REF_REAL_REG here to get inside any subregs.  */
	      machine_mode mode = GET_MODE (DF_REF_REAL_REG (mention));

	      /* If we're loading up a hard vector register for a call,
		 it looks like (set (reg:V4SI 9 9) (...)).  The df
		 analysis creates two mentions for GPR9 and GPR10, both
		 DImode.  So relying on the mode from the mentions
		 isn't sufficient to ensure we union the call into the
		 web with the parameter setup code.  */
	      if (mode == DImode && GET_CODE (insn) == SET
		  && ALTIVEC_OR_VSX_VECTOR_MODE (GET_MODE (SET_DEST (insn))))
		mode = GET_MODE (SET_DEST (insn));

	      if (ALTIVEC_OR_VSX_VECTOR_MODE (mode) || mode == TImode)
		{
		  insn_entry[uid].is_relevant = 1;
		  if (mode == TImode || mode == V1TImode
		      || FLOAT128_VECTOR_P (mode))
		    insn_entry[uid].is_128_int = 1;
		  if (DF_REF_INSN_INFO (mention))
		    insn_entry[uid].contains_subreg
		      = !rtx_equal_p (DF_REF_REG (mention),
				      DF_REF_REAL_REG (mention));
		  /* REG_FUNCTION_VALUE_P is not valid for subregs. */
		  else if (REG_FUNCTION_VALUE_P (DF_REF_REG (mention)))
		    insn_entry[uid].is_live_out = 1;
		  union_uses (insn_entry, insn, mention);
		}
	    }

	  if (insn_entry[uid].is_relevant)
	    {
	      /* Determine if this is a load or store.  */
	      insn_entry[uid].is_load = insn_is_load_p (insn);
	      insn_entry[uid].is_store = insn_is_store_p (insn);

	      /* Determine if this is a doubleword swap.  If not,
		 determine whether it can legally be swapped.  */
	      if (insn_is_swap_p (insn))
		insn_entry[uid].is_swap = 1;
	      else
		{
		  unsigned int special = SH_NONE;
		  insn_entry[uid].is_swappable
		    = insn_is_swappable_p (insn_entry, insn, &special);
		  if (special != SH_NONE && insn_entry[uid].contains_subreg)
		    insn_entry[uid].is_swappable = 0;
		  else if (special != SH_NONE)
		    insn_entry[uid].special_handling = special;
		  else if (insn_entry[uid].contains_subreg)
		    insn_entry[uid].special_handling = SH_SUBREG;
		}
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nSwap insn entry table when first built\n");
      dump_swap_insn_table (insn_entry);
    }

  /* Record unoptimizable webs.  */
  unsigned e = get_max_uid (), i;
  for (i = 0; i < e; ++i)
    {
      if (!insn_entry[i].is_relevant)
	continue;

      swap_web_entry *root
	= (swap_web_entry*)(&insn_entry[i])->unionfind_root ();

      if (insn_entry[i].is_live_in || insn_entry[i].is_live_out
	  || (insn_entry[i].contains_subreg
	      && insn_entry[i].special_handling != SH_SUBREG)
	  || insn_entry[i].is_128_int || insn_entry[i].is_call
	  || !(insn_entry[i].is_swappable || insn_entry[i].is_swap))
	root->web_not_optimizable = 1;

      /* If we have loads or stores that aren't permuting then the
	 optimization isn't appropriate.  */
      else if ((insn_entry[i].is_load || insn_entry[i].is_store)
	  && !insn_entry[i].is_swap && !insn_entry[i].is_swappable)
	root->web_not_optimizable = 1;

      /* If we have a swap that is both fed by a permuting load
	 and a feeder of a permuting store, then the optimization
	 isn't appropriate.  (Consider vec_xl followed by vec_xst_be.)  */
      else if (insn_entry[i].is_swap && !insn_entry[i].is_load
	       && !insn_entry[i].is_store
	       && swap_feeds_both_load_and_store (&insn_entry[i]))
	root->web_not_optimizable = 1;

      /* If we have permuting loads or stores that are not accompanied
	 by a register swap, the optimization isn't appropriate.  */
      else if (insn_entry[i].is_load && insn_entry[i].is_swap)
	{
	  rtx insn = insn_entry[i].insn;
	  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	  df_ref def;

	  FOR_EACH_INSN_INFO_DEF (def, insn_info)
	    {
	      struct df_link *link = DF_REF_CHAIN (def);

	      if (!chain_contains_only_swaps (insn_entry, link, FOR_LOADS))
		{
		  root->web_not_optimizable = 1;
		  break;
		}
	    }
	}
      else if (insn_entry[i].is_store && insn_entry[i].is_swap)
	{
	  rtx insn = insn_entry[i].insn;
	  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	  df_ref use;

	  FOR_EACH_INSN_INFO_USE (use, insn_info)
	    {
	      struct df_link *link = DF_REF_CHAIN (use);

	      if (!chain_contains_only_swaps (insn_entry, link, FOR_STORES))
		{
		  root->web_not_optimizable = 1;
		  break;
		}
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nSwap insn entry table after web analysis\n");
      dump_swap_insn_table (insn_entry);
    }

  /* For each load and store in an optimizable web (which implies
     the loads and stores are permuting), find the associated
     register swaps and mark them for removal.  Due to various
     optimizations we may mark the same swap more than once.  Also
     perform special handling for swappable insns that require it.  */
  for (i = 0; i < e; ++i)
    if ((insn_entry[i].is_load || insn_entry[i].is_store)
	&& insn_entry[i].is_swap)
      {
	swap_web_entry* root_entry
	  = (swap_web_entry*)((&insn_entry[i])->unionfind_root ());
	if (!root_entry->web_not_optimizable)
	  mark_swaps_for_removal (insn_entry, i);
      }
    else if (insn_entry[i].is_swappable && insn_entry[i].special_handling)
      {
	swap_web_entry* root_entry
	  = (swap_web_entry*)((&insn_entry[i])->unionfind_root ());
	if (!root_entry->web_not_optimizable)
	  handle_special_swappables (insn_entry, i);
      }

  /* Now delete the swaps marked for removal.  */
  for (i = 0; i < e; ++i)
    if (insn_entry[i].will_delete)
      replace_swap_with_copy (insn_entry, i);

  /* Clean up.  */
  free (insn_entry);

  /* Use a second pass over rtl to detect that certain vector values
     fetched from or stored to memory on quad-word aligned addresses
     can use lvx/stvx without swaps.  */

  /* First, rebuild ud chains.  */
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  swap_web_entry *pass2_insn_entry;
  pass2_insn_entry = XCNEWVEC (swap_web_entry, get_max_uid ());

  /* Walk the insns to gather basic data.  */
  FOR_ALL_BB_FN (bb, fun)
    FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
    {
      unsigned int uid = INSN_UID (insn);
      if (NONDEBUG_INSN_P (insn))
	{
	  pass2_insn_entry[uid].insn = insn;

	  pass2_insn_entry[uid].is_relevant = 1;
	  pass2_insn_entry[uid].is_load = insn_is_load_p (insn);
	  pass2_insn_entry[uid].is_store = insn_is_store_p (insn);

	  /* Determine if this is a doubleword swap.  If not,
	     determine whether it can legally be swapped.  */
	  if (insn_is_swap_p (insn))
	    pass2_insn_entry[uid].is_swap = 1;
	}
    }

  e = get_max_uid ();
  for (unsigned i = 0; i < e; ++i)
    if (pass2_insn_entry[i].is_swap && !pass2_insn_entry[i].is_load
	&& !pass2_insn_entry[i].is_store)
      {
	/* Replace swap of aligned load-swap with aligned unswapped
	   load.  */
	rtx_insn *rtx_insn = pass2_insn_entry[i].insn;
	if (quad_aligned_load_p (pass2_insn_entry, rtx_insn))
	  replace_swapped_aligned_load (pass2_insn_entry, rtx_insn);
      }
    else if (pass2_insn_entry[i].is_swap && pass2_insn_entry[i].is_store)
      {
	/* Replace aligned store-swap of swapped value with aligned
	   unswapped store.  */
	rtx_insn *rtx_insn = pass2_insn_entry[i].insn;
	if (quad_aligned_store_p (pass2_insn_entry, rtx_insn))
	  replace_swapped_aligned_store (pass2_insn_entry, rtx_insn);
      }

  /* Clean up.  */
  free (pass2_insn_entry);

  /* Use a third pass over rtl to replace swap(load(vector constant))
     with load(swapped vector constant).  */

  /* First, rebuild ud chains.  */
  df_remove_problem (df_chain);
  df_process_deferred_rescans ();
  df_set_flags (DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();

  swap_web_entry *pass3_insn_entry;
  pass3_insn_entry = XCNEWVEC (swap_web_entry, get_max_uid ());

  /* Walk the insns to gather basic data.  */
  FOR_ALL_BB_FN (bb, fun)
    FOR_BB_INSNS_SAFE (bb, insn, curr_insn)
    {
      unsigned int uid = INSN_UID (insn);
      if (NONDEBUG_INSN_P (insn))
	{
	  pass3_insn_entry[uid].insn = insn;

	  pass3_insn_entry[uid].is_relevant = 1;
	  pass3_insn_entry[uid].is_load = insn_is_load_p (insn);
	  pass3_insn_entry[uid].is_store = insn_is_store_p (insn);

	  /* Determine if this is a doubleword swap.  If not,
	     determine whether it can legally be swapped.  */
	  if (insn_is_swap_p (insn))
	    pass3_insn_entry[uid].is_swap = 1;
	}
    }

  e = get_max_uid ();
  for (unsigned i = 0; i < e; ++i)
    if (pass3_insn_entry[i].is_swap && !pass3_insn_entry[i].is_load
	&& !pass3_insn_entry[i].is_store)
      {
	insn = pass3_insn_entry[i].insn;
	if (const_load_sequence_p (pass3_insn_entry, insn))
	  replace_swapped_load_constant (pass3_insn_entry, insn);
      }

  /* Clean up.  */
  free (pass3_insn_entry);
  return 0;
}

const pass_data pass_data_analyze_swaps =
{
  RTL_PASS, /* type */
  "swaps", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_analyze_swaps : public rtl_opt_pass
{
public:
  pass_analyze_swaps(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_analyze_swaps, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (optimize > 0 && !BYTES_BIG_ENDIAN && TARGET_VSX
	      && !TARGET_P9_VECTOR && rs6000_optimize_swaps);
    }

  virtual unsigned int execute (function *fun)
    {
      return rs6000_analyze_swaps (fun);
    }

  opt_pass *clone ()
    {
      return new pass_analyze_swaps (m_ctxt);
    }

}; // class pass_analyze_swaps

rtl_opt_pass *
make_pass_analyze_swaps (gcc::context *ctxt)
{
  return new pass_analyze_swaps (ctxt);
}

