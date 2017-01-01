/* RTL manipulation functions exported by Pointer Bounds Checker.
   Copyright (C) 2014-2017 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "expr.h"
#include "rtl-chkp.h"
#include "tree-chkp.h"

static hash_map<tree, rtx> *chkp_rtx_bounds_map;

/* Get bounds rtx associated with NODE via
   chkp_set_rtl_bounds call.  */
rtx
chkp_get_rtl_bounds (tree node)
{
  rtx *slot;

  if (!chkp_rtx_bounds_map)
    return NULL_RTX;

  slot = chkp_rtx_bounds_map->get (node);
  return slot ? *slot : NULL_RTX;
}

/* Associate bounds rtx VAL with NODE.  */
void
chkp_set_rtl_bounds (tree node, rtx val)
{
  if (!chkp_rtx_bounds_map)
    chkp_rtx_bounds_map = new hash_map<tree, rtx>;

  chkp_rtx_bounds_map->put (node, val);
}

/* Reset all bounds stored via chkp_set_rtl_bounds.  */
void
chkp_reset_rtl_bounds ()
{
  if (!chkp_rtx_bounds_map)
    return;

  delete chkp_rtx_bounds_map;
  chkp_rtx_bounds_map = NULL;
}

/* Split SLOT identifying slot for function value or
   argument into two parts SLOT_VAL and SLOT_BND.
   First is the slot for regular value and the other one is
   for bounds.  */
void
chkp_split_slot (rtx slot, rtx *slot_val, rtx *slot_bnd)
{
  int i;
  int val_num = 0;
  int bnd_num = 0;
  rtx *val_tmps;
  rtx *bnd_tmps;

  *slot_bnd = 0;

  if (!slot
      || GET_CODE (slot) != PARALLEL)
    {
      *slot_val = slot;
      return;
    }

  val_tmps = XALLOCAVEC (rtx, XVECLEN (slot, 0));
  bnd_tmps = XALLOCAVEC (rtx, XVECLEN (slot, 0));

  for (i = 0; i < XVECLEN (slot, 0); i++)
    {
      rtx elem = XVECEXP (slot, 0, i);
      rtx reg = GET_CODE (elem) == EXPR_LIST ? XEXP (elem, 0) : elem;

      if (!reg)
	continue;

      if (POINTER_BOUNDS_MODE_P (GET_MODE (reg)) || CONST_INT_P (reg))
	bnd_tmps[bnd_num++] = elem;
      else
	val_tmps[val_num++] = elem;
    }

  gcc_assert (val_num);

  if (!bnd_num)
    {
      *slot_val = slot;
      return;
    }

  if ((GET_CODE (val_tmps[0]) == EXPR_LIST) || (val_num > 1))
    *slot_val = gen_rtx_PARALLEL (GET_MODE (slot),
				  gen_rtvec_v (val_num, val_tmps));
  else
    *slot_val = val_tmps[0];

  if ((GET_CODE (bnd_tmps[0]) == EXPR_LIST) || (bnd_num > 1))
    *slot_bnd = gen_rtx_PARALLEL (VOIDmode,
				  gen_rtvec_v (bnd_num, bnd_tmps));
  else
    *slot_bnd = bnd_tmps[0];
}

/* Join previously splitted to VAL and BND rtx for function
   value or argument and return it.  */
rtx
chkp_join_splitted_slot (rtx val, rtx bnd)
{
  rtx res;
  int i, n = 0;

  if (!bnd)
    return val;

  if (GET_CODE (val) == PARALLEL)
    n += XVECLEN (val, 0);
  else
    n++;

  if (GET_CODE (bnd) == PARALLEL)
    n += XVECLEN (bnd, 0);
  else
    n++;

  res = gen_rtx_PARALLEL (GET_MODE (val), rtvec_alloc (n));

  n = 0;

  if (GET_CODE (val) == PARALLEL)
    for (i = 0; i < XVECLEN (val, 0); i++)
      XVECEXP (res, 0, n++) = XVECEXP (val, 0, i);
  else
    XVECEXP (res, 0, n++) = val;

  if (GET_CODE (bnd) == PARALLEL)
    for (i = 0; i < XVECLEN (bnd, 0); i++)
      XVECEXP (res, 0, n++) = XVECEXP (bnd, 0, i);
  else
    XVECEXP (res, 0, n++) = bnd;

  return res;
}

/* If PAR is PARALLEL holding registers then transform
   it into PARALLEL holding EXPR_LISTs of those regs
   and zero constant (similar to how function value
   on multiple registers looks like).  */
void
chkp_put_regs_to_expr_list (rtx par)
{
  int n;

  if (GET_CODE (par) != PARALLEL
      || GET_CODE (XVECEXP (par, 0, 0)) == EXPR_LIST)
    return;

  for (n = 0; n < XVECLEN (par, 0); n++)
    XVECEXP (par, 0, n) = gen_rtx_EXPR_LIST (VOIDmode,
					     XVECEXP (par, 0, n),
					     const0_rtx);
}

/*  Search rtx PAR describing function return value for an
    item related to value at offset OFFS and return it.
    Return NULL if item was not found.  */
rtx
chkp_get_value_with_offs (rtx par, rtx offs)
{
  int n;

  gcc_assert (GET_CODE (par) == PARALLEL);

  for (n = 0; n < XVECLEN (par, 0); n++)
    {
      rtx par_offs = XEXP (XVECEXP (par, 0, n), 1);
      if (INTVAL (offs) == INTVAL (par_offs))
	return XEXP (XVECEXP (par, 0, n), 0);
    }

  return NULL;
}

/* Emit instructions to store BOUNDS for pointer VALUE
   stored in MEM.
   Function is used by expand to pass bounds for args
   passed on stack.  */
void
chkp_emit_bounds_store (rtx bounds, rtx value, rtx mem)
{
  gcc_assert (MEM_P (mem));

  if (REG_P (bounds) || CONST_INT_P (bounds))
    {
      rtx ptr;

      if (REG_P (value))
	ptr = value;
      else
	{
	  rtx slot = adjust_address (value, Pmode, 0);
	  ptr = gen_reg_rtx (Pmode);
	  emit_move_insn (ptr, slot);
	}

      if (CONST_INT_P (bounds))
	bounds = targetm.calls.load_bounds_for_arg (value, ptr, bounds);

      targetm.calls.store_bounds_for_arg (ptr, mem,
					  bounds, NULL);
    }
  else
    {
      int i;

      gcc_assert (GET_CODE (bounds) == PARALLEL);
      gcc_assert (GET_CODE (value) == PARALLEL || MEM_P (value) || REG_P (value));

      for (i = 0; i < XVECLEN (bounds, 0); i++)
	{
	  rtx reg = XEXP (XVECEXP (bounds, 0, i), 0);
	  rtx offs = XEXP (XVECEXP (bounds, 0, i), 1);
	  rtx slot = adjust_address (mem, Pmode, INTVAL (offs));
	  rtx ptr;

	  if (GET_CODE (value) == PARALLEL)
	    ptr = chkp_get_value_with_offs (value, offs);
	  else if (MEM_P (value))
	    {
	      rtx tmp = adjust_address (value, Pmode, INTVAL (offs));
	      ptr = gen_reg_rtx (Pmode);
	      emit_move_insn (ptr, tmp);
	    }
	  else
	    ptr = gen_rtx_SUBREG (Pmode, value, INTVAL (offs));

	  targetm.calls.store_bounds_for_arg (ptr, slot, reg, NULL);
	}
    }
}

/* Emit code to copy bounds for structure VALUE of type TYPE
   copied to SLOT.  */
void
chkp_copy_bounds_for_stack_parm (rtx slot, rtx value, tree type)
{
  bitmap have_bound;
  bitmap_iterator bi;
  unsigned i;
  rtx tmp = NULL, bnd;

  gcc_assert (TYPE_SIZE (type));
  gcc_assert (MEM_P (value));
  gcc_assert (MEM_P (slot));
  gcc_assert (RECORD_OR_UNION_TYPE_P (type));

  bitmap_obstack_initialize (NULL);
  have_bound = BITMAP_ALLOC (NULL);
  chkp_find_bound_slots (type, have_bound);

  EXECUTE_IF_SET_IN_BITMAP (have_bound, 0, i, bi)
    {
      rtx ptr = adjust_address (value, Pmode, i * POINTER_SIZE / 8);
      rtx to = adjust_address (slot, Pmode, i * POINTER_SIZE / 8);

      if (!tmp)
	tmp = gen_reg_rtx (Pmode);

      emit_move_insn (tmp, ptr);
      bnd = targetm.calls.load_bounds_for_arg (ptr, tmp, NULL);
      targetm.calls.store_bounds_for_arg (tmp, to, bnd, NULL);
    }

  BITMAP_FREE (have_bound);
  bitmap_obstack_release (NULL);
}
