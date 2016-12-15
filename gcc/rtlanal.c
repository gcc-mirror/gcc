/* Analyze RTL for GNU compiler.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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
#include "predict.h"
#include "df.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"  /* FIXME: Can go away once crtl is moved to rtl.h.  */
#include "recog.h"
#include "addresses.h"
#include "rtl-iter.h"

/* Forward declarations */
static void set_of_1 (rtx, const_rtx, void *);
static bool covers_regno_p (const_rtx, unsigned int);
static bool covers_regno_no_parallel_p (const_rtx, unsigned int);
static int computed_jump_p_1 (const_rtx);
static void parms_set (rtx, const_rtx, void *);

static unsigned HOST_WIDE_INT cached_nonzero_bits (const_rtx, machine_mode,
                                                   const_rtx, machine_mode,
                                                   unsigned HOST_WIDE_INT);
static unsigned HOST_WIDE_INT nonzero_bits1 (const_rtx, machine_mode,
					     const_rtx, machine_mode,
                                             unsigned HOST_WIDE_INT);
static unsigned int cached_num_sign_bit_copies (const_rtx, machine_mode, const_rtx,
                                                machine_mode,
                                                unsigned int);
static unsigned int num_sign_bit_copies1 (const_rtx, machine_mode, const_rtx,
                                          machine_mode, unsigned int);

rtx_subrtx_bound_info rtx_all_subrtx_bounds[NUM_RTX_CODE];
rtx_subrtx_bound_info rtx_nonconst_subrtx_bounds[NUM_RTX_CODE];

/* Truncation narrows the mode from SOURCE mode to DESTINATION mode.
   If TARGET_MODE_REP_EXTENDED (DESTINATION, DESTINATION_REP) is
   SIGN_EXTEND then while narrowing we also have to enforce the
   representation and sign-extend the value to mode DESTINATION_REP.

   If the value is already sign-extended to DESTINATION_REP mode we
   can just switch to DESTINATION mode on it.  For each pair of
   integral modes SOURCE and DESTINATION, when truncating from SOURCE
   to DESTINATION, NUM_SIGN_BIT_COPIES_IN_REP[SOURCE][DESTINATION]
   contains the number of high-order bits in SOURCE that have to be
   copies of the sign-bit so that we can do this mode-switch to
   DESTINATION.  */

static unsigned int
num_sign_bit_copies_in_rep[MAX_MODE_INT + 1][MAX_MODE_INT + 1];

/* Store X into index I of ARRAY.  ARRAY is known to have at least I
   elements.  Return the new base of ARRAY.  */

template <typename T>
typename T::value_type *
generic_subrtx_iterator <T>::add_single_to_queue (array_type &array,
						  value_type *base,
						  size_t i, value_type x)
{
  if (base == array.stack)
    {
      if (i < LOCAL_ELEMS)
	{
	  base[i] = x;
	  return base;
	}
      gcc_checking_assert (i == LOCAL_ELEMS);
      /* A previous iteration might also have moved from the stack to the
	 heap, in which case the heap array will already be big enough.  */
      if (vec_safe_length (array.heap) <= i)
	vec_safe_grow (array.heap, i + 1);
      base = array.heap->address ();
      memcpy (base, array.stack, sizeof (array.stack));
      base[LOCAL_ELEMS] = x;
      return base;
    }
  unsigned int length = array.heap->length ();
  if (length > i)
    {
      gcc_checking_assert (base == array.heap->address ());
      base[i] = x;
      return base;
    }
  else
    {
      gcc_checking_assert (i == length);
      vec_safe_push (array.heap, x);
      return array.heap->address ();
    }
}

/* Add the subrtxes of X to worklist ARRAY, starting at END.  Return the
   number of elements added to the worklist.  */

template <typename T>
size_t
generic_subrtx_iterator <T>::add_subrtxes_to_queue (array_type &array,
						    value_type *base,
						    size_t end, rtx_type x)
{
  enum rtx_code code = GET_CODE (x);
  const char *format = GET_RTX_FORMAT (code);
  size_t orig_end = end;
  if (__builtin_expect (INSN_P (x), false))
    {
      /* Put the pattern at the top of the queue, since that's what
	 we're likely to want most.  It also allows for the SEQUENCE
	 code below.  */
      for (int i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; --i)
	if (format[i] == 'e')
	  {
	    value_type subx = T::get_value (x->u.fld[i].rt_rtx);
	    if (__builtin_expect (end < LOCAL_ELEMS, true))
	      base[end++] = subx;
	    else
	      base = add_single_to_queue (array, base, end++, subx);
	  }
    }
  else
    for (int i = 0; format[i]; ++i)
      if (format[i] == 'e')
	{
	  value_type subx = T::get_value (x->u.fld[i].rt_rtx);
	  if (__builtin_expect (end < LOCAL_ELEMS, true))
	    base[end++] = subx;
	  else
	    base = add_single_to_queue (array, base, end++, subx);
	}
      else if (format[i] == 'E')
	{
	  unsigned int length = GET_NUM_ELEM (x->u.fld[i].rt_rtvec);
	  rtx *vec = x->u.fld[i].rt_rtvec->elem;
	  if (__builtin_expect (end + length <= LOCAL_ELEMS, true))
	    for (unsigned int j = 0; j < length; j++)
	      base[end++] = T::get_value (vec[j]);
	  else
	    for (unsigned int j = 0; j < length; j++)
	      base = add_single_to_queue (array, base, end++,
					  T::get_value (vec[j]));
	  if (code == SEQUENCE && end == length)
	    /* If the subrtxes of the sequence fill the entire array then
	       we know that no other parts of a containing insn are queued.
	       The caller is therefore iterating over the sequence as a
	       PATTERN (...), so we also want the patterns of the
	       subinstructions.  */
	    for (unsigned int j = 0; j < length; j++)
	      {
		typename T::rtx_type x = T::get_rtx (base[j]);
		if (INSN_P (x))
		  base[j] = T::get_value (PATTERN (x));
	      }
	}
  return end - orig_end;
}

template <typename T>
void
generic_subrtx_iterator <T>::free_array (array_type &array)
{
  vec_free (array.heap);
}

template <typename T>
const size_t generic_subrtx_iterator <T>::LOCAL_ELEMS;

template class generic_subrtx_iterator <const_rtx_accessor>;
template class generic_subrtx_iterator <rtx_var_accessor>;
template class generic_subrtx_iterator <rtx_ptr_accessor>;

/* Return 1 if the value of X is unstable
   (would be different at a different point in the program).
   The frame pointer, arg pointer, etc. are considered stable
   (within one function) and so is anything marked `unchanging'.  */

int
rtx_unstable_p (const_rtx x)
{
  const RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case MEM:
      return !MEM_READONLY_P (x) || rtx_unstable_p (XEXP (x, 0));

    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case REG:
      /* As in rtx_varies_p, we have to use the actual rtx, not reg number.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  /* The arg pointer varies if it is not a fixed register.  */
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM]))
	return 0;
      /* ??? When call-clobbered, the value is stable modulo the restore
	 that must happen after a call.  This currently screws up local-alloc
	 into believing that the restore is not needed.  */
      if (!PIC_OFFSET_TABLE_REG_CALL_CLOBBERED && x == pic_offset_table_rtx)
	return 0;
      return 1;

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

      /* Fall through.  */

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_unstable_p (XEXP (x, i)))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_unstable_p (XVECEXP (x, i, j)))
	    return 1;
      }

  return 0;
}

/* Return 1 if X has a value that can vary even between two
   executions of the program.  0 means X can be compared reliably
   against certain constants or near-constants.
   FOR_ALIAS is nonzero if we are called from alias analysis; if it is
   zero, we are slightly more conservative.
   The frame pointer and the arg pointer are considered constant.  */

bool
rtx_varies_p (const_rtx x, bool for_alias)
{
  RTX_CODE code;
  int i;
  const char *fmt;

  if (!x)
    return 0;

  code = GET_CODE (x);
  switch (code)
    {
    case MEM:
      return !MEM_READONLY_P (x) || rtx_varies_p (XEXP (x, 0), for_alias);

    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case REG:
      /* Note that we have to test for the actual rtx used for the frame
	 and arg pointers and not just the register number in case we have
	 eliminated the frame and/or arg pointer and are using it
	 for pseudos.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  /* The arg pointer varies if it is not a fixed register.  */
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM]))
	return 0;
      if (x == pic_offset_table_rtx
	  /* ??? When call-clobbered, the value is stable modulo the restore
	     that must happen after a call.  This currently screws up
	     local-alloc into believing that the restore is not needed, so we
	     must return 0 only if we are called from alias analysis.  */
	  && (!PIC_OFFSET_TABLE_REG_CALL_CLOBBERED || for_alias))
	return 0;
      return 1;

    case LO_SUM:
      /* The operand 0 of a LO_SUM is considered constant
	 (in fact it is related specifically to operand 1)
	 during alias analysis.  */
      return (! for_alias && rtx_varies_p (XEXP (x, 0), for_alias))
	     || rtx_varies_p (XEXP (x, 1), for_alias);

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

      /* Fall through.  */

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_varies_p (XEXP (x, i), for_alias))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_varies_p (XVECEXP (x, i, j), for_alias))
	    return 1;
      }

  return 0;
}

/* Compute an approximation for the offset between the register
   FROM and TO for the current function, as it was at the start
   of the routine.  */

static HOST_WIDE_INT
get_initial_register_offset (int from, int to)
{
#ifdef ELIMINABLE_REGS
  static const struct elim_table_t
  {
    const int from;
    const int to;
  } table[] = ELIMINABLE_REGS;
  HOST_WIDE_INT offset1, offset2;
  unsigned int i, j;

  if (to == from)
    return 0;

  /* It is not safe to call INITIAL_ELIMINATION_OFFSET
     before the reload pass.  We need to give at least
     an estimation for the resulting frame size.  */
  if (! reload_completed)
    {
      offset1 = crtl->outgoing_args_size + get_frame_size ();
#if !STACK_GROWS_DOWNWARD
      offset1 = - offset1;
#endif
      if (to == STACK_POINTER_REGNUM)
	return offset1;
      else if (from == STACK_POINTER_REGNUM)
	return - offset1;
      else
	return 0;
     }

  for (i = 0; i < ARRAY_SIZE (table); i++)
      if (table[i].from == from)
	{
	  if (table[i].to == to)
	    {
	      INITIAL_ELIMINATION_OFFSET (table[i].from, table[i].to,
					  offset1);
	      return offset1;
	    }
	  for (j = 0; j < ARRAY_SIZE (table); j++)
	    {
	      if (table[j].to == to
		  && table[j].from == table[i].to)
		{
		  INITIAL_ELIMINATION_OFFSET (table[i].from, table[i].to,
					      offset1);
		  INITIAL_ELIMINATION_OFFSET (table[j].from, table[j].to,
					      offset2);
		  return offset1 + offset2;
		}
	      if (table[j].from == to
		  && table[j].to == table[i].to)
		{
		  INITIAL_ELIMINATION_OFFSET (table[i].from, table[i].to,
					      offset1);
		  INITIAL_ELIMINATION_OFFSET (table[j].from, table[j].to,
					      offset2);
		  return offset1 - offset2;
		}
	    }
	}
      else if (table[i].to == from)
	{
	  if (table[i].from == to)
	    {
	      INITIAL_ELIMINATION_OFFSET (table[i].from, table[i].to,
					  offset1);
	      return - offset1;
	    }
	  for (j = 0; j < ARRAY_SIZE (table); j++)
	    {
	      if (table[j].to == to
		  && table[j].from == table[i].from)
		{
		  INITIAL_ELIMINATION_OFFSET (table[i].from, table[i].to,
					      offset1);
		  INITIAL_ELIMINATION_OFFSET (table[j].from, table[j].to,
					      offset2);
		  return - offset1 + offset2;
		}
	      if (table[j].from == to
		  && table[j].to == table[i].from)
		{
		  INITIAL_ELIMINATION_OFFSET (table[i].from, table[i].to,
					      offset1);
		  INITIAL_ELIMINATION_OFFSET (table[j].from, table[j].to,
					      offset2);
		  return - offset1 - offset2;
		}
	    }
	}

  /* If the requested register combination was not found,
     try a different more simple combination.  */
  if (from == ARG_POINTER_REGNUM)
    return get_initial_register_offset (HARD_FRAME_POINTER_REGNUM, to);
  else if (to == ARG_POINTER_REGNUM)
    return get_initial_register_offset (from, HARD_FRAME_POINTER_REGNUM);
  else if (from == HARD_FRAME_POINTER_REGNUM)
    return get_initial_register_offset (FRAME_POINTER_REGNUM, to);
  else if (to == HARD_FRAME_POINTER_REGNUM)
    return get_initial_register_offset (from, FRAME_POINTER_REGNUM);
  else
    return 0;

#else
  HOST_WIDE_INT offset;

  if (to == from)
    return 0;

  if (reload_completed)
    {
      INITIAL_FRAME_POINTER_OFFSET (offset);
    }
  else
    {
      offset = crtl->outgoing_args_size + get_frame_size ();
#if !STACK_GROWS_DOWNWARD
      offset = - offset;
#endif
    }

  if (to == STACK_POINTER_REGNUM)
    return offset;
  else if (from == STACK_POINTER_REGNUM)
    return - offset;
  else
    return 0;

#endif
}

/* Return nonzero if the use of X+OFFSET as an address in a MEM with SIZE
   bytes can cause a trap.  MODE is the mode of the MEM (not that of X) and
   UNALIGNED_MEMS controls whether nonzero is returned for unaligned memory
   references on strict alignment machines.  */

static int
rtx_addr_can_trap_p_1 (const_rtx x, HOST_WIDE_INT offset, HOST_WIDE_INT size,
		       machine_mode mode, bool unaligned_mems)
{
  enum rtx_code code = GET_CODE (x);

  /* The offset must be a multiple of the mode size if we are considering
     unaligned memory references on strict alignment machines.  */
  if (STRICT_ALIGNMENT && unaligned_mems && GET_MODE_SIZE (mode) != 0)
    {
      HOST_WIDE_INT actual_offset = offset;

#ifdef SPARC_STACK_BOUNDARY_HACK
      /* ??? The SPARC port may claim a STACK_BOUNDARY higher than
	     the real alignment of %sp.  However, when it does this, the
	     alignment of %sp+STACK_POINTER_OFFSET is STACK_BOUNDARY.  */
      if (SPARC_STACK_BOUNDARY_HACK
	  && (x == stack_pointer_rtx || x == hard_frame_pointer_rtx))
	actual_offset -= STACK_POINTER_OFFSET;
#endif

      if (actual_offset % GET_MODE_SIZE (mode) != 0)
	return 1;
    }

  switch (code)
    {
    case SYMBOL_REF:
      if (SYMBOL_REF_WEAK (x))
	return 1;
      if (!CONSTANT_POOL_ADDRESS_P (x))
	{
	  tree decl;
	  HOST_WIDE_INT decl_size;

	  if (offset < 0)
	    return 1;
	  if (size == 0)
	    size = GET_MODE_SIZE (mode);
	  if (size == 0)
	    return offset != 0;

	  /* If the size of the access or of the symbol is unknown,
	     assume the worst.  */
	  decl = SYMBOL_REF_DECL (x);

	  /* Else check that the access is in bounds.  TODO: restructure
	     expr_size/tree_expr_size/int_expr_size and just use the latter.  */
	  if (!decl)
	    decl_size = -1;
	  else if (DECL_P (decl) && DECL_SIZE_UNIT (decl))
	    decl_size = (tree_fits_shwi_p (DECL_SIZE_UNIT (decl))
			 ? tree_to_shwi (DECL_SIZE_UNIT (decl))
			 : -1);
	  else if (TREE_CODE (decl) == STRING_CST)
	    decl_size = TREE_STRING_LENGTH (decl);
	  else if (TYPE_SIZE_UNIT (TREE_TYPE (decl)))
	    decl_size = int_size_in_bytes (TREE_TYPE (decl));
	  else
	    decl_size = -1;

	  return (decl_size <= 0 ? offset != 0 : offset + size > decl_size);
        }

      return 0;

    case LABEL_REF:
      return 0;

    case REG:
      /* Stack references are assumed not to trap, but we need to deal with
	 nonsensical offsets.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	 || x == stack_pointer_rtx
	 /* The arg pointer varies if it is not a fixed register.  */
	 || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM]))
	{
#ifdef RED_ZONE_SIZE
	  HOST_WIDE_INT red_zone_size = RED_ZONE_SIZE;
#else
	  HOST_WIDE_INT red_zone_size = 0;
#endif
	  HOST_WIDE_INT stack_boundary = PREFERRED_STACK_BOUNDARY
					 / BITS_PER_UNIT;
	  HOST_WIDE_INT low_bound, high_bound;

	  if (size == 0)
	    size = GET_MODE_SIZE (mode);

	  if (x == frame_pointer_rtx)
	    {
	      if (FRAME_GROWS_DOWNWARD)
		{
		  high_bound = STARTING_FRAME_OFFSET;
		  low_bound  = high_bound - get_frame_size ();
		}
	      else
		{
		  low_bound  = STARTING_FRAME_OFFSET;
		  high_bound = low_bound + get_frame_size ();
		}
	    }
	  else if (x == hard_frame_pointer_rtx)
	    {
	      HOST_WIDE_INT sp_offset
		= get_initial_register_offset (STACK_POINTER_REGNUM,
					       HARD_FRAME_POINTER_REGNUM);
	      HOST_WIDE_INT ap_offset
		= get_initial_register_offset (ARG_POINTER_REGNUM,
					       HARD_FRAME_POINTER_REGNUM);

#if STACK_GROWS_DOWNWARD
	      low_bound  = sp_offset - red_zone_size - stack_boundary;
	      high_bound = ap_offset
			   + FIRST_PARM_OFFSET (current_function_decl)
#if !ARGS_GROW_DOWNWARD
			   + crtl->args.size
#endif
			   + stack_boundary;
#else
	      high_bound = sp_offset + red_zone_size + stack_boundary;
	      low_bound  = ap_offset
			   + FIRST_PARM_OFFSET (current_function_decl)
#if ARGS_GROW_DOWNWARD
			   - crtl->args.size
#endif
			   - stack_boundary;
#endif
	    }
	  else if (x == stack_pointer_rtx)
	    {
	      HOST_WIDE_INT ap_offset
		= get_initial_register_offset (ARG_POINTER_REGNUM,
					       STACK_POINTER_REGNUM);

#if STACK_GROWS_DOWNWARD
	      low_bound  = - red_zone_size - stack_boundary;
	      high_bound = ap_offset
			   + FIRST_PARM_OFFSET (current_function_decl)
#if !ARGS_GROW_DOWNWARD
			   + crtl->args.size
#endif
			   + stack_boundary;
#else
	      high_bound = red_zone_size + stack_boundary;
	      low_bound  = ap_offset
			   + FIRST_PARM_OFFSET (current_function_decl)
#if ARGS_GROW_DOWNWARD
			   - crtl->args.size
#endif
			   - stack_boundary;
#endif
	    }
	  else
	    {
	      /* We assume that accesses are safe to at least the
		 next stack boundary.
		 Examples are varargs and __builtin_return_address.  */
#if ARGS_GROW_DOWNWARD
	      high_bound = FIRST_PARM_OFFSET (current_function_decl)
			   + stack_boundary;
	      low_bound  = FIRST_PARM_OFFSET (current_function_decl)
			   - crtl->args.size - stack_boundary;
#else
	      low_bound  = FIRST_PARM_OFFSET (current_function_decl)
			   - stack_boundary;
	      high_bound = FIRST_PARM_OFFSET (current_function_decl)
			   + crtl->args.size + stack_boundary;
#endif
	    }

	  if (offset >= low_bound && offset <= high_bound - size)
	    return 0;
	  return 1;
	}
      /* All of the virtual frame registers are stack references.  */
      if (REGNO (x) >= FIRST_VIRTUAL_REGISTER
	  && REGNO (x) <= LAST_VIRTUAL_REGISTER)
	return 0;
      return 1;

    case CONST:
      return rtx_addr_can_trap_p_1 (XEXP (x, 0), offset, size,
				    mode, unaligned_mems);

    case PLUS:
      /* An address is assumed not to trap if:
         - it is the pic register plus a constant.  */
      if (XEXP (x, 0) == pic_offset_table_rtx && CONSTANT_P (XEXP (x, 1)))
	return 0;

      /* - or it is an address that can't trap plus a constant integer.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && !rtx_addr_can_trap_p_1 (XEXP (x, 0), offset + INTVAL (XEXP (x, 1)),
				     size, mode, unaligned_mems))
	return 0;

      return 1;

    case LO_SUM:
    case PRE_MODIFY:
      return rtx_addr_can_trap_p_1 (XEXP (x, 1), offset, size,
				    mode, unaligned_mems);

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      return rtx_addr_can_trap_p_1 (XEXP (x, 0), offset, size,
				    mode, unaligned_mems);

    default:
      break;
    }

  /* If it isn't one of the case above, it can cause a trap.  */
  return 1;
}

/* Return nonzero if the use of X as an address in a MEM can cause a trap.  */

int
rtx_addr_can_trap_p (const_rtx x)
{
  return rtx_addr_can_trap_p_1 (x, 0, 0, VOIDmode, false);
}

/* Return true if X is an address that is known to not be zero.  */

bool
nonzero_address_p (const_rtx x)
{
  const enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case SYMBOL_REF:
      return flag_delete_null_pointer_checks && !SYMBOL_REF_WEAK (x);

    case LABEL_REF:
      return true;

    case REG:
      /* As in rtx_varies_p, we have to use the actual rtx, not reg number.  */
      if (x == frame_pointer_rtx || x == hard_frame_pointer_rtx
	  || x == stack_pointer_rtx
	  || (x == arg_pointer_rtx && fixed_regs[ARG_POINTER_REGNUM]))
	return true;
      /* All of the virtual frame registers are stack references.  */
      if (REGNO (x) >= FIRST_VIRTUAL_REGISTER
	  && REGNO (x) <= LAST_VIRTUAL_REGISTER)
	return true;
      return false;

    case CONST:
      return nonzero_address_p (XEXP (x, 0));

    case PLUS:
      /* Handle PIC references.  */
      if (XEXP (x, 0) == pic_offset_table_rtx
	       && CONSTANT_P (XEXP (x, 1)))
	return true;
      return false;

    case PRE_MODIFY:
      /* Similar to the above; allow positive offsets.  Further, since
	 auto-inc is only allowed in memories, the register must be a
	 pointer.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) > 0)
	return true;
      return nonzero_address_p (XEXP (x, 0));

    case PRE_INC:
      /* Similarly.  Further, the offset is always positive.  */
      return true;

    case PRE_DEC:
    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      return nonzero_address_p (XEXP (x, 0));

    case LO_SUM:
      return nonzero_address_p (XEXP (x, 1));

    default:
      break;
    }

  /* If it isn't one of the case above, might be zero.  */
  return false;
}

/* Return 1 if X refers to a memory location whose address
   cannot be compared reliably with constant addresses,
   or if X refers to a BLKmode memory object.
   FOR_ALIAS is nonzero if we are called from alias analysis; if it is
   zero, we are slightly more conservative.  */

bool
rtx_addr_varies_p (const_rtx x, bool for_alias)
{
  enum rtx_code code;
  int i;
  const char *fmt;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  if (code == MEM)
    return GET_MODE (x) == BLKmode || rtx_varies_p (XEXP (x, 0), for_alias);

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (rtx_addr_varies_p (XEXP (x, i), for_alias))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (rtx_addr_varies_p (XVECEXP (x, i, j), for_alias))
	    return 1;
      }
  return 0;
}

/* Return the CALL in X if there is one.  */

rtx
get_call_rtx_from (rtx x)
{
  if (INSN_P (x))
    x = PATTERN (x);
  if (GET_CODE (x) == PARALLEL)
    x = XVECEXP (x, 0, 0);
  if (GET_CODE (x) == SET)
    x = SET_SRC (x);
  if (GET_CODE (x) == CALL && MEM_P (XEXP (x, 0)))
    return x;
  return NULL_RTX;
}

/* Return the value of the integer term in X, if one is apparent;
   otherwise return 0.
   Only obvious integer terms are detected.
   This is used in cse.c with the `related_value' field.  */

HOST_WIDE_INT
get_integer_term (const_rtx x)
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == MINUS
      && CONST_INT_P (XEXP (x, 1)))
    return - INTVAL (XEXP (x, 1));
  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1)))
    return INTVAL (XEXP (x, 1));
  return 0;
}

/* If X is a constant, return the value sans apparent integer term;
   otherwise return 0.
   Only obvious integer terms are detected.  */

rtx
get_related_value (const_rtx x)
{
  if (GET_CODE (x) != CONST)
    return 0;
  x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1)))
    return XEXP (x, 0);
  else if (GET_CODE (x) == MINUS
	   && CONST_INT_P (XEXP (x, 1)))
    return XEXP (x, 0);
  return 0;
}

/* Return true if SYMBOL is a SYMBOL_REF and OFFSET + SYMBOL points
   to somewhere in the same object or object_block as SYMBOL.  */

bool
offset_within_block_p (const_rtx symbol, HOST_WIDE_INT offset)
{
  tree decl;

  if (GET_CODE (symbol) != SYMBOL_REF)
    return false;

  if (offset == 0)
    return true;

  if (offset > 0)
    {
      if (CONSTANT_POOL_ADDRESS_P (symbol)
	  && offset < (int) GET_MODE_SIZE (get_pool_mode (symbol)))
	return true;

      decl = SYMBOL_REF_DECL (symbol);
      if (decl && offset < int_size_in_bytes (TREE_TYPE (decl)))
	return true;
    }

  if (SYMBOL_REF_HAS_BLOCK_INFO_P (symbol)
      && SYMBOL_REF_BLOCK (symbol)
      && SYMBOL_REF_BLOCK_OFFSET (symbol) >= 0
      && ((unsigned HOST_WIDE_INT) offset + SYMBOL_REF_BLOCK_OFFSET (symbol)
	  < (unsigned HOST_WIDE_INT) SYMBOL_REF_BLOCK (symbol)->size))
    return true;

  return false;
}

/* Split X into a base and a constant offset, storing them in *BASE_OUT
   and *OFFSET_OUT respectively.  */

void
split_const (rtx x, rtx *base_out, rtx *offset_out)
{
  if (GET_CODE (x) == CONST)
    {
      x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
	{
	  *base_out = XEXP (x, 0);
	  *offset_out = XEXP (x, 1);
	  return;
	}
    }
  *base_out = x;
  *offset_out = const0_rtx;
}

/* Return the number of places FIND appears within X.  If COUNT_DEST is
   zero, we do not count occurrences inside the destination of a SET.  */

int
count_occurrences (const_rtx x, const_rtx find, int count_dest)
{
  int i, j;
  enum rtx_code code;
  const char *format_ptr;
  int count;

  if (x == find)
    return 1;

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
      return 0;

    case EXPR_LIST:
      count = count_occurrences (XEXP (x, 0), find, count_dest);
      if (XEXP (x, 1))
	count += count_occurrences (XEXP (x, 1), find, count_dest);
      return count;

    case MEM:
      if (MEM_P (find) && rtx_equal_p (x, find))
	return 1;
      break;

    case SET:
      if (SET_DEST (x) == find && ! count_dest)
	return count_occurrences (SET_SRC (x), find, count_dest);
      break;

    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (code);
  count = 0;

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  count += count_occurrences (XEXP (x, i), find, count_dest);
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (x, i); j++)
	    count += count_occurrences (XVECEXP (x, i, j), find, count_dest);
	  break;
	}
    }
  return count;
}


/* Return TRUE if OP is a register or subreg of a register that
   holds an unsigned quantity.  Otherwise, return FALSE.  */

bool
unsigned_reg_p (rtx op)
{
  if (REG_P (op)
      && REG_EXPR (op)
      && TYPE_UNSIGNED (TREE_TYPE (REG_EXPR (op))))
    return true;

  if (GET_CODE (op) == SUBREG
      && SUBREG_PROMOTED_SIGN (op))
    return true;

  return false;
}


/* Nonzero if register REG appears somewhere within IN.
   Also works if REG is not a register; in this case it checks
   for a subexpression of IN that is Lisp "equal" to REG.  */

int
reg_mentioned_p (const_rtx reg, const_rtx in)
{
  const char *fmt;
  int i;
  enum rtx_code code;

  if (in == 0)
    return 0;

  if (reg == in)
    return 1;

  if (GET_CODE (in) == LABEL_REF)
    return reg == LABEL_REF_LABEL (in);

  code = GET_CODE (in);

  switch (code)
    {
      /* Compare registers by number.  */
    case REG:
      return REG_P (reg) && REGNO (in) == REGNO (reg);

      /* These codes have no constituent expressions
	 and are unique.  */
    case SCRATCH:
    case CC0:
    case PC:
      return 0;

    CASE_CONST_ANY:
      /* These are kept unique for a given value.  */
      return 0;

    default:
      break;
    }

  if (GET_CODE (reg) == code && rtx_equal_p (reg, in))
    return 1;

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	    if (reg_mentioned_p (reg, XVECEXP (in, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && reg_mentioned_p (reg, XEXP (in, i)))
	return 1;
    }
  return 0;
}

/* Return 1 if in between BEG and END, exclusive of BEG and END, there is
   no CODE_LABEL insn.  */

int
no_labels_between_p (const rtx_insn *beg, const rtx_insn *end)
{
  rtx_insn *p;
  if (beg == end)
    return 0;
  for (p = NEXT_INSN (beg); p != end; p = NEXT_INSN (p))
    if (LABEL_P (p))
      return 0;
  return 1;
}

/* Nonzero if register REG is used in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).  */

int
reg_used_between_p (const_rtx reg, const rtx_insn *from_insn,
		    const rtx_insn *to_insn)
{
  rtx_insn *insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn)
	&& (reg_overlap_mentioned_p (reg, PATTERN (insn))
	   || (CALL_P (insn) && find_reg_fusage (insn, USE, reg))))
      return 1;
  return 0;
}

/* Nonzero if the old value of X, a register, is referenced in BODY.  If X
   is entirely replaced by a new value and the only use is as a SET_DEST,
   we do not consider it a reference.  */

int
reg_referenced_p (const_rtx x, const_rtx body)
{
  int i;

  switch (GET_CODE (body))
    {
    case SET:
      if (reg_overlap_mentioned_p (x, SET_SRC (body)))
	return 1;

      /* If the destination is anything other than CC0, PC, a REG or a SUBREG
	 of a REG that occupies all of the REG, the insn references X if
	 it is mentioned in the destination.  */
      if (GET_CODE (SET_DEST (body)) != CC0
	  && GET_CODE (SET_DEST (body)) != PC
	  && !REG_P (SET_DEST (body))
	  && ! (GET_CODE (SET_DEST (body)) == SUBREG
		&& REG_P (SUBREG_REG (SET_DEST (body)))
		&& (((GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_DEST (body))))
		      + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)
		    == ((GET_MODE_SIZE (GET_MODE (SET_DEST (body)))
			 + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)))
	  && reg_overlap_mentioned_p (x, SET_DEST (body)))
	return 1;
      return 0;

    case ASM_OPERANDS:
      for (i = ASM_OPERANDS_INPUT_LENGTH (body) - 1; i >= 0; i--)
	if (reg_overlap_mentioned_p (x, ASM_OPERANDS_INPUT (body, i)))
	  return 1;
      return 0;

    case CALL:
    case USE:
    case IF_THEN_ELSE:
      return reg_overlap_mentioned_p (x, body);

    case TRAP_IF:
      return reg_overlap_mentioned_p (x, TRAP_CONDITION (body));

    case PREFETCH:
      return reg_overlap_mentioned_p (x, XEXP (body, 0));

    case UNSPEC:
    case UNSPEC_VOLATILE:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	if (reg_overlap_mentioned_p (x, XVECEXP (body, 0, i)))
	  return 1;
      return 0;

    case PARALLEL:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	if (reg_referenced_p (x, XVECEXP (body, 0, i)))
	  return 1;
      return 0;

    case CLOBBER:
      if (MEM_P (XEXP (body, 0)))
	if (reg_overlap_mentioned_p (x, XEXP (XEXP (body, 0), 0)))
	  return 1;
      return 0;

    case COND_EXEC:
      if (reg_overlap_mentioned_p (x, COND_EXEC_TEST (body)))
	return 1;
      return reg_referenced_p (x, COND_EXEC_CODE (body));

    default:
      return 0;
    }
}

/* Nonzero if register REG is set or clobbered in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).  */

int
reg_set_between_p (const_rtx reg, const rtx_insn *from_insn,
		   const rtx_insn *to_insn)
{
  const rtx_insn *insn;

  if (from_insn == to_insn)
    return 0;

  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && reg_set_p (reg, insn))
      return 1;
  return 0;
}

/* Return true if REG is set or clobbered inside INSN.  */

int
reg_set_p (const_rtx reg, const_rtx insn)
{
  /* After delay slot handling, call and branch insns might be in a
     sequence.  Check all the elements there.  */
  if (INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      for (int i = 0; i < XVECLEN (PATTERN (insn), 0); ++i)
	if (reg_set_p (reg, XVECEXP (PATTERN (insn), 0, i)))
	  return true;

      return false;
    }

  /* We can be passed an insn or part of one.  If we are passed an insn,
     check if a side-effect of the insn clobbers REG.  */
  if (INSN_P (insn)
      && (FIND_REG_INC_NOTE (insn, reg)
	  || (CALL_P (insn)
	      && ((REG_P (reg)
		   && REGNO (reg) < FIRST_PSEUDO_REGISTER
		   && overlaps_hard_reg_set_p (regs_invalidated_by_call,
					       GET_MODE (reg), REGNO (reg)))
		  || MEM_P (reg)
		  || find_reg_fusage (insn, CLOBBER, reg)))))
    return true;

  return set_of (reg, insn) != NULL_RTX;
}

/* Similar to reg_set_between_p, but check all registers in X.  Return 0
   only if none of them are modified between START and END.  Return 1 if
   X contains a MEM; this routine does use memory aliasing.  */

int
modified_between_p (const_rtx x, const rtx_insn *start, const rtx_insn *end)
{
  const enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;
  rtx_insn *insn;

  if (start == end)
    return 0;

  switch (code)
    {
    CASE_CONST_ANY:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case PC:
    case CC0:
      return 1;

    case MEM:
      if (modified_between_p (XEXP (x, 0), start, end))
	return 1;
      if (MEM_READONLY_P (x))
	return 0;
      for (insn = NEXT_INSN (start); insn != end; insn = NEXT_INSN (insn))
	if (memory_modified_in_insn_p (x, insn))
	  return 1;
      return 0;
      break;

    case REG:
      return reg_set_between_p (x, start, end);

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && modified_between_p (XEXP (x, i), start, end))
	return 1;

      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (modified_between_p (XVECEXP (x, i, j), start, end))
	    return 1;
    }

  return 0;
}

/* Similar to reg_set_p, but check all registers in X.  Return 0 only if none
   of them are modified in INSN.  Return 1 if X contains a MEM; this routine
   does use memory aliasing.  */

int
modified_in_p (const_rtx x, const_rtx insn)
{
  const enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  switch (code)
    {
    CASE_CONST_ANY:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case PC:
    case CC0:
      return 1;

    case MEM:
      if (modified_in_p (XEXP (x, 0), insn))
	return 1;
      if (MEM_READONLY_P (x))
	return 0;
      if (memory_modified_in_insn_p (x, insn))
	return 1;
      return 0;
      break;

    case REG:
      return reg_set_p (x, insn);

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && modified_in_p (XEXP (x, i), insn))
	return 1;

      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (modified_in_p (XVECEXP (x, i, j), insn))
	    return 1;
    }

  return 0;
}

/* Helper function for set_of.  */
struct set_of_data
  {
    const_rtx found;
    const_rtx pat;
  };

static void
set_of_1 (rtx x, const_rtx pat, void *data1)
{
  struct set_of_data *const data = (struct set_of_data *) (data1);
  if (rtx_equal_p (x, data->pat)
      || (!MEM_P (x) && reg_overlap_mentioned_p (data->pat, x)))
    data->found = pat;
}

/* Give an INSN, return a SET or CLOBBER expression that does modify PAT
   (either directly or via STRICT_LOW_PART and similar modifiers).  */
const_rtx
set_of (const_rtx pat, const_rtx insn)
{
  struct set_of_data data;
  data.found = NULL_RTX;
  data.pat = pat;
  note_stores (INSN_P (insn) ? PATTERN (insn) : insn, set_of_1, &data);
  return data.found;
}

/* Add all hard register in X to *PSET.  */
void
find_all_hard_regs (const_rtx x, HARD_REG_SET *pset)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER)
	add_to_hard_reg_set (pset, GET_MODE (x), REGNO (x));
    }
}

/* This function, called through note_stores, collects sets and
   clobbers of hard registers in a HARD_REG_SET, which is pointed to
   by DATA.  */
void
record_hard_reg_sets (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  HARD_REG_SET *pset = (HARD_REG_SET *)data;
  if (REG_P (x) && HARD_REGISTER_P (x))
    add_to_hard_reg_set (pset, GET_MODE (x), REGNO (x));
}

/* Examine INSN, and compute the set of hard registers written by it.
   Store it in *PSET.  Should only be called after reload.  */
void
find_all_hard_reg_sets (const rtx_insn *insn, HARD_REG_SET *pset, bool implicit)
{
  rtx link;

  CLEAR_HARD_REG_SET (*pset);
  note_stores (PATTERN (insn), record_hard_reg_sets, pset);
  if (CALL_P (insn))
    {
      if (implicit)
	IOR_HARD_REG_SET (*pset, call_used_reg_set);

      for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
	record_hard_reg_sets (XEXP (link, 0), NULL, pset);
    }
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_INC)
      record_hard_reg_sets (XEXP (link, 0), NULL, pset);
}

/* Like record_hard_reg_sets, but called through note_uses.  */
void
record_hard_reg_uses (rtx *px, void *data)
{
  find_all_hard_regs (*px, (HARD_REG_SET *) data);
}

/* Given an INSN, return a SET expression if this insn has only a single SET.
   It may also have CLOBBERs, USEs, or SET whose output
   will not be used, which we ignore.  */

rtx
single_set_2 (const rtx_insn *insn, const_rtx pat)
{
  rtx set = NULL;
  int set_verified = 1;
  int i;

  if (GET_CODE (pat) == PARALLEL)
    {
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx sub = XVECEXP (pat, 0, i);
	  switch (GET_CODE (sub))
	    {
	    case USE:
	    case CLOBBER:
	      break;

	    case SET:
	      /* We can consider insns having multiple sets, where all
		 but one are dead as single set insns.  In common case
		 only single set is present in the pattern so we want
		 to avoid checking for REG_UNUSED notes unless necessary.

		 When we reach set first time, we just expect this is
		 the single set we are looking for and only when more
		 sets are found in the insn, we check them.  */
	      if (!set_verified)
		{
		  if (find_reg_note (insn, REG_UNUSED, SET_DEST (set))
		      && !side_effects_p (set))
		    set = NULL;
		  else
		    set_verified = 1;
		}
	      if (!set)
		set = sub, set_verified = 0;
	      else if (!find_reg_note (insn, REG_UNUSED, SET_DEST (sub))
		       || side_effects_p (sub))
		return NULL_RTX;
	      break;

	    default:
	      return NULL_RTX;
	    }
	}
    }
  return set;
}

/* Given an INSN, return nonzero if it has more than one SET, else return
   zero.  */

int
multiple_sets (const_rtx insn)
{
  int found;
  int i;

  /* INSN must be an insn.  */
  if (! INSN_P (insn))
    return 0;

  /* Only a PARALLEL can have multiple SETs.  */
  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      for (i = 0, found = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET)
	  {
	    /* If we have already found a SET, then return now.  */
	    if (found)
	      return 1;
	    else
	      found = 1;
	  }
    }

  /* Either zero or one SET.  */
  return 0;
}

/* Return nonzero if the destination of SET equals the source
   and there are no side effects.  */

int
set_noop_p (const_rtx set)
{
  rtx src = SET_SRC (set);
  rtx dst = SET_DEST (set);

  if (dst == pc_rtx && src == pc_rtx)
    return 1;

  if (MEM_P (dst) && MEM_P (src))
    return rtx_equal_p (dst, src) && !side_effects_p (dst);

  if (GET_CODE (dst) == ZERO_EXTRACT)
    return rtx_equal_p (XEXP (dst, 0), src)
	   && !BITS_BIG_ENDIAN && XEXP (dst, 2) == const0_rtx
	   && !side_effects_p (src);

  if (GET_CODE (dst) == STRICT_LOW_PART)
    dst = XEXP (dst, 0);

  if (GET_CODE (src) == SUBREG && GET_CODE (dst) == SUBREG)
    {
      if (SUBREG_BYTE (src) != SUBREG_BYTE (dst))
	return 0;
      src = SUBREG_REG (src);
      dst = SUBREG_REG (dst);
    }

  /* It is a NOOP if destination overlaps with selected src vector
     elements.  */
  if (GET_CODE (src) == VEC_SELECT
      && REG_P (XEXP (src, 0)) && REG_P (dst)
      && HARD_REGISTER_P (XEXP (src, 0))
      && HARD_REGISTER_P (dst))
    {
      int i;
      rtx par = XEXP (src, 1);
      rtx src0 = XEXP (src, 0);
      int c0 = INTVAL (XVECEXP (par, 0, 0));
      HOST_WIDE_INT offset = GET_MODE_UNIT_SIZE (GET_MODE (src0)) * c0;

      for (i = 1; i < XVECLEN (par, 0); i++)
	if (INTVAL (XVECEXP (par, 0, i)) != c0 + i)
	  return 0;
      return
	simplify_subreg_regno (REGNO (src0), GET_MODE (src0),
			       offset, GET_MODE (dst)) == (int) REGNO (dst);
    }

  return (REG_P (src) && REG_P (dst)
	  && REGNO (src) == REGNO (dst));
}

/* Return nonzero if an insn consists only of SETs, each of which only sets a
   value to itself.  */

int
noop_move_p (const rtx_insn *insn)
{
  rtx pat = PATTERN (insn);

  if (INSN_CODE (insn) == NOOP_MOVE_INSN_CODE)
    return 1;

  /* Insns carrying these notes are useful later on.  */
  if (find_reg_note (insn, REG_EQUAL, NULL_RTX))
    return 0;

  /* Check the code to be executed for COND_EXEC.  */
  if (GET_CODE (pat) == COND_EXEC)
    pat = COND_EXEC_CODE (pat);

  if (GET_CODE (pat) == SET && set_noop_p (pat))
    return 1;

  if (GET_CODE (pat) == PARALLEL)
    {
      int i;
      /* If nothing but SETs of registers to themselves,
	 this insn can also be deleted.  */
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx tem = XVECEXP (pat, 0, i);

	  if (GET_CODE (tem) == USE
	      || GET_CODE (tem) == CLOBBER)
	    continue;

	  if (GET_CODE (tem) != SET || ! set_noop_p (tem))
	    return 0;
	}

      return 1;
    }
  return 0;
}


/* Return nonzero if register in range [REGNO, ENDREGNO)
   appears either explicitly or implicitly in X
   other than being stored into.

   References contained within the substructure at LOC do not count.
   LOC may be zero, meaning don't ignore anything.  */

bool
refers_to_regno_p (unsigned int regno, unsigned int endregno, const_rtx x,
		   rtx *loc)
{
  int i;
  unsigned int x_regno;
  RTX_CODE code;
  const char *fmt;

 repeat:
  /* The contents of a REG_NONNEG note is always zero, so we must come here
     upon repeat in case the last REG_NOTE is a REG_NONNEG note.  */
  if (x == 0)
    return false;

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
      x_regno = REGNO (x);

      /* If we modifying the stack, frame, or argument pointer, it will
	 clobber a virtual register.  In fact, we could be more precise,
	 but it isn't worth it.  */
      if ((x_regno == STACK_POINTER_REGNUM
	   || (FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	       && x_regno == ARG_POINTER_REGNUM)
	   || x_regno == FRAME_POINTER_REGNUM)
	  && regno >= FIRST_VIRTUAL_REGISTER && regno <= LAST_VIRTUAL_REGISTER)
	return true;

      return endregno > x_regno && regno < END_REGNO (x);

    case SUBREG:
      /* If this is a SUBREG of a hard reg, we can see exactly which
	 registers are being modified.  Otherwise, handle normally.  */
      if (REG_P (SUBREG_REG (x))
	  && REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER)
	{
	  unsigned int inner_regno = subreg_regno (x);
	  unsigned int inner_endregno
	    = inner_regno + (inner_regno < FIRST_PSEUDO_REGISTER
			     ? subreg_nregs (x) : 1);

	  return endregno > inner_regno && regno < inner_endregno;
	}
      break;

    case CLOBBER:
    case SET:
      if (&SET_DEST (x) != loc
	  /* Note setting a SUBREG counts as referring to the REG it is in for
	     a pseudo but not for hard registers since we can
	     treat each word individually.  */
	  && ((GET_CODE (SET_DEST (x)) == SUBREG
	       && loc != &SUBREG_REG (SET_DEST (x))
	       && REG_P (SUBREG_REG (SET_DEST (x)))
	       && REGNO (SUBREG_REG (SET_DEST (x))) >= FIRST_PSEUDO_REGISTER
	       && refers_to_regno_p (regno, endregno,
				     SUBREG_REG (SET_DEST (x)), loc))
	      || (!REG_P (SET_DEST (x))
		  && refers_to_regno_p (regno, endregno, SET_DEST (x), loc))))
	return true;

      if (code == CLOBBER || loc == &SET_SRC (x))
	return false;
      x = SET_SRC (x);
      goto repeat;

    default:
      break;
    }

  /* X does not match, so try its subexpressions.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && loc != &XEXP (x, i))
	{
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }
	  else
	    if (refers_to_regno_p (regno, endregno, XEXP (x, i), loc))
	      return true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (loc != &XVECEXP (x, i, j)
		&& refers_to_regno_p (regno, endregno, XVECEXP (x, i, j), loc))
	      return true;
	}
    }
  return false;
}

/* Nonzero if modifying X will affect IN.  If X is a register or a SUBREG,
   we check if any register number in X conflicts with the relevant register
   numbers.  If X is a constant, return 0.  If X is a MEM, return 1 iff IN
   contains a MEM (we don't bother checking for memory addresses that can't
   conflict because we expect this to be a rare case.  */

int
reg_overlap_mentioned_p (const_rtx x, const_rtx in)
{
  unsigned int regno, endregno;

  /* If either argument is a constant, then modifying X can not
     affect IN.  Here we look at IN, we can profitably combine
     CONSTANT_P (x) with the switch statement below.  */
  if (CONSTANT_P (in))
    return 0;

 recurse:
  switch (GET_CODE (x))
    {
    case STRICT_LOW_PART:
    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      /* Overly conservative.  */
      x = XEXP (x, 0);
      goto recurse;

    case SUBREG:
      regno = REGNO (SUBREG_REG (x));
      if (regno < FIRST_PSEUDO_REGISTER)
	regno = subreg_regno (x);
      endregno = regno + (regno < FIRST_PSEUDO_REGISTER
			  ? subreg_nregs (x) : 1);
      goto do_reg;

    case REG:
      regno = REGNO (x);
      endregno = END_REGNO (x);
    do_reg:
      return refers_to_regno_p (regno, endregno, in, (rtx*) 0);

    case MEM:
      {
	const char *fmt;
	int i;

	if (MEM_P (in))
	  return 1;

	fmt = GET_RTX_FORMAT (GET_CODE (in));
	for (i = GET_RTX_LENGTH (GET_CODE (in)) - 1; i >= 0; i--)
	  if (fmt[i] == 'e')
	    {
	      if (reg_overlap_mentioned_p (x, XEXP (in, i)))
		return 1;
	    }
	  else if (fmt[i] == 'E')
	    {
	      int j;
	      for (j = XVECLEN (in, i) - 1; j >= 0; --j)
		if (reg_overlap_mentioned_p (x, XVECEXP (in, i, j)))
		  return 1;
	    }

	return 0;
      }

    case SCRATCH:
    case PC:
    case CC0:
      return reg_mentioned_p (x, in);

    case PARALLEL:
      {
	int i;

	/* If any register in here refers to it we return true.  */
	for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	  if (XEXP (XVECEXP (x, 0, i), 0) != 0
	      && reg_overlap_mentioned_p (XEXP (XVECEXP (x, 0, i), 0), in))
	    return 1;
	return 0;
      }

    default:
      gcc_assert (CONSTANT_P (x));
      return 0;
    }
}

/* Call FUN on each register or MEM that is stored into or clobbered by X.
   (X would be the pattern of an insn).  DATA is an arbitrary pointer,
   ignored by note_stores, but passed to FUN.

   FUN receives three arguments:
   1. the REG, MEM, CC0 or PC being stored in or clobbered,
   2. the SET or CLOBBER rtx that does the store,
   3. the pointer DATA provided to note_stores.

  If the item being stored in or clobbered is a SUBREG of a hard register,
  the SUBREG will be passed.  */

void
note_stores (const_rtx x, void (*fun) (rtx, const_rtx, void *), void *data)
{
  int i;

  if (GET_CODE (x) == COND_EXEC)
    x = COND_EXEC_CODE (x);

  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      rtx dest = SET_DEST (x);

      while ((GET_CODE (dest) == SUBREG
	      && (!REG_P (SUBREG_REG (dest))
		  || REGNO (SUBREG_REG (dest)) >= FIRST_PSEUDO_REGISTER))
	     || GET_CODE (dest) == ZERO_EXTRACT
	     || GET_CODE (dest) == STRICT_LOW_PART)
	dest = XEXP (dest, 0);

      /* If we have a PARALLEL, SET_DEST is a list of EXPR_LIST expressions,
	 each of whose first operand is a register.  */
      if (GET_CODE (dest) == PARALLEL)
	{
	  for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	    if (XEXP (XVECEXP (dest, 0, i), 0) != 0)
	      (*fun) (XEXP (XVECEXP (dest, 0, i), 0), x, data);
	}
      else
	(*fun) (dest, x, data);
    }

  else if (GET_CODE (x) == PARALLEL)
    for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
      note_stores (XVECEXP (x, 0, i), fun, data);
}

/* Like notes_stores, but call FUN for each expression that is being
   referenced in PBODY, a pointer to the PATTERN of an insn.  We only call
   FUN for each expression, not any interior subexpressions.  FUN receives a
   pointer to the expression and the DATA passed to this function.

   Note that this is not quite the same test as that done in reg_referenced_p
   since that considers something as being referenced if it is being
   partially set, while we do not.  */

void
note_uses (rtx *pbody, void (*fun) (rtx *, void *), void *data)
{
  rtx body = *pbody;
  int i;

  switch (GET_CODE (body))
    {
    case COND_EXEC:
      (*fun) (&COND_EXEC_TEST (body), data);
      note_uses (&COND_EXEC_CODE (body), fun, data);
      return;

    case PARALLEL:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	note_uses (&XVECEXP (body, 0, i), fun, data);
      return;

    case SEQUENCE:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	note_uses (&PATTERN (XVECEXP (body, 0, i)), fun, data);
      return;

    case USE:
      (*fun) (&XEXP (body, 0), data);
      return;

    case ASM_OPERANDS:
      for (i = ASM_OPERANDS_INPUT_LENGTH (body) - 1; i >= 0; i--)
	(*fun) (&ASM_OPERANDS_INPUT (body, i), data);
      return;

    case TRAP_IF:
      (*fun) (&TRAP_CONDITION (body), data);
      return;

    case PREFETCH:
      (*fun) (&XEXP (body, 0), data);
      return;

    case UNSPEC:
    case UNSPEC_VOLATILE:
      for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
	(*fun) (&XVECEXP (body, 0, i), data);
      return;

    case CLOBBER:
      if (MEM_P (XEXP (body, 0)))
	(*fun) (&XEXP (XEXP (body, 0), 0), data);
      return;

    case SET:
      {
	rtx dest = SET_DEST (body);

	/* For sets we replace everything in source plus registers in memory
	   expression in store and operands of a ZERO_EXTRACT.  */
	(*fun) (&SET_SRC (body), data);

	if (GET_CODE (dest) == ZERO_EXTRACT)
	  {
	    (*fun) (&XEXP (dest, 1), data);
	    (*fun) (&XEXP (dest, 2), data);
	  }

	while (GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART)
	  dest = XEXP (dest, 0);

	if (MEM_P (dest))
	  (*fun) (&XEXP (dest, 0), data);
      }
      return;

    default:
      /* All the other possibilities never store.  */
      (*fun) (pbody, data);
      return;
    }
}

/* Return nonzero if X's old contents don't survive after INSN.
   This will be true if X is (cc0) or if X is a register and
   X dies in INSN or because INSN entirely sets X.

   "Entirely set" means set directly and not through a SUBREG, or
   ZERO_EXTRACT, so no trace of the old contents remains.
   Likewise, REG_INC does not count.

   REG may be a hard or pseudo reg.  Renumbering is not taken into account,
   but for this use that makes no difference, since regs don't overlap
   during their lifetimes.  Therefore, this function may be used
   at any time after deaths have been computed.

   If REG is a hard reg that occupies multiple machine registers, this
   function will only return 1 if each of those registers will be replaced
   by INSN.  */

int
dead_or_set_p (const_rtx insn, const_rtx x)
{
  unsigned int regno, end_regno;
  unsigned int i;

  /* Can't use cc0_rtx below since this file is used by genattrtab.c.  */
  if (GET_CODE (x) == CC0)
    return 1;

  gcc_assert (REG_P (x));

  regno = REGNO (x);
  end_regno = END_REGNO (x);
  for (i = regno; i < end_regno; i++)
    if (! dead_or_set_regno_p (insn, i))
      return 0;

  return 1;
}

/* Return TRUE iff DEST is a register or subreg of a register and
   doesn't change the number of words of the inner register, and any
   part of the register is TEST_REGNO.  */

static bool
covers_regno_no_parallel_p (const_rtx dest, unsigned int test_regno)
{
  unsigned int regno, endregno;

  if (GET_CODE (dest) == SUBREG
      && (((GET_MODE_SIZE (GET_MODE (dest))
	    + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
	  == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))
	       + UNITS_PER_WORD - 1) / UNITS_PER_WORD)))
    dest = SUBREG_REG (dest);

  if (!REG_P (dest))
    return false;

  regno = REGNO (dest);
  endregno = END_REGNO (dest);
  return (test_regno >= regno && test_regno < endregno);
}

/* Like covers_regno_no_parallel_p, but also handles PARALLELs where
   any member matches the covers_regno_no_parallel_p criteria.  */

static bool
covers_regno_p (const_rtx dest, unsigned int test_regno)
{
  if (GET_CODE (dest) == PARALLEL)
    {
      /* Some targets place small structures in registers for return
	 values of functions, and those registers are wrapped in
	 PARALLELs that we may see as the destination of a SET.  */
      int i;

      for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	{
	  rtx inner = XEXP (XVECEXP (dest, 0, i), 0);
	  if (inner != NULL_RTX
	      && covers_regno_no_parallel_p (inner, test_regno))
	    return true;
	}

      return false;
    }
  else
    return covers_regno_no_parallel_p (dest, test_regno);
}

/* Utility function for dead_or_set_p to check an individual register. */

int
dead_or_set_regno_p (const_rtx insn, unsigned int test_regno)
{
  const_rtx pattern;

  /* See if there is a death note for something that includes TEST_REGNO.  */
  if (find_regno_note (insn, REG_DEAD, test_regno))
    return 1;

  if (CALL_P (insn)
      && find_regno_fusage (insn, CLOBBER, test_regno))
    return 1;

  pattern = PATTERN (insn);

  /* If a COND_EXEC is not executed, the value survives.  */
  if (GET_CODE (pattern) == COND_EXEC)
    return 0;

  if (GET_CODE (pattern) == SET)
    return covers_regno_p (SET_DEST (pattern), test_regno);
  else if (GET_CODE (pattern) == PARALLEL)
    {
      int i;

      for (i = XVECLEN (pattern, 0) - 1; i >= 0; i--)
	{
	  rtx body = XVECEXP (pattern, 0, i);

	  if (GET_CODE (body) == COND_EXEC)
	    body = COND_EXEC_CODE (body);

	  if ((GET_CODE (body) == SET || GET_CODE (body) == CLOBBER)
	      && covers_regno_p (SET_DEST (body), test_regno))
	    return 1;
	}
    }

  return 0;
}

/* Return the reg-note of kind KIND in insn INSN, if there is one.
   If DATUM is nonzero, look for one whose datum is DATUM.  */

rtx
find_reg_note (const_rtx insn, enum reg_note kind, const_rtx datum)
{
  rtx link;

  gcc_checking_assert (insn);

  /* Ignore anything that is not an INSN, JUMP_INSN or CALL_INSN.  */
  if (! INSN_P (insn))
    return 0;
  if (datum == 0)
    {
      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	if (REG_NOTE_KIND (link) == kind)
	  return link;
      return 0;
    }

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == kind && datum == XEXP (link, 0))
      return link;
  return 0;
}

/* Return the reg-note of kind KIND in insn INSN which applies to register
   number REGNO, if any.  Return 0 if there is no such reg-note.  Note that
   the REGNO of this NOTE need not be REGNO if REGNO is a hard register;
   it might be the case that the note overlaps REGNO.  */

rtx
find_regno_note (const_rtx insn, enum reg_note kind, unsigned int regno)
{
  rtx link;

  /* Ignore anything that is not an INSN, JUMP_INSN or CALL_INSN.  */
  if (! INSN_P (insn))
    return 0;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == kind
	/* Verify that it is a register, so that scratch and MEM won't cause a
	   problem here.  */
	&& REG_P (XEXP (link, 0))
	&& REGNO (XEXP (link, 0)) <= regno
	&& END_REGNO (XEXP (link, 0)) > regno)
      return link;
  return 0;
}

/* Return a REG_EQUIV or REG_EQUAL note if insn has only a single set and
   has such a note.  */

rtx
find_reg_equal_equiv_note (const_rtx insn)
{
  rtx link;

  if (!INSN_P (insn))
    return 0;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_EQUAL
	|| REG_NOTE_KIND (link) == REG_EQUIV)
      {
	/* FIXME: We should never have REG_EQUAL/REG_EQUIV notes on
	   insns that have multiple sets.  Checking single_set to
	   make sure of this is not the proper check, as explained
	   in the comment in set_unique_reg_note.

	   This should be changed into an assert.  */
	if (GET_CODE (PATTERN (insn)) == PARALLEL && multiple_sets (insn))
	  return 0;
	return link;
      }
  return NULL;
}

/* Check whether INSN is a single_set whose source is known to be
   equivalent to a constant.  Return that constant if so, otherwise
   return null.  */

rtx
find_constant_src (const rtx_insn *insn)
{
  rtx note, set, x;

  set = single_set (insn);
  if (set)
    {
      x = avoid_constant_pool_reference (SET_SRC (set));
      if (CONSTANT_P (x))
	return x;
    }

  note = find_reg_equal_equiv_note (insn);
  if (note && CONSTANT_P (XEXP (note, 0)))
    return XEXP (note, 0);

  return NULL_RTX;
}

/* Return true if DATUM, or any overlap of DATUM, of kind CODE is found
   in the CALL_INSN_FUNCTION_USAGE information of INSN.  */

int
find_reg_fusage (const_rtx insn, enum rtx_code code, const_rtx datum)
{
  /* If it's not a CALL_INSN, it can't possibly have a
     CALL_INSN_FUNCTION_USAGE field, so don't bother checking.  */
  if (!CALL_P (insn))
    return 0;

  gcc_assert (datum);

  if (!REG_P (datum))
    {
      rtx link;

      for (link = CALL_INSN_FUNCTION_USAGE (insn);
	   link;
	   link = XEXP (link, 1))
	if (GET_CODE (XEXP (link, 0)) == code
	    && rtx_equal_p (datum, XEXP (XEXP (link, 0), 0)))
	  return 1;
    }
  else
    {
      unsigned int regno = REGNO (datum);

      /* CALL_INSN_FUNCTION_USAGE information cannot contain references
	 to pseudo registers, so don't bother checking.  */

      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  unsigned int end_regno = END_REGNO (datum);
	  unsigned int i;

	  for (i = regno; i < end_regno; i++)
	    if (find_regno_fusage (insn, code, i))
	      return 1;
	}
    }

  return 0;
}

/* Return true if REGNO, or any overlap of REGNO, of kind CODE is found
   in the CALL_INSN_FUNCTION_USAGE information of INSN.  */

int
find_regno_fusage (const_rtx insn, enum rtx_code code, unsigned int regno)
{
  rtx link;

  /* CALL_INSN_FUNCTION_USAGE information cannot contain references
     to pseudo registers, so don't bother checking.  */

  if (regno >= FIRST_PSEUDO_REGISTER
      || !CALL_P (insn) )
    return 0;

  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    {
      rtx op, reg;

      if (GET_CODE (op = XEXP (link, 0)) == code
	  && REG_P (reg = XEXP (op, 0))
	  && REGNO (reg) <= regno
	  && END_REGNO (reg) > regno)
	return 1;
    }

  return 0;
}


/* Return true if KIND is an integer REG_NOTE.  */

static bool
int_reg_note_p (enum reg_note kind)
{
  return kind == REG_BR_PROB;
}

/* Allocate a register note with kind KIND and datum DATUM.  LIST is
   stored as the pointer to the next register note.  */

rtx
alloc_reg_note (enum reg_note kind, rtx datum, rtx list)
{
  rtx note;

  gcc_checking_assert (!int_reg_note_p (kind));
  switch (kind)
    {
    case REG_CC_SETTER:
    case REG_CC_USER:
    case REG_LABEL_TARGET:
    case REG_LABEL_OPERAND:
    case REG_TM:
      /* These types of register notes use an INSN_LIST rather than an
	 EXPR_LIST, so that copying is done right and dumps look
	 better.  */
      note = alloc_INSN_LIST (datum, list);
      PUT_REG_NOTE_KIND (note, kind);
      break;

    default:
      note = alloc_EXPR_LIST (kind, datum, list);
      break;
    }

  return note;
}

/* Add register note with kind KIND and datum DATUM to INSN.  */

void
add_reg_note (rtx insn, enum reg_note kind, rtx datum)
{
  REG_NOTES (insn) = alloc_reg_note (kind, datum, REG_NOTES (insn));
}

/* Add an integer register note with kind KIND and datum DATUM to INSN.  */

void
add_int_reg_note (rtx insn, enum reg_note kind, int datum)
{
  gcc_checking_assert (int_reg_note_p (kind));
  REG_NOTES (insn) = gen_rtx_INT_LIST ((machine_mode) kind,
				       datum, REG_NOTES (insn));
}

/* Add a register note like NOTE to INSN.  */

void
add_shallow_copy_of_reg_note (rtx_insn *insn, rtx note)
{
  if (GET_CODE (note) == INT_LIST)
    add_int_reg_note (insn, REG_NOTE_KIND (note), XINT (note, 0));
  else
    add_reg_note (insn, REG_NOTE_KIND (note), XEXP (note, 0));
}

/* Remove register note NOTE from the REG_NOTES of INSN.  */

void
remove_note (rtx insn, const_rtx note)
{
  rtx link;

  if (note == NULL_RTX)
    return;

  if (REG_NOTES (insn) == note)
    REG_NOTES (insn) = XEXP (note, 1);
  else
    for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
      if (XEXP (link, 1) == note)
	{
	  XEXP (link, 1) = XEXP (note, 1);
	  break;
	}

  switch (REG_NOTE_KIND (note))
    {
    case REG_EQUAL:
    case REG_EQUIV:
      df_notes_rescan (as_a <rtx_insn *> (insn));
      break;
    default:
      break;
    }
}

/* Remove REG_EQUAL and/or REG_EQUIV notes if INSN has such notes.
   Return true if any note has been removed.  */

bool
remove_reg_equal_equiv_notes (rtx_insn *insn)
{
  rtx *loc;
  bool ret = false;

  loc = &REG_NOTES (insn);
  while (*loc)
    {
      enum reg_note kind = REG_NOTE_KIND (*loc);
      if (kind == REG_EQUAL || kind == REG_EQUIV)
	{
	  *loc = XEXP (*loc, 1);
	  ret = true;
	}
      else
	loc = &XEXP (*loc, 1);
    }
  return ret;
}

/* Remove all REG_EQUAL and REG_EQUIV notes referring to REGNO.  */

void
remove_reg_equal_equiv_notes_for_regno (unsigned int regno)
{
  df_ref eq_use;

  if (!df)
    return;

  /* This loop is a little tricky.  We cannot just go down the chain because
     it is being modified by some actions in the loop.  So we just iterate
     over the head.  We plan to drain the list anyway.  */
  while ((eq_use = DF_REG_EQ_USE_CHAIN (regno)) != NULL)
    {
      rtx_insn *insn = DF_REF_INSN (eq_use);
      rtx note = find_reg_equal_equiv_note (insn);

      /* This assert is generally triggered when someone deletes a REG_EQUAL
	 or REG_EQUIV note by hacking the list manually rather than calling
	 remove_note.  */
      gcc_assert (note);

      remove_note (insn, note);
    }
}

/* Search LISTP (an EXPR_LIST) for an entry whose first operand is NODE and
   return 1 if it is found.  A simple equality test is used to determine if
   NODE matches.  */

bool
in_insn_list_p (const rtx_insn_list *listp, const rtx_insn *node)
{
  const_rtx x;

  for (x = listp; x; x = XEXP (x, 1))
    if (node == XEXP (x, 0))
      return true;

  return false;
}

/* Search LISTP (an EXPR_LIST) for an entry whose first operand is NODE and
   remove that entry from the list if it is found.

   A simple equality test is used to determine if NODE matches.  */

void
remove_node_from_expr_list (const_rtx node, rtx_expr_list **listp)
{
  rtx_expr_list *temp = *listp;
  rtx_expr_list *prev = NULL;

  while (temp)
    {
      if (node == temp->element ())
	{
	  /* Splice the node out of the list.  */
	  if (prev)
	    XEXP (prev, 1) = temp->next ();
	  else
	    *listp = temp->next ();

	  return;
	}

      prev = temp;
      temp = temp->next ();
    }
}

/* Search LISTP (an INSN_LIST) for an entry whose first operand is NODE and
   remove that entry from the list if it is found.

   A simple equality test is used to determine if NODE matches.  */

void
remove_node_from_insn_list (const rtx_insn *node, rtx_insn_list **listp)
{
  rtx_insn_list *temp = *listp;
  rtx_insn_list *prev = NULL;

  while (temp)
    {
      if (node == temp->insn ())
	{
	  /* Splice the node out of the list.  */
	  if (prev)
	    XEXP (prev, 1) = temp->next ();
	  else
	    *listp = temp->next ();

	  return;
	}

      prev = temp;
      temp = temp->next ();
    }
}

/* Nonzero if X contains any volatile instructions.  These are instructions
   which may cause unpredictable machine state instructions, and thus no
   instructions or register uses should be moved or combined across them.
   This includes only volatile asms and UNSPEC_VOLATILE instructions.  */

int
volatile_insn_p (const_rtx x)
{
  const RTX_CODE code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    CASE_CONST_ANY:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case CLOBBER:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case CALL:
    case MEM:
      return 0;

    case UNSPEC_VOLATILE:
      return 1;

    case ASM_INPUT:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *const fmt = GET_RTX_FORMAT (code);
    int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_insn_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (volatile_insn_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Nonzero if X contains any volatile memory references
   UNSPEC_VOLATILE operations or volatile ASM_OPERANDS expressions.  */

int
volatile_refs_p (const_rtx x)
{
  const RTX_CODE code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    CASE_CONST_ANY:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case CLOBBER:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case UNSPEC_VOLATILE:
      return 1;

    case MEM:
    case ASM_INPUT:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *const fmt = GET_RTX_FORMAT (code);
    int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_refs_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (volatile_refs_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Similar to above, except that it also rejects register pre- and post-
   incrementing.  */

int
side_effects_p (const_rtx x)
{
  const RTX_CODE code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    CASE_CONST_ANY:
    case CC0:
    case PC:
    case REG:
    case SCRATCH:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case VAR_LOCATION:
      return 0;

    case CLOBBER:
      /* Reject CLOBBER with a non-VOID mode.  These are made by combine.c
	 when some combination can't be done.  If we see one, don't think
	 that we can simplify the expression.  */
      return (GET_MODE (x) != VOIDmode);

    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
    case CALL:
    case UNSPEC_VOLATILE:
      return 1;

    case MEM:
    case ASM_INPUT:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *fmt = GET_RTX_FORMAT (code);
    int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (side_effects_p (XEXP (x, i)))
	      return 1;
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (side_effects_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Return nonzero if evaluating rtx X might cause a trap.
   FLAGS controls how to consider MEMs.  A nonzero means the context
   of the access may have changed from the original, such that the
   address may have become invalid.  */

int
may_trap_p_1 (const_rtx x, unsigned flags)
{
  int i;
  enum rtx_code code;
  const char *fmt;

  /* We make no distinction currently, but this function is part of
     the internal target-hooks ABI so we keep the parameter as
     "unsigned flags".  */
  bool code_changed = flags != 0;

  if (x == 0)
    return 0;
  code = GET_CODE (x);
  switch (code)
    {
      /* Handle these cases quickly.  */
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case PC:
    case CC0:
    case REG:
    case SCRATCH:
      return 0;

    case UNSPEC:
      return targetm.unspec_may_trap_p (x, flags);

    case UNSPEC_VOLATILE:
    case ASM_INPUT:
    case TRAP_IF:
      return 1;

    case ASM_OPERANDS:
      return MEM_VOLATILE_P (x);

      /* Memory ref can trap unless it's a static var or a stack slot.  */
    case MEM:
      /* Recognize specific pattern of stack checking probes.  */
      if (flag_stack_check
	  && MEM_VOLATILE_P (x)
	  && XEXP (x, 0) == stack_pointer_rtx)
	return 1;
      if (/* MEM_NOTRAP_P only relates to the actual position of the memory
	     reference; moving it out of context such as when moving code
	     when optimizing, might cause its address to become invalid.  */
	  code_changed
	  || !MEM_NOTRAP_P (x))
	{
	  HOST_WIDE_INT size = MEM_SIZE_KNOWN_P (x) ? MEM_SIZE (x) : 0;
	  return rtx_addr_can_trap_p_1 (XEXP (x, 0), 0, size,
					GET_MODE (x), code_changed);
	}

      return 0;

      /* Division by a non-constant might trap.  */
    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      if (HONOR_SNANS (x))
	return 1;
      if (SCALAR_FLOAT_MODE_P (GET_MODE (x)))
	return flag_trapping_math;
      if (!CONSTANT_P (XEXP (x, 1)) || (XEXP (x, 1) == const0_rtx))
	return 1;
      break;

    case EXPR_LIST:
      /* An EXPR_LIST is used to represent a function call.  This
	 certainly may trap.  */
      return 1;

    case GE:
    case GT:
    case LE:
    case LT:
    case LTGT:
    case COMPARE:
      /* Some floating point comparisons may trap.  */
      if (!flag_trapping_math)
	break;
      /* ??? There is no machine independent way to check for tests that trap
	 when COMPARE is used, though many targets do make this distinction.
	 For instance, sparc uses CCFPE for compares which generate exceptions
	 and CCFP for compares which do not generate exceptions.  */
      if (HONOR_NANS (x))
	return 1;
      /* But often the compare has some CC mode, so check operand
	 modes as well.  */
      if (HONOR_NANS (XEXP (x, 0))
	  || HONOR_NANS (XEXP (x, 1)))
	return 1;
      break;

    case EQ:
    case NE:
      if (HONOR_SNANS (x))
	return 1;
      /* Often comparison is CC mode, so check operand modes.  */
      if (HONOR_SNANS (XEXP (x, 0))
	  || HONOR_SNANS (XEXP (x, 1)))
	return 1;
      break;

    case FIX:
      /* Conversion of floating point might trap.  */
      if (flag_trapping_math && HONOR_NANS (XEXP (x, 0)))
	return 1;
      break;

    case NEG:
    case ABS:
    case SUBREG:
      /* These operations don't trap even with floating point.  */
      break;

    default:
      /* Any floating arithmetic may trap.  */
      if (SCALAR_FLOAT_MODE_P (GET_MODE (x)) && flag_trapping_math)
	return 1;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (may_trap_p_1 (XEXP (x, i), flags))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (may_trap_p_1 (XVECEXP (x, i, j), flags))
	      return 1;
	}
    }
  return 0;
}

/* Return nonzero if evaluating rtx X might cause a trap.  */

int
may_trap_p (const_rtx x)
{
  return may_trap_p_1 (x, 0);
}

/* Same as above, but additionally return nonzero if evaluating rtx X might
   cause a fault.  We define a fault for the purpose of this function as a
   erroneous execution condition that cannot be encountered during the normal
   execution of a valid program; the typical example is an unaligned memory
   access on a strict alignment machine.  The compiler guarantees that it
   doesn't generate code that will fault from a valid program, but this
   guarantee doesn't mean anything for individual instructions.  Consider
   the following example:

      struct S { int d; union { char *cp; int *ip; }; };

      int foo(struct S *s)
      {
	if (s->d == 1)
	  return *s->ip;
	else
	  return *s->cp;
      }

   on a strict alignment machine.  In a valid program, foo will never be
   invoked on a structure for which d is equal to 1 and the underlying
   unique field of the union not aligned on a 4-byte boundary, but the
   expression *s->ip might cause a fault if considered individually.

   At the RTL level, potentially problematic expressions will almost always
   verify may_trap_p; for example, the above dereference can be emitted as
   (mem:SI (reg:P)) and this expression is may_trap_p for a generic register.
   However, suppose that foo is inlined in a caller that causes s->cp to
   point to a local character variable and guarantees that s->d is not set
   to 1; foo may have been effectively translated into pseudo-RTL as:

      if ((reg:SI) == 1)
	(set (reg:SI) (mem:SI (%fp - 7)))
      else
	(set (reg:QI) (mem:QI (%fp - 7)))

   Now (mem:SI (%fp - 7)) is considered as not may_trap_p since it is a
   memory reference to a stack slot, but it will certainly cause a fault
   on a strict alignment machine.  */

int
may_trap_or_fault_p (const_rtx x)
{
  return may_trap_p_1 (x, 1);
}

/* Return nonzero if X contains a comparison that is not either EQ or NE,
   i.e., an inequality.  */

int
inequality_comparisons_p (const_rtx x)
{
  const char *fmt;
  int len, i;
  const enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case SCRATCH:
    case PC:
    case CC0:
    CASE_CONST_ANY:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      return 0;

    case LT:
    case LTU:
    case GT:
    case GTU:
    case LE:
    case LEU:
    case GE:
    case GEU:
      return 1;

    default:
      break;
    }

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e')
	{
	  if (inequality_comparisons_p (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (inequality_comparisons_p (XVECEXP (x, i, j)))
	      return 1;
	}
    }

  return 0;
}

/* Replace any occurrence of FROM in X with TO.  The function does
   not enter into CONST_DOUBLE for the replace.

   Note that copying is not done so X must not be shared unless all copies
   are to be modified.

   ALL_REGS is true if we want to replace all REGs equal to FROM, not just
   those pointer-equal ones.  */

rtx
replace_rtx (rtx x, rtx from, rtx to, bool all_regs)
{
  int i, j;
  const char *fmt;

  if (x == from)
    return to;

  /* Allow this function to make replacements in EXPR_LISTs.  */
  if (x == 0)
    return 0;

  if (all_regs
      && REG_P (x)
      && REG_P (from)
      && REGNO (x) == REGNO (from))
    {
      gcc_assert (GET_MODE (x) == GET_MODE (from));
      return to;
    }
  else if (GET_CODE (x) == SUBREG)
    {
      rtx new_rtx = replace_rtx (SUBREG_REG (x), from, to, all_regs);

      if (CONST_INT_P (new_rtx))
	{
	  x = simplify_subreg (GET_MODE (x), new_rtx,
			       GET_MODE (SUBREG_REG (x)),
			       SUBREG_BYTE (x));
	  gcc_assert (x);
	}
      else
	SUBREG_REG (x) = new_rtx;

      return x;
    }
  else if (GET_CODE (x) == ZERO_EXTEND)
    {
      rtx new_rtx = replace_rtx (XEXP (x, 0), from, to, all_regs);

      if (CONST_INT_P (new_rtx))
	{
	  x = simplify_unary_operation (ZERO_EXTEND, GET_MODE (x),
					new_rtx, GET_MODE (XEXP (x, 0)));
	  gcc_assert (x);
	}
      else
	XEXP (x, 0) = new_rtx;

      return x;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = replace_rtx (XEXP (x, i), from, to, all_regs);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  XVECEXP (x, i, j) = replace_rtx (XVECEXP (x, i, j),
					   from, to, all_regs);
    }

  return x;
}

/* Replace occurrences of the OLD_LABEL in *LOC with NEW_LABEL.  Also track
   the change in LABEL_NUSES if UPDATE_LABEL_NUSES.  */

void
replace_label (rtx *loc, rtx old_label, rtx new_label, bool update_label_nuses)
{
  /* Handle jump tables specially, since ADDR_{DIFF_,}VECs can be long.  */
  rtx x = *loc;
  if (JUMP_TABLE_DATA_P (x))
    {
      x = PATTERN (x);
      rtvec vec = XVEC (x, GET_CODE (x) == ADDR_DIFF_VEC);
      int len = GET_NUM_ELEM (vec);
      for (int i = 0; i < len; ++i)
	{
	  rtx ref = RTVEC_ELT (vec, i);
	  if (XEXP (ref, 0) == old_label)
	    {
	      XEXP (ref, 0) = new_label;
	      if (update_label_nuses)
		{
		  ++LABEL_NUSES (new_label);
		  --LABEL_NUSES (old_label);
		}
	    }
	}
      return;
    }

  /* If this is a JUMP_INSN, then we also need to fix the JUMP_LABEL
     field.  This is not handled by the iterator because it doesn't
     handle unprinted ('0') fields.  */
  if (JUMP_P (x) && JUMP_LABEL (x) == old_label)
    JUMP_LABEL (x) = new_label;

  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, loc, ALL)
    {
      rtx *loc = *iter;
      if (rtx x = *loc)
	{
	  if (GET_CODE (x) == SYMBOL_REF
	      && CONSTANT_POOL_ADDRESS_P (x))
	    {
	      rtx c = get_pool_constant (x);
	      if (rtx_referenced_p (old_label, c))
		{
		  /* Create a copy of constant C; replace the label inside
		     but do not update LABEL_NUSES because uses in constant pool
		     are not counted.  */
		  rtx new_c = copy_rtx (c);
		  replace_label (&new_c, old_label, new_label, false);

		  /* Add the new constant NEW_C to constant pool and replace
		     the old reference to constant by new reference.  */
		  rtx new_mem = force_const_mem (get_pool_mode (x), new_c);
		  *loc = replace_rtx (x, x, XEXP (new_mem, 0));
		}
	    }

	  if ((GET_CODE (x) == LABEL_REF
	       || GET_CODE (x) == INSN_LIST)
	      && XEXP (x, 0) == old_label)
	    {
	      XEXP (x, 0) = new_label;
	      if (update_label_nuses)
		{
		  ++LABEL_NUSES (new_label);
		  --LABEL_NUSES (old_label);
		}
	    }
	}
    }
}

void
replace_label_in_insn (rtx_insn *insn, rtx old_label, rtx new_label,
		       bool update_label_nuses)
{
  rtx insn_as_rtx = insn;
  replace_label (&insn_as_rtx, old_label, new_label, update_label_nuses);
  gcc_checking_assert (insn_as_rtx == insn);
}

/* Return true if X is referenced in BODY.  */

bool
rtx_referenced_p (const_rtx x, const_rtx body)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, body, ALL)
    if (const_rtx y = *iter)
      {
	/* Check if a label_ref Y refers to label X.  */
	if (GET_CODE (y) == LABEL_REF
	    && LABEL_P (x)
	    && LABEL_REF_LABEL (y) == x)
	  return true;

	if (rtx_equal_p (x, y))
	  return true;

	/* If Y is a reference to pool constant traverse the constant.  */
	if (GET_CODE (y) == SYMBOL_REF
	    && CONSTANT_POOL_ADDRESS_P (y))
	  iter.substitute (get_pool_constant (y));
      }
  return false;
}

/* If INSN is a tablejump return true and store the label (before jump table) to
   *LABELP and the jump table to *TABLEP.  LABELP and TABLEP may be NULL.  */

bool
tablejump_p (const rtx_insn *insn, rtx *labelp, rtx_jump_table_data **tablep)
{
  rtx label;
  rtx_insn *table;

  if (!JUMP_P (insn))
    return false;

  label = JUMP_LABEL (insn);
  if (label != NULL_RTX && !ANY_RETURN_P (label)
      && (table = NEXT_INSN (as_a <rtx_insn *> (label))) != NULL_RTX
      && JUMP_TABLE_DATA_P (table))
    {
      if (labelp)
	*labelp = label;
      if (tablep)
	*tablep = as_a <rtx_jump_table_data *> (table);
      return true;
    }
  return false;
}

/* A subroutine of computed_jump_p, return 1 if X contains a REG or MEM or
   constant that is not in the constant pool and not in the condition
   of an IF_THEN_ELSE.  */

static int
computed_jump_p_1 (const_rtx x)
{
  const enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  switch (code)
    {
    case LABEL_REF:
    case PC:
      return 0;

    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case REG:
      return 1;

    case MEM:
      return ! (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
		&& CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)));

    case IF_THEN_ELSE:
      return (computed_jump_p_1 (XEXP (x, 1))
	      || computed_jump_p_1 (XEXP (x, 2)));

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e'
	  && computed_jump_p_1 (XEXP (x, i)))
	return 1;

      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (computed_jump_p_1 (XVECEXP (x, i, j)))
	    return 1;
    }

  return 0;
}

/* Return nonzero if INSN is an indirect jump (aka computed jump).

   Tablejumps and casesi insns are not considered indirect jumps;
   we can recognize them by a (use (label_ref)).  */

int
computed_jump_p (const rtx_insn *insn)
{
  int i;
  if (JUMP_P (insn))
    {
      rtx pat = PATTERN (insn);

      /* If we have a JUMP_LABEL set, we're not a computed jump.  */
      if (JUMP_LABEL (insn) != NULL)
	return 0;

      if (GET_CODE (pat) == PARALLEL)
	{
	  int len = XVECLEN (pat, 0);
	  int has_use_labelref = 0;

	  for (i = len - 1; i >= 0; i--)
	    if (GET_CODE (XVECEXP (pat, 0, i)) == USE
		&& (GET_CODE (XEXP (XVECEXP (pat, 0, i), 0))
		    == LABEL_REF))
	      {
	        has_use_labelref = 1;
	        break;
	      }

	  if (! has_use_labelref)
	    for (i = len - 1; i >= 0; i--)
	      if (GET_CODE (XVECEXP (pat, 0, i)) == SET
		  && SET_DEST (XVECEXP (pat, 0, i)) == pc_rtx
		  && computed_jump_p_1 (SET_SRC (XVECEXP (pat, 0, i))))
		return 1;
	}
      else if (GET_CODE (pat) == SET
	       && SET_DEST (pat) == pc_rtx
	       && computed_jump_p_1 (SET_SRC (pat)))
	return 1;
    }
  return 0;
}



/* MEM has a PRE/POST-INC/DEC/MODIFY address X.  Extract the operands of
   the equivalent add insn and pass the result to FN, using DATA as the
   final argument.  */

static int
for_each_inc_dec_find_inc_dec (rtx mem, for_each_inc_dec_fn fn, void *data)
{
  rtx x = XEXP (mem, 0);
  switch (GET_CODE (x))
    {
    case PRE_INC:
    case POST_INC:
      {
	int size = GET_MODE_SIZE (GET_MODE (mem));
	rtx r1 = XEXP (x, 0);
	rtx c = gen_int_mode (size, GET_MODE (r1));
	return fn (mem, x, r1, r1, c, data);
      }

    case PRE_DEC:
    case POST_DEC:
      {
	int size = GET_MODE_SIZE (GET_MODE (mem));
	rtx r1 = XEXP (x, 0);
	rtx c = gen_int_mode (-size, GET_MODE (r1));
	return fn (mem, x, r1, r1, c, data);
      }

    case PRE_MODIFY:
    case POST_MODIFY:
      {
	rtx r1 = XEXP (x, 0);
	rtx add = XEXP (x, 1);
	return fn (mem, x, r1, add, NULL, data);
      }

    default:
      gcc_unreachable ();
    }
}

/* Traverse *LOC looking for MEMs that have autoinc addresses.
   For each such autoinc operation found, call FN, passing it
   the innermost enclosing MEM, the operation itself, the RTX modified
   by the operation, two RTXs (the second may be NULL) that, once
   added, represent the value to be held by the modified RTX
   afterwards, and DATA.  FN is to return 0 to continue the
   traversal or any other value to have it returned to the caller of
   for_each_inc_dec.  */

int
for_each_inc_dec (rtx x,
		  for_each_inc_dec_fn fn,
		  void *data)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, x, NONCONST)
    {
      rtx mem = *iter;
      if (mem
	  && MEM_P (mem)
	  && GET_RTX_CLASS (GET_CODE (XEXP (mem, 0))) == RTX_AUTOINC)
	{
	  int res = for_each_inc_dec_find_inc_dec (mem, fn, data);
	  if (res != 0)
	    return res;
	  iter.skip_subrtxes ();
	}
    }
  return 0;
}


/* Searches X for any reference to REGNO, returning the rtx of the
   reference found if any.  Otherwise, returns NULL_RTX.  */

rtx
regno_use_in (unsigned int regno, rtx x)
{
  const char *fmt;
  int i, j;
  rtx tem;

  if (REG_P (x) && REGNO (x) == regno)
    return x;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if ((tem = regno_use_in (regno, XEXP (x, i))))
	    return tem;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if ((tem = regno_use_in (regno , XVECEXP (x, i, j))))
	    return tem;
    }

  return NULL_RTX;
}

/* Return a value indicating whether OP, an operand of a commutative
   operation, is preferred as the first or second operand.  The more
   positive the value, the stronger the preference for being the first
   operand.  */

int
commutative_operand_precedence (rtx op)
{
  enum rtx_code code = GET_CODE (op);

  /* Constants always become the second operand.  Prefer "nice" constants.  */
  if (code == CONST_INT)
    return -8;
  if (code == CONST_WIDE_INT)
    return -7;
  if (code == CONST_DOUBLE)
    return -7;
  if (code == CONST_FIXED)
    return -7;
  op = avoid_constant_pool_reference (op);
  code = GET_CODE (op);

  switch (GET_RTX_CLASS (code))
    {
    case RTX_CONST_OBJ:
      if (code == CONST_INT)
        return -6;
      if (code == CONST_WIDE_INT)
        return -6;
      if (code == CONST_DOUBLE)
        return -5;
      if (code == CONST_FIXED)
        return -5;
      return -4;

    case RTX_EXTRA:
      /* SUBREGs of objects should come second.  */
      if (code == SUBREG && OBJECT_P (SUBREG_REG (op)))
        return -3;
      return 0;

    case RTX_OBJ:
      /* Complex expressions should be the first, so decrease priority
         of objects.  Prefer pointer objects over non pointer objects.  */
      if ((REG_P (op) && REG_POINTER (op))
	  || (MEM_P (op) && MEM_POINTER (op)))
	return -1;
      return -2;

    case RTX_COMM_ARITH:
      /* Prefer operands that are themselves commutative to be first.
         This helps to make things linear.  In particular,
         (and (and (reg) (reg)) (not (reg))) is canonical.  */
      return 4;

    case RTX_BIN_ARITH:
      /* If only one operand is a binary expression, it will be the first
         operand.  In particular,  (plus (minus (reg) (reg)) (neg (reg)))
         is canonical, although it will usually be further simplified.  */
      return 2;

    case RTX_UNARY:
      /* Then prefer NEG and NOT.  */
      if (code == NEG || code == NOT)
        return 1;

    default:
      return 0;
    }
}

/* Return 1 iff it is necessary to swap operands of commutative operation
   in order to canonicalize expression.  */

bool
swap_commutative_operands_p (rtx x, rtx y)
{
  return (commutative_operand_precedence (x)
	  < commutative_operand_precedence (y));
}

/* Return 1 if X is an autoincrement side effect and the register is
   not the stack pointer.  */
int
auto_inc_p (const_rtx x)
{
  switch (GET_CODE (x))
    {
    case PRE_INC:
    case POST_INC:
    case PRE_DEC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      /* There are no REG_INC notes for SP.  */
      if (XEXP (x, 0) != stack_pointer_rtx)
	return 1;
    default:
      break;
    }
  return 0;
}

/* Return nonzero if IN contains a piece of rtl that has the address LOC.  */
int
loc_mentioned_in_p (rtx *loc, const_rtx in)
{
  enum rtx_code code;
  const char *fmt;
  int i, j;

  if (!in)
    return 0;

  code = GET_CODE (in);
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (loc == &XEXP (in, i) || loc_mentioned_in_p (loc, XEXP (in, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	  if (loc == &XVECEXP (in, i, j)
	      || loc_mentioned_in_p (loc, XVECEXP (in, i, j)))
	    return 1;
    }
  return 0;
}

/* Helper function for subreg_lsb.  Given a subreg's OUTER_MODE, INNER_MODE,
   and SUBREG_BYTE, return the bit offset where the subreg begins
   (counting from the least significant bit of the operand).  */

unsigned int
subreg_lsb_1 (machine_mode outer_mode,
	      machine_mode inner_mode,
	      unsigned int subreg_byte)
{
  unsigned int bitpos;
  unsigned int byte;
  unsigned int word;

  /* A paradoxical subreg begins at bit position 0.  */
  if (GET_MODE_PRECISION (outer_mode) > GET_MODE_PRECISION (inner_mode))
    return 0;

  if (WORDS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
    /* If the subreg crosses a word boundary ensure that
       it also begins and ends on a word boundary.  */
    gcc_assert (!((subreg_byte % UNITS_PER_WORD
		  + GET_MODE_SIZE (outer_mode)) > UNITS_PER_WORD
		  && (subreg_byte % UNITS_PER_WORD
		      || GET_MODE_SIZE (outer_mode) % UNITS_PER_WORD)));

  if (WORDS_BIG_ENDIAN)
    word = (GET_MODE_SIZE (inner_mode)
	    - (subreg_byte + GET_MODE_SIZE (outer_mode))) / UNITS_PER_WORD;
  else
    word = subreg_byte / UNITS_PER_WORD;
  bitpos = word * BITS_PER_WORD;

  if (BYTES_BIG_ENDIAN)
    byte = (GET_MODE_SIZE (inner_mode)
	    - (subreg_byte + GET_MODE_SIZE (outer_mode))) % UNITS_PER_WORD;
  else
    byte = subreg_byte % UNITS_PER_WORD;
  bitpos += byte * BITS_PER_UNIT;

  return bitpos;
}

/* Given a subreg X, return the bit offset where the subreg begins
   (counting from the least significant bit of the reg).  */

unsigned int
subreg_lsb (const_rtx x)
{
  return subreg_lsb_1 (GET_MODE (x), GET_MODE (SUBREG_REG (x)),
		       SUBREG_BYTE (x));
}

/* Fill in information about a subreg of a hard register.
   xregno - A regno of an inner hard subreg_reg (or what will become one).
   xmode  - The mode of xregno.
   offset - The byte offset.
   ymode  - The mode of a top level SUBREG (or what may become one).
   info   - Pointer to structure to fill in.

   Rather than considering one particular inner register (and thus one
   particular "outer" register) in isolation, this function really uses
   XREGNO as a model for a sequence of isomorphic hard registers.  Thus the
   function does not check whether adding INFO->offset to XREGNO gives
   a valid hard register; even if INFO->offset + XREGNO is out of range,
   there might be another register of the same type that is in range.
   Likewise it doesn't check whether HARD_REGNO_MODE_OK accepts the new
   register, since that can depend on things like whether the final
   register number is even or odd.  Callers that want to check whether
   this particular subreg can be replaced by a simple (reg ...) should
   use simplify_subreg_regno.  */

void
subreg_get_info (unsigned int xregno, machine_mode xmode,
		 unsigned int offset, machine_mode ymode,
		 struct subreg_info *info)
{
  int nregs_xmode, nregs_ymode;
  int mode_multiple, nregs_multiple;
  int offset_adj, y_offset, y_offset_adj;
  int regsize_xmode, regsize_ymode;
  bool rknown;

  gcc_assert (xregno < FIRST_PSEUDO_REGISTER);

  rknown = false;

  /* If there are holes in a non-scalar mode in registers, we expect
     that it is made up of its units concatenated together.  */
  if (HARD_REGNO_NREGS_HAS_PADDING (xregno, xmode))
    {
      machine_mode xmode_unit;

      nregs_xmode = HARD_REGNO_NREGS_WITH_PADDING (xregno, xmode);
      xmode_unit = GET_MODE_INNER (xmode);
      gcc_assert (HARD_REGNO_NREGS_HAS_PADDING (xregno, xmode_unit));
      gcc_assert (nregs_xmode
		  == (GET_MODE_NUNITS (xmode)
		      * HARD_REGNO_NREGS_WITH_PADDING (xregno, xmode_unit)));
      gcc_assert (hard_regno_nregs[xregno][xmode]
		  == (hard_regno_nregs[xregno][xmode_unit]
		      * GET_MODE_NUNITS (xmode)));

      /* You can only ask for a SUBREG of a value with holes in the middle
	 if you don't cross the holes.  (Such a SUBREG should be done by
	 picking a different register class, or doing it in memory if
	 necessary.)  An example of a value with holes is XCmode on 32-bit
	 x86 with -m128bit-long-double; it's represented in 6 32-bit registers,
	 3 for each part, but in memory it's two 128-bit parts.
	 Padding is assumed to be at the end (not necessarily the 'high part')
	 of each unit.  */
      if ((offset / GET_MODE_SIZE (xmode_unit) + 1
	   < GET_MODE_NUNITS (xmode))
	  && (offset / GET_MODE_SIZE (xmode_unit)
	      != ((offset + GET_MODE_SIZE (ymode) - 1)
		  / GET_MODE_SIZE (xmode_unit))))
	{
	  info->representable_p = false;
	  rknown = true;
	}
    }
  else
    nregs_xmode = hard_regno_nregs[xregno][xmode];

  nregs_ymode = hard_regno_nregs[xregno][ymode];

  /* Paradoxical subregs are otherwise valid.  */
  if (!rknown
      && offset == 0
      && GET_MODE_PRECISION (ymode) > GET_MODE_PRECISION (xmode))
    {
      info->representable_p = true;
      /* If this is a big endian paradoxical subreg, which uses more
	 actual hard registers than the original register, we must
	 return a negative offset so that we find the proper highpart
	 of the register.  */
      if (GET_MODE_SIZE (ymode) > UNITS_PER_WORD
	  ? REG_WORDS_BIG_ENDIAN : BYTES_BIG_ENDIAN)
	info->offset = nregs_xmode - nregs_ymode;
      else
	info->offset = 0;
      info->nregs = nregs_ymode;
      return;
    }

  /* If registers store different numbers of bits in the different
     modes, we cannot generally form this subreg.  */
  if (!HARD_REGNO_NREGS_HAS_PADDING (xregno, xmode)
      && !HARD_REGNO_NREGS_HAS_PADDING (xregno, ymode)
      && (GET_MODE_SIZE (xmode) % nregs_xmode) == 0
      && (GET_MODE_SIZE (ymode) % nregs_ymode) == 0)
    {
      regsize_xmode = GET_MODE_SIZE (xmode) / nregs_xmode;
      regsize_ymode = GET_MODE_SIZE (ymode) / nregs_ymode;
      if (!rknown && regsize_xmode > regsize_ymode && nregs_ymode > 1)
	{
	  info->representable_p = false;
	  info->nregs
	    = (GET_MODE_SIZE (ymode) + regsize_xmode - 1) / regsize_xmode;
	  info->offset = offset / regsize_xmode;
	  return;
	}
      if (!rknown && regsize_ymode > regsize_xmode && nregs_xmode > 1)
	{
	  info->representable_p = false;
	  info->nregs
	    = (GET_MODE_SIZE (ymode) + regsize_xmode - 1) / regsize_xmode;
	  info->offset = offset / regsize_xmode;
	  return;
	}
      /* Quick exit for the simple and common case of extracting whole
	 subregisters from a multiregister value.  */
      /* ??? It would be better to integrate this into the code below,
	 if we can generalize the concept enough and figure out how
	 odd-sized modes can coexist with the other weird cases we support.  */
      if (!rknown
	  && WORDS_BIG_ENDIAN == REG_WORDS_BIG_ENDIAN
	  && regsize_xmode == regsize_ymode
	  && (offset % regsize_ymode) == 0)
	{
	  info->representable_p = true;
	  info->nregs = nregs_ymode;
	  info->offset = offset / regsize_ymode;
	  gcc_assert (info->offset + info->nregs <= nregs_xmode);
	  return;
	}
    }

  /* Lowpart subregs are otherwise valid.  */
  if (!rknown && offset == subreg_lowpart_offset (ymode, xmode))
    {
      info->representable_p = true;
      rknown = true;

      if (offset == 0 || nregs_xmode == nregs_ymode)
	{
	  info->offset = 0;
	  info->nregs = nregs_ymode;
	  return;
	}
    }

  /* This should always pass, otherwise we don't know how to verify
     the constraint.  These conditions may be relaxed but
     subreg_regno_offset would need to be redesigned.  */
  gcc_assert ((GET_MODE_SIZE (xmode) % GET_MODE_SIZE (ymode)) == 0);
  gcc_assert ((nregs_xmode % nregs_ymode) == 0);

  if (WORDS_BIG_ENDIAN != REG_WORDS_BIG_ENDIAN
      && GET_MODE_SIZE (xmode) > UNITS_PER_WORD)
    {
      HOST_WIDE_INT xsize = GET_MODE_SIZE (xmode);
      HOST_WIDE_INT ysize = GET_MODE_SIZE (ymode);
      HOST_WIDE_INT off_low = offset & (ysize - 1);
      HOST_WIDE_INT off_high = offset & ~(ysize - 1);
      offset = (xsize - ysize - off_high) | off_low;
    }
  /* The XMODE value can be seen as a vector of NREGS_XMODE
     values.  The subreg must represent a lowpart of given field.
     Compute what field it is.  */
  offset_adj = offset;
  offset_adj -= subreg_lowpart_offset (ymode,
				       mode_for_size (GET_MODE_BITSIZE (xmode)
						      / nregs_xmode,
						      MODE_INT, 0));

  /* Size of ymode must not be greater than the size of xmode.  */
  mode_multiple = GET_MODE_SIZE (xmode) / GET_MODE_SIZE (ymode);
  gcc_assert (mode_multiple != 0);

  y_offset = offset / GET_MODE_SIZE (ymode);
  y_offset_adj = offset_adj / GET_MODE_SIZE (ymode);
  nregs_multiple = nregs_xmode / nregs_ymode;

  gcc_assert ((offset_adj % GET_MODE_SIZE (ymode)) == 0);
  gcc_assert ((mode_multiple % nregs_multiple) == 0);

  if (!rknown)
    {
      info->representable_p = (!(y_offset_adj % (mode_multiple / nregs_multiple)));
      rknown = true;
    }
  info->offset = (y_offset / (mode_multiple / nregs_multiple)) * nregs_ymode;
  info->nregs = nregs_ymode;
}

/* This function returns the regno offset of a subreg expression.
   xregno - A regno of an inner hard subreg_reg (or what will become one).
   xmode  - The mode of xregno.
   offset - The byte offset.
   ymode  - The mode of a top level SUBREG (or what may become one).
   RETURN - The regno offset which would be used.  */
unsigned int
subreg_regno_offset (unsigned int xregno, machine_mode xmode,
		     unsigned int offset, machine_mode ymode)
{
  struct subreg_info info;
  subreg_get_info (xregno, xmode, offset, ymode, &info);
  return info.offset;
}

/* This function returns true when the offset is representable via
   subreg_offset in the given regno.
   xregno - A regno of an inner hard subreg_reg (or what will become one).
   xmode  - The mode of xregno.
   offset - The byte offset.
   ymode  - The mode of a top level SUBREG (or what may become one).
   RETURN - Whether the offset is representable.  */
bool
subreg_offset_representable_p (unsigned int xregno, machine_mode xmode,
			       unsigned int offset, machine_mode ymode)
{
  struct subreg_info info;
  subreg_get_info (xregno, xmode, offset, ymode, &info);
  return info.representable_p;
}

/* Return the number of a YMODE register to which

       (subreg:YMODE (reg:XMODE XREGNO) OFFSET)

   can be simplified.  Return -1 if the subreg can't be simplified.

   XREGNO is a hard register number.  */

int
simplify_subreg_regno (unsigned int xregno, machine_mode xmode,
		       unsigned int offset, machine_mode ymode)
{
  struct subreg_info info;
  unsigned int yregno;

#ifdef CANNOT_CHANGE_MODE_CLASS
  /* Give the backend a chance to disallow the mode change.  */
  if (GET_MODE_CLASS (xmode) != MODE_COMPLEX_INT
      && GET_MODE_CLASS (xmode) != MODE_COMPLEX_FLOAT
      && REG_CANNOT_CHANGE_MODE_P (xregno, xmode, ymode)
      /* We can use mode change in LRA for some transformations.  */
      && ! lra_in_progress)
    return -1;
#endif

  /* We shouldn't simplify stack-related registers.  */
  if ((!reload_completed || frame_pointer_needed)
      && xregno == FRAME_POINTER_REGNUM)
    return -1;

  if (FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      && xregno == ARG_POINTER_REGNUM)
    return -1;

  if (xregno == STACK_POINTER_REGNUM
      /* We should convert hard stack register in LRA if it is
	 possible.  */
      && ! lra_in_progress)
    return -1;

  /* Try to get the register offset.  */
  subreg_get_info (xregno, xmode, offset, ymode, &info);
  if (!info.representable_p)
    return -1;

  /* Make sure that the offsetted register value is in range.  */
  yregno = xregno + info.offset;
  if (!HARD_REGISTER_NUM_P (yregno))
    return -1;

  /* See whether (reg:YMODE YREGNO) is valid.

     ??? We allow invalid registers if (reg:XMODE XREGNO) is also invalid.
     This is a kludge to work around how complex FP arguments are passed
     on IA-64 and should be fixed.  See PR target/49226.  */
  if (!HARD_REGNO_MODE_OK (yregno, ymode)
      && HARD_REGNO_MODE_OK (xregno, xmode))
    return -1;

  return (int) yregno;
}

/* Return the final regno that a subreg expression refers to.  */
unsigned int
subreg_regno (const_rtx x)
{
  unsigned int ret;
  rtx subreg = SUBREG_REG (x);
  int regno = REGNO (subreg);

  ret = regno + subreg_regno_offset (regno,
				     GET_MODE (subreg),
				     SUBREG_BYTE (x),
				     GET_MODE (x));
  return ret;

}

/* Return the number of registers that a subreg expression refers
   to.  */
unsigned int
subreg_nregs (const_rtx x)
{
  return subreg_nregs_with_regno (REGNO (SUBREG_REG (x)), x);
}

/* Return the number of registers that a subreg REG with REGNO
   expression refers to.  This is a copy of the rtlanal.c:subreg_nregs
   changed so that the regno can be passed in. */

unsigned int
subreg_nregs_with_regno (unsigned int regno, const_rtx x)
{
  struct subreg_info info;
  rtx subreg = SUBREG_REG (x);

  subreg_get_info (regno, GET_MODE (subreg), SUBREG_BYTE (x), GET_MODE (x),
		   &info);
  return info.nregs;
}


struct parms_set_data
{
  int nregs;
  HARD_REG_SET regs;
};

/* Helper function for noticing stores to parameter registers.  */
static void
parms_set (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  struct parms_set_data *const d = (struct parms_set_data *) data;
  if (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER
      && TEST_HARD_REG_BIT (d->regs, REGNO (x)))
    {
      CLEAR_HARD_REG_BIT (d->regs, REGNO (x));
      d->nregs--;
    }
}

/* Look backward for first parameter to be loaded.
   Note that loads of all parameters will not necessarily be
   found if CSE has eliminated some of them (e.g., an argument
   to the outer function is passed down as a parameter).
   Do not skip BOUNDARY.  */
rtx_insn *
find_first_parameter_load (rtx_insn *call_insn, rtx_insn *boundary)
{
  struct parms_set_data parm;
  rtx p;
  rtx_insn *before, *first_set;

  /* Since different machines initialize their parameter registers
     in different orders, assume nothing.  Collect the set of all
     parameter registers.  */
  CLEAR_HARD_REG_SET (parm.regs);
  parm.nregs = 0;
  for (p = CALL_INSN_FUNCTION_USAGE (call_insn); p; p = XEXP (p, 1))
    if (GET_CODE (XEXP (p, 0)) == USE
	&& REG_P (XEXP (XEXP (p, 0), 0)))
      {
	gcc_assert (REGNO (XEXP (XEXP (p, 0), 0)) < FIRST_PSEUDO_REGISTER);

	/* We only care about registers which can hold function
	   arguments.  */
	if (!FUNCTION_ARG_REGNO_P (REGNO (XEXP (XEXP (p, 0), 0))))
	  continue;

	SET_HARD_REG_BIT (parm.regs, REGNO (XEXP (XEXP (p, 0), 0)));
	parm.nregs++;
      }
  before = call_insn;
  first_set = call_insn;

  /* Search backward for the first set of a register in this set.  */
  while (parm.nregs && before != boundary)
    {
      before = PREV_INSN (before);

      /* It is possible that some loads got CSEed from one call to
         another.  Stop in that case.  */
      if (CALL_P (before))
	break;

      /* Our caller needs either ensure that we will find all sets
         (in case code has not been optimized yet), or take care
         for possible labels in a way by setting boundary to preceding
         CODE_LABEL.  */
      if (LABEL_P (before))
	{
	  gcc_assert (before == boundary);
	  break;
	}

      if (INSN_P (before))
	{
	  int nregs_old = parm.nregs;
	  note_stores (PATTERN (before), parms_set, &parm);
	  /* If we found something that did not set a parameter reg,
	     we're done.  Do not keep going, as that might result
	     in hoisting an insn before the setting of a pseudo
	     that is used by the hoisted insn. */
	  if (nregs_old != parm.nregs)
	    first_set = before;
	  else
	    break;
	}
    }
  return first_set;
}

/* Return true if we should avoid inserting code between INSN and preceding
   call instruction.  */

bool
keep_with_call_p (const rtx_insn *insn)
{
  rtx set;

  if (INSN_P (insn) && (set = single_set (insn)) != NULL)
    {
      if (REG_P (SET_DEST (set))
	  && REGNO (SET_DEST (set)) < FIRST_PSEUDO_REGISTER
	  && fixed_regs[REGNO (SET_DEST (set))]
	  && general_operand (SET_SRC (set), VOIDmode))
	return true;
      if (REG_P (SET_SRC (set))
	  && targetm.calls.function_value_regno_p (REGNO (SET_SRC (set)))
	  && REG_P (SET_DEST (set))
	  && REGNO (SET_DEST (set)) >= FIRST_PSEUDO_REGISTER)
	return true;
      /* There may be a stack pop just after the call and before the store
	 of the return register.  Search for the actual store when deciding
	 if we can break or not.  */
      if (SET_DEST (set) == stack_pointer_rtx)
	{
	  /* This CONST_CAST is okay because next_nonnote_insn just
	     returns its argument and we assign it to a const_rtx
	     variable.  */
	  const rtx_insn *i2
	    = next_nonnote_insn (const_cast<rtx_insn *> (insn));
	  if (i2 && keep_with_call_p (i2))
	    return true;
	}
    }
  return false;
}

/* Return true if LABEL is a target of JUMP_INSN.  This applies only
   to non-complex jumps.  That is, direct unconditional, conditional,
   and tablejumps, but not computed jumps or returns.  It also does
   not apply to the fallthru case of a conditional jump.  */

bool
label_is_jump_target_p (const_rtx label, const rtx_insn *jump_insn)
{
  rtx tmp = JUMP_LABEL (jump_insn);
  rtx_jump_table_data *table;

  if (label == tmp)
    return true;

  if (tablejump_p (jump_insn, NULL, &table))
    {
      rtvec vec = table->get_labels ();
      int i, veclen = GET_NUM_ELEM (vec);

      for (i = 0; i < veclen; ++i)
	if (XEXP (RTVEC_ELT (vec, i), 0) == label)
	  return true;
    }

  if (find_reg_note (jump_insn, REG_LABEL_TARGET, label))
    return true;

  return false;
}


/* Return an estimate of the cost of computing rtx X.
   One use is in cse, to decide which expression to keep in the hash table.
   Another is in rtl generation, to pick the cheapest way to multiply.
   Other uses like the latter are expected in the future.

   X appears as operand OPNO in an expression with code OUTER_CODE.
   SPEED specifies whether costs optimized for speed or size should
   be returned.  */

int
rtx_cost (rtx x, machine_mode mode, enum rtx_code outer_code,
	  int opno, bool speed)
{
  int i, j;
  enum rtx_code code;
  const char *fmt;
  int total;
  int factor;

  if (x == 0)
    return 0;

  if (GET_MODE (x) != VOIDmode)
    mode = GET_MODE (x);

  /* A size N times larger than UNITS_PER_WORD likely needs N times as
     many insns, taking N times as long.  */
  factor = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
  if (factor == 0)
    factor = 1;

  /* Compute the default costs of certain things.
     Note that targetm.rtx_costs can override the defaults.  */

  code = GET_CODE (x);
  switch (code)
    {
    case MULT:
      /* Multiplication has time-complexity O(N*N), where N is the
	 number of units (translated from digits) when using
	 schoolbook long multiplication.  */
      total = factor * factor * COSTS_N_INSNS (5);
      break;
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      /* Similarly, complexity for schoolbook long division.  */
      total = factor * factor * COSTS_N_INSNS (7);
      break;
    case USE:
      /* Used in combine.c as a marker.  */
      total = 0;
      break;
    case SET:
      /* A SET doesn't have a mode, so let's look at the SET_DEST to get
	 the mode for the factor.  */
      mode = GET_MODE (SET_DEST (x));
      factor = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
      if (factor == 0)
	factor = 1;
      /* Pass through.  */
    default:
      total = factor * COSTS_N_INSNS (1);
    }

  switch (code)
    {
    case REG:
      return 0;

    case SUBREG:
      total = 0;
      /* If we can't tie these modes, make this expensive.  The larger
	 the mode, the more expensive it is.  */
      if (! MODES_TIEABLE_P (mode, GET_MODE (SUBREG_REG (x))))
	return COSTS_N_INSNS (2 + factor);
      break;

    default:
      if (targetm.rtx_costs (x, mode, outer_code, opno, &total, speed))
	return total;
      break;
    }

  /* Sum the costs of the sub-rtx's, plus cost of this operation,
     which is already in total.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      total += rtx_cost (XEXP (x, i), mode, code, i, speed);
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	total += rtx_cost (XVECEXP (x, i, j), mode, code, i, speed);

  return total;
}

/* Fill in the structure C with information about both speed and size rtx
   costs for X, which is operand OPNO in an expression with code OUTER.  */

void
get_full_rtx_cost (rtx x, machine_mode mode, enum rtx_code outer, int opno,
		   struct full_rtx_costs *c)
{
  c->speed = rtx_cost (x, mode, outer, opno, true);
  c->size = rtx_cost (x, mode, outer, opno, false);
}


/* Return cost of address expression X.
   Expect that X is properly formed address reference.

   SPEED parameter specify whether costs optimized for speed or size should
   be returned.  */

int
address_cost (rtx x, machine_mode mode, addr_space_t as, bool speed)
{
  /* We may be asked for cost of various unusual addresses, such as operands
     of push instruction.  It is not worthwhile to complicate writing
     of the target hook by such cases.  */

  if (!memory_address_addr_space_p (mode, x, as))
    return 1000;

  return targetm.address_cost (x, mode, as, speed);
}

/* If the target doesn't override, compute the cost as with arithmetic.  */

int
default_address_cost (rtx x, machine_mode, addr_space_t, bool speed)
{
  return rtx_cost (x, Pmode, MEM, 0, speed);
}


unsigned HOST_WIDE_INT
nonzero_bits (const_rtx x, machine_mode mode)
{
  return cached_nonzero_bits (x, mode, NULL_RTX, VOIDmode, 0);
}

unsigned int
num_sign_bit_copies (const_rtx x, machine_mode mode)
{
  return cached_num_sign_bit_copies (x, mode, NULL_RTX, VOIDmode, 0);
}

/* Return true if nonzero_bits1 might recurse into both operands
   of X.  */

static inline bool
nonzero_bits_binary_arith_p (const_rtx x)
{
  if (!ARITHMETIC_P (x))
    return false;
  switch (GET_CODE (x))
    {
    case AND:
    case XOR:
    case IOR:
    case UMIN:
    case UMAX:
    case SMIN:
    case SMAX:
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      return true;
    default:
      return false;
    }
}

/* The function cached_nonzero_bits is a wrapper around nonzero_bits1.
   It avoids exponential behavior in nonzero_bits1 when X has
   identical subexpressions on the first or the second level.  */

static unsigned HOST_WIDE_INT
cached_nonzero_bits (const_rtx x, machine_mode mode, const_rtx known_x,
		     machine_mode known_mode,
		     unsigned HOST_WIDE_INT known_ret)
{
  if (x == known_x && mode == known_mode)
    return known_ret;

  /* Try to find identical subexpressions.  If found call
     nonzero_bits1 on X with the subexpressions as KNOWN_X and the
     precomputed value for the subexpression as KNOWN_RET.  */

  if (nonzero_bits_binary_arith_p (x))
    {
      rtx x0 = XEXP (x, 0);
      rtx x1 = XEXP (x, 1);

      /* Check the first level.  */
      if (x0 == x1)
	return nonzero_bits1 (x, mode, x0, mode,
			      cached_nonzero_bits (x0, mode, known_x,
						   known_mode, known_ret));

      /* Check the second level.  */
      if (nonzero_bits_binary_arith_p (x0)
	  && (x1 == XEXP (x0, 0) || x1 == XEXP (x0, 1)))
	return nonzero_bits1 (x, mode, x1, mode,
			      cached_nonzero_bits (x1, mode, known_x,
						   known_mode, known_ret));

      if (nonzero_bits_binary_arith_p (x1)
	  && (x0 == XEXP (x1, 0) || x0 == XEXP (x1, 1)))
	return nonzero_bits1 (x, mode, x0, mode,
			      cached_nonzero_bits (x0, mode, known_x,
						   known_mode, known_ret));
    }

  return nonzero_bits1 (x, mode, known_x, known_mode, known_ret);
}

/* We let num_sign_bit_copies recur into nonzero_bits as that is useful.
   We don't let nonzero_bits recur into num_sign_bit_copies, because that
   is less useful.  We can't allow both, because that results in exponential
   run time recursion.  There is a nullstone testcase that triggered
   this.  This macro avoids accidental uses of num_sign_bit_copies.  */
#define cached_num_sign_bit_copies sorry_i_am_preventing_exponential_behavior

/* Given an expression, X, compute which bits in X can be nonzero.
   We don't care about bits outside of those defined in MODE.

   For most X this is simply GET_MODE_MASK (GET_MODE (MODE)), but if X is
   an arithmetic operation, we can do better.  */

static unsigned HOST_WIDE_INT
nonzero_bits1 (const_rtx x, machine_mode mode, const_rtx known_x,
	       machine_mode known_mode,
	       unsigned HOST_WIDE_INT known_ret)
{
  unsigned HOST_WIDE_INT nonzero = GET_MODE_MASK (mode);
  unsigned HOST_WIDE_INT inner_nz;
  enum rtx_code code;
  machine_mode inner_mode;
  unsigned int mode_width = GET_MODE_PRECISION (mode);

  /* For floating-point and vector values, assume all bits are needed.  */
  if (FLOAT_MODE_P (GET_MODE (x)) || FLOAT_MODE_P (mode)
      || VECTOR_MODE_P (GET_MODE (x)) || VECTOR_MODE_P (mode))
    return nonzero;

  /* If X is wider than MODE, use its mode instead.  */
  if (GET_MODE_PRECISION (GET_MODE (x)) > mode_width)
    {
      mode = GET_MODE (x);
      nonzero = GET_MODE_MASK (mode);
      mode_width = GET_MODE_PRECISION (mode);
    }

  if (mode_width > HOST_BITS_PER_WIDE_INT)
    /* Our only callers in this case look for single bit values.  So
       just return the mode mask.  Those tests will then be false.  */
    return nonzero;

  /* If MODE is wider than X, but both are a single word for both the host
     and target machines, we can compute this from which bits of the
     object might be nonzero in its own mode, taking into account the fact
     that on many CISC machines, accessing an object in a wider mode
     causes the high-order bits to become undefined.  So they are
     not known to be zero.  */

  if (!WORD_REGISTER_OPERATIONS
      && GET_MODE (x) != VOIDmode
      && GET_MODE (x) != mode
      && GET_MODE_PRECISION (GET_MODE (x)) <= BITS_PER_WORD
      && GET_MODE_PRECISION (GET_MODE (x)) <= HOST_BITS_PER_WIDE_INT
      && GET_MODE_PRECISION (mode) > GET_MODE_PRECISION (GET_MODE (x)))
    {
      nonzero &= cached_nonzero_bits (x, GET_MODE (x),
				      known_x, known_mode, known_ret);
      nonzero |= GET_MODE_MASK (mode) & ~GET_MODE_MASK (GET_MODE (x));
      return nonzero;
    }

  /* Please keep nonzero_bits_binary_arith_p above in sync with
     the code in the switch below.  */
  code = GET_CODE (x);
  switch (code)
    {
    case REG:
#if defined(POINTERS_EXTEND_UNSIGNED)
      /* If pointers extend unsigned and this is a pointer in Pmode, say that
	 all the bits above ptr_mode are known to be zero.  */
      /* As we do not know which address space the pointer is referring to,
	 we can do this only if the target does not support different pointer
	 or address modes depending on the address space.  */
      if (target_default_pointer_address_modes_p ()
	  && POINTERS_EXTEND_UNSIGNED && GET_MODE (x) == Pmode
	  && REG_POINTER (x)
	  && !targetm.have_ptr_extend ())
	nonzero &= GET_MODE_MASK (ptr_mode);
#endif

      /* Include declared information about alignment of pointers.  */
      /* ??? We don't properly preserve REG_POINTER changes across
	 pointer-to-integer casts, so we can't trust it except for
	 things that we know must be pointers.  See execute/960116-1.c.  */
      if ((x == stack_pointer_rtx
	   || x == frame_pointer_rtx
	   || x == arg_pointer_rtx)
	  && REGNO_POINTER_ALIGN (REGNO (x)))
	{
	  unsigned HOST_WIDE_INT alignment
	    = REGNO_POINTER_ALIGN (REGNO (x)) / BITS_PER_UNIT;

#ifdef PUSH_ROUNDING
	  /* If PUSH_ROUNDING is defined, it is possible for the
	     stack to be momentarily aligned only to that amount,
	     so we pick the least alignment.  */
	  if (x == stack_pointer_rtx && PUSH_ARGS)
	    alignment = MIN ((unsigned HOST_WIDE_INT) PUSH_ROUNDING (1),
			     alignment);
#endif

	  nonzero &= ~(alignment - 1);
	}

      {
	unsigned HOST_WIDE_INT nonzero_for_hook = nonzero;
	rtx new_rtx = rtl_hooks.reg_nonzero_bits (x, mode, known_x,
					      known_mode, known_ret,
					      &nonzero_for_hook);

	if (new_rtx)
	  nonzero_for_hook &= cached_nonzero_bits (new_rtx, mode, known_x,
						   known_mode, known_ret);

	return nonzero_for_hook;
      }

    case CONST_INT:
      /* If X is negative in MODE, sign-extend the value.  */
      if (SHORT_IMMEDIATES_SIGN_EXTEND && INTVAL (x) > 0
	  && mode_width < BITS_PER_WORD
	  && (UINTVAL (x) & ((unsigned HOST_WIDE_INT) 1 << (mode_width - 1)))
	     != 0)
	return UINTVAL (x) | (HOST_WIDE_INT_M1U << mode_width);

      return UINTVAL (x);

    case MEM:
#ifdef LOAD_EXTEND_OP
      /* In many, if not most, RISC machines, reading a byte from memory
	 zeros the rest of the register.  Noticing that fact saves a lot
	 of extra zero-extends.  */
      if (LOAD_EXTEND_OP (GET_MODE (x)) == ZERO_EXTEND)
	nonzero &= GET_MODE_MASK (GET_MODE (x));
#endif
      break;

    case EQ:  case NE:
    case UNEQ:  case LTGT:
    case GT:  case GTU:  case UNGT:
    case LT:  case LTU:  case UNLT:
    case GE:  case GEU:  case UNGE:
    case LE:  case LEU:  case UNLE:
    case UNORDERED: case ORDERED:
      /* If this produces an integer result, we know which bits are set.
	 Code here used to clear bits outside the mode of X, but that is
	 now done above.  */
      /* Mind that MODE is the mode the caller wants to look at this
	 operation in, and not the actual operation mode.  We can wind
	 up with (subreg:DI (gt:V4HI x y)), and we don't have anything
	 that describes the results of a vector compare.  */
      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT
	  && mode_width <= HOST_BITS_PER_WIDE_INT)
	nonzero = STORE_FLAG_VALUE;
      break;

    case NEG:
#if 0
      /* Disabled to avoid exponential mutual recursion between nonzero_bits
	 and num_sign_bit_copies.  */
      if (num_sign_bit_copies (XEXP (x, 0), GET_MODE (x))
	  == GET_MODE_PRECISION (GET_MODE (x)))
	nonzero = 1;
#endif

      if (GET_MODE_PRECISION (GET_MODE (x)) < mode_width)
	nonzero |= (GET_MODE_MASK (mode) & ~GET_MODE_MASK (GET_MODE (x)));
      break;

    case ABS:
#if 0
      /* Disabled to avoid exponential mutual recursion between nonzero_bits
	 and num_sign_bit_copies.  */
      if (num_sign_bit_copies (XEXP (x, 0), GET_MODE (x))
	  == GET_MODE_PRECISION (GET_MODE (x)))
	nonzero = 1;
#endif
      break;

    case TRUNCATE:
      nonzero &= (cached_nonzero_bits (XEXP (x, 0), mode,
				       known_x, known_mode, known_ret)
		  & GET_MODE_MASK (mode));
      break;

    case ZERO_EXTEND:
      nonzero &= cached_nonzero_bits (XEXP (x, 0), mode,
				      known_x, known_mode, known_ret);
      if (GET_MODE (XEXP (x, 0)) != VOIDmode)
	nonzero &= GET_MODE_MASK (GET_MODE (XEXP (x, 0)));
      break;

    case SIGN_EXTEND:
      /* If the sign bit is known clear, this is the same as ZERO_EXTEND.
	 Otherwise, show all the bits in the outer mode but not the inner
	 may be nonzero.  */
      inner_nz = cached_nonzero_bits (XEXP (x, 0), mode,
				      known_x, known_mode, known_ret);
      if (GET_MODE (XEXP (x, 0)) != VOIDmode)
	{
	  inner_nz &= GET_MODE_MASK (GET_MODE (XEXP (x, 0)));
	  if (val_signbit_known_set_p (GET_MODE (XEXP (x, 0)), inner_nz))
	    inner_nz |= (GET_MODE_MASK (mode)
			 & ~GET_MODE_MASK (GET_MODE (XEXP (x, 0))));
	}

      nonzero &= inner_nz;
      break;

    case AND:
      nonzero &= cached_nonzero_bits (XEXP (x, 0), mode,
				       known_x, known_mode, known_ret)
      		 & cached_nonzero_bits (XEXP (x, 1), mode,
					known_x, known_mode, known_ret);
      break;

    case XOR:   case IOR:
    case UMIN:  case UMAX:  case SMIN:  case SMAX:
      {
	unsigned HOST_WIDE_INT nonzero0
	   = cached_nonzero_bits (XEXP (x, 0), mode,
				  known_x, known_mode, known_ret);

	/* Don't call nonzero_bits for the second time if it cannot change
	   anything.  */
	if ((nonzero & nonzero0) != nonzero)
	  nonzero &= nonzero0
      		     | cached_nonzero_bits (XEXP (x, 1), mode,
					    known_x, known_mode, known_ret);
      }
      break;

    case PLUS:  case MINUS:
    case MULT:
    case DIV:   case UDIV:
    case MOD:   case UMOD:
      /* We can apply the rules of arithmetic to compute the number of
	 high- and low-order zero bits of these operations.  We start by
	 computing the width (position of the highest-order nonzero bit)
	 and the number of low-order zero bits for each value.  */
      {
	unsigned HOST_WIDE_INT nz0
	  = cached_nonzero_bits (XEXP (x, 0), mode,
				 known_x, known_mode, known_ret);
	unsigned HOST_WIDE_INT nz1
	  = cached_nonzero_bits (XEXP (x, 1), mode,
				 known_x, known_mode, known_ret);
	int sign_index = GET_MODE_PRECISION (GET_MODE (x)) - 1;
	int width0 = floor_log2 (nz0) + 1;
	int width1 = floor_log2 (nz1) + 1;
	int low0 = floor_log2 (nz0 & -nz0);
	int low1 = floor_log2 (nz1 & -nz1);
	unsigned HOST_WIDE_INT op0_maybe_minusp
	  = nz0 & ((unsigned HOST_WIDE_INT) 1 << sign_index);
	unsigned HOST_WIDE_INT op1_maybe_minusp
	  = nz1 & ((unsigned HOST_WIDE_INT) 1 << sign_index);
	unsigned int result_width = mode_width;
	int result_low = 0;

	switch (code)
	  {
	  case PLUS:
	    result_width = MAX (width0, width1) + 1;
	    result_low = MIN (low0, low1);
	    break;
	  case MINUS:
	    result_low = MIN (low0, low1);
	    break;
	  case MULT:
	    result_width = width0 + width1;
	    result_low = low0 + low1;
	    break;
	  case DIV:
	    if (width1 == 0)
	      break;
	    if (!op0_maybe_minusp && !op1_maybe_minusp)
	      result_width = width0;
	    break;
	  case UDIV:
	    if (width1 == 0)
	      break;
	    result_width = width0;
	    break;
	  case MOD:
	    if (width1 == 0)
	      break;
	    if (!op0_maybe_minusp && !op1_maybe_minusp)
	      result_width = MIN (width0, width1);
	    result_low = MIN (low0, low1);
	    break;
	  case UMOD:
	    if (width1 == 0)
	      break;
	    result_width = MIN (width0, width1);
	    result_low = MIN (low0, low1);
	    break;
	  default:
	    gcc_unreachable ();
	  }

	if (result_width < mode_width)
	  nonzero &= ((unsigned HOST_WIDE_INT) 1 << result_width) - 1;

	if (result_low > 0)
	  nonzero &= ~(((unsigned HOST_WIDE_INT) 1 << result_low) - 1);
      }
      break;

    case ZERO_EXTRACT:
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT)
	nonzero &= ((unsigned HOST_WIDE_INT) 1 << INTVAL (XEXP (x, 1))) - 1;
      break;

    case SUBREG:
      /* If this is a SUBREG formed for a promoted variable that has
	 been zero-extended, we know that at least the high-order bits
	 are zero, though others might be too.  */

      if (SUBREG_PROMOTED_VAR_P (x) && SUBREG_PROMOTED_UNSIGNED_P (x))
	nonzero = GET_MODE_MASK (GET_MODE (x))
		  & cached_nonzero_bits (SUBREG_REG (x), GET_MODE (x),
					 known_x, known_mode, known_ret);

      inner_mode = GET_MODE (SUBREG_REG (x));
      /* If the inner mode is a single word for both the host and target
	 machines, we can compute this from which bits of the inner
	 object might be nonzero.  */
      if (GET_MODE_PRECISION (inner_mode) <= BITS_PER_WORD
	  && (GET_MODE_PRECISION (inner_mode) <= HOST_BITS_PER_WIDE_INT))
	{
	  nonzero &= cached_nonzero_bits (SUBREG_REG (x), mode,
					  known_x, known_mode, known_ret);

#if WORD_REGISTER_OPERATIONS && defined (LOAD_EXTEND_OP)
	  /* If this is a typical RISC machine, we only have to worry
	     about the way loads are extended.  */
	  if ((LOAD_EXTEND_OP (inner_mode) == SIGN_EXTEND
	       ? val_signbit_known_set_p (inner_mode, nonzero)
	       : LOAD_EXTEND_OP (inner_mode) != ZERO_EXTEND)
	      || !MEM_P (SUBREG_REG (x)))
#endif
	    {
	      /* On many CISC machines, accessing an object in a wider mode
		 causes the high-order bits to become undefined.  So they are
		 not known to be zero.  */
	      if (GET_MODE_PRECISION (GET_MODE (x))
		  > GET_MODE_PRECISION (inner_mode))
		nonzero |= (GET_MODE_MASK (GET_MODE (x))
			    & ~GET_MODE_MASK (inner_mode));
	    }
	}
      break;

    case ASHIFTRT:
    case LSHIFTRT:
    case ASHIFT:
    case ROTATE:
      /* The nonzero bits are in two classes: any bits within MODE
	 that aren't in GET_MODE (x) are always significant.  The rest of the
	 nonzero bits are those that are significant in the operand of
	 the shift when shifted the appropriate number of bits.  This
	 shows that high-order bits are cleared by the right shift and
	 low-order bits by left shifts.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT
	  && INTVAL (XEXP (x, 1)) < GET_MODE_PRECISION (GET_MODE (x)))
	{
	  machine_mode inner_mode = GET_MODE (x);
	  unsigned int width = GET_MODE_PRECISION (inner_mode);
	  int count = INTVAL (XEXP (x, 1));
	  unsigned HOST_WIDE_INT mode_mask = GET_MODE_MASK (inner_mode);
	  unsigned HOST_WIDE_INT op_nonzero
	    = cached_nonzero_bits (XEXP (x, 0), mode,
				   known_x, known_mode, known_ret);
	  unsigned HOST_WIDE_INT inner = op_nonzero & mode_mask;
	  unsigned HOST_WIDE_INT outer = 0;

	  if (mode_width > width)
	    outer = (op_nonzero & nonzero & ~mode_mask);

	  if (code == LSHIFTRT)
	    inner >>= count;
	  else if (code == ASHIFTRT)
	    {
	      inner >>= count;

	      /* If the sign bit may have been nonzero before the shift, we
		 need to mark all the places it could have been copied to
		 by the shift as possibly nonzero.  */
	      if (inner & ((unsigned HOST_WIDE_INT) 1 << (width - 1 - count)))
		inner |= (((unsigned HOST_WIDE_INT) 1 << count) - 1)
			   << (width - count);
	    }
	  else if (code == ASHIFT)
	    inner <<= count;
	  else
	    inner = ((inner << (count % width)
		      | (inner >> (width - (count % width)))) & mode_mask);

	  nonzero &= (outer | inner);
	}
      break;

    case FFS:
    case POPCOUNT:
      /* This is at most the number of bits in the mode.  */
      nonzero = ((unsigned HOST_WIDE_INT) 2 << (floor_log2 (mode_width))) - 1;
      break;

    case CLZ:
      /* If CLZ has a known value at zero, then the nonzero bits are
	 that value, plus the number of bits in the mode minus one.  */
      if (CLZ_DEFINED_VALUE_AT_ZERO (mode, nonzero))
	nonzero
	  |= ((unsigned HOST_WIDE_INT) 1 << (floor_log2 (mode_width))) - 1;
      else
	nonzero = -1;
      break;

    case CTZ:
      /* If CTZ has a known value at zero, then the nonzero bits are
	 that value, plus the number of bits in the mode minus one.  */
      if (CTZ_DEFINED_VALUE_AT_ZERO (mode, nonzero))
	nonzero
	  |= ((unsigned HOST_WIDE_INT) 1 << (floor_log2 (mode_width))) - 1;
      else
	nonzero = -1;
      break;

    case CLRSB:
      /* This is at most the number of bits in the mode minus 1.  */
      nonzero = ((unsigned HOST_WIDE_INT) 1 << (floor_log2 (mode_width))) - 1;
      break;

    case PARITY:
      nonzero = 1;
      break;

    case IF_THEN_ELSE:
      {
	unsigned HOST_WIDE_INT nonzero_true
	  = cached_nonzero_bits (XEXP (x, 1), mode,
				 known_x, known_mode, known_ret);

	/* Don't call nonzero_bits for the second time if it cannot change
	   anything.  */
	if ((nonzero & nonzero_true) != nonzero)
	  nonzero &= nonzero_true
      		     | cached_nonzero_bits (XEXP (x, 2), mode,
					    known_x, known_mode, known_ret);
      }
      break;

    default:
      break;
    }

  return nonzero;
}

/* See the macro definition above.  */
#undef cached_num_sign_bit_copies


/* Return true if num_sign_bit_copies1 might recurse into both operands
   of X.  */

static inline bool
num_sign_bit_copies_binary_arith_p (const_rtx x)
{
  if (!ARITHMETIC_P (x))
    return false;
  switch (GET_CODE (x))
    {
    case IOR:
    case AND:
    case XOR:
    case SMIN:
    case SMAX:
    case UMIN:
    case UMAX:
    case PLUS:
    case MINUS:
    case MULT:
      return true;
    default:
      return false;
    }
}

/* The function cached_num_sign_bit_copies is a wrapper around
   num_sign_bit_copies1.  It avoids exponential behavior in
   num_sign_bit_copies1 when X has identical subexpressions on the
   first or the second level.  */

static unsigned int
cached_num_sign_bit_copies (const_rtx x, machine_mode mode, const_rtx known_x,
			    machine_mode known_mode,
			    unsigned int known_ret)
{
  if (x == known_x && mode == known_mode)
    return known_ret;

  /* Try to find identical subexpressions.  If found call
     num_sign_bit_copies1 on X with the subexpressions as KNOWN_X and
     the precomputed value for the subexpression as KNOWN_RET.  */

  if (num_sign_bit_copies_binary_arith_p (x))
    {
      rtx x0 = XEXP (x, 0);
      rtx x1 = XEXP (x, 1);

      /* Check the first level.  */
      if (x0 == x1)
	return
	  num_sign_bit_copies1 (x, mode, x0, mode,
				cached_num_sign_bit_copies (x0, mode, known_x,
							    known_mode,
							    known_ret));

      /* Check the second level.  */
      if (num_sign_bit_copies_binary_arith_p (x0)
	  && (x1 == XEXP (x0, 0) || x1 == XEXP (x0, 1)))
	return
	  num_sign_bit_copies1 (x, mode, x1, mode,
				cached_num_sign_bit_copies (x1, mode, known_x,
							    known_mode,
							    known_ret));

      if (num_sign_bit_copies_binary_arith_p (x1)
	  && (x0 == XEXP (x1, 0) || x0 == XEXP (x1, 1)))
	return
	  num_sign_bit_copies1 (x, mode, x0, mode,
				cached_num_sign_bit_copies (x0, mode, known_x,
							    known_mode,
							    known_ret));
    }

  return num_sign_bit_copies1 (x, mode, known_x, known_mode, known_ret);
}

/* Return the number of bits at the high-order end of X that are known to
   be equal to the sign bit.  X will be used in mode MODE; if MODE is
   VOIDmode, X will be used in its own mode.  The returned value  will always
   be between 1 and the number of bits in MODE.  */

static unsigned int
num_sign_bit_copies1 (const_rtx x, machine_mode mode, const_rtx known_x,
		      machine_mode known_mode,
		      unsigned int known_ret)
{
  enum rtx_code code = GET_CODE (x);
  unsigned int bitwidth = GET_MODE_PRECISION (mode);
  int num0, num1, result;
  unsigned HOST_WIDE_INT nonzero;

  /* If we weren't given a mode, use the mode of X.  If the mode is still
     VOIDmode, we don't know anything.  Likewise if one of the modes is
     floating-point.  */

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  if (mode == VOIDmode || FLOAT_MODE_P (mode) || FLOAT_MODE_P (GET_MODE (x))
      || VECTOR_MODE_P (GET_MODE (x)) || VECTOR_MODE_P (mode))
    return 1;

  /* For a smaller object, just ignore the high bits.  */
  if (bitwidth < GET_MODE_PRECISION (GET_MODE (x)))
    {
      num0 = cached_num_sign_bit_copies (x, GET_MODE (x),
					 known_x, known_mode, known_ret);
      return MAX (1,
		  num0 - (int) (GET_MODE_PRECISION (GET_MODE (x)) - bitwidth));
    }

  if (GET_MODE (x) != VOIDmode && bitwidth > GET_MODE_PRECISION (GET_MODE (x)))
    {
      /* If this machine does not do all register operations on the entire
	 register and MODE is wider than the mode of X, we can say nothing
	 at all about the high-order bits.  */
      if (!WORD_REGISTER_OPERATIONS)
	return 1;

      /* Likewise on machines that do, if the mode of the object is smaller
	 than a word and loads of that size don't sign extend, we can say
	 nothing about the high order bits.  */
      if (GET_MODE_PRECISION (GET_MODE (x)) < BITS_PER_WORD
#ifdef LOAD_EXTEND_OP
	  && LOAD_EXTEND_OP (GET_MODE (x)) != SIGN_EXTEND
#endif
	  )
	return 1;
    }

  /* Please keep num_sign_bit_copies_binary_arith_p above in sync with
     the code in the switch below.  */
  switch (code)
    {
    case REG:

#if defined(POINTERS_EXTEND_UNSIGNED)
      /* If pointers extend signed and this is a pointer in Pmode, say that
	 all the bits above ptr_mode are known to be sign bit copies.  */
      /* As we do not know which address space the pointer is referring to,
	 we can do this only if the target does not support different pointer
	 or address modes depending on the address space.  */
      if (target_default_pointer_address_modes_p ()
	  && ! POINTERS_EXTEND_UNSIGNED && GET_MODE (x) == Pmode
	  && mode == Pmode && REG_POINTER (x)
	  && !targetm.have_ptr_extend ())
	return GET_MODE_PRECISION (Pmode) - GET_MODE_PRECISION (ptr_mode) + 1;
#endif

      {
	unsigned int copies_for_hook = 1, copies = 1;
	rtx new_rtx = rtl_hooks.reg_num_sign_bit_copies (x, mode, known_x,
						     known_mode, known_ret,
						     &copies_for_hook);

	if (new_rtx)
	  copies = cached_num_sign_bit_copies (new_rtx, mode, known_x,
					       known_mode, known_ret);

	if (copies > 1 || copies_for_hook > 1)
	  return MAX (copies, copies_for_hook);

	/* Else, use nonzero_bits to guess num_sign_bit_copies (see below).  */
      }
      break;

    case MEM:
#ifdef LOAD_EXTEND_OP
      /* Some RISC machines sign-extend all loads of smaller than a word.  */
      if (LOAD_EXTEND_OP (GET_MODE (x)) == SIGN_EXTEND)
	return MAX (1, ((int) bitwidth
			- (int) GET_MODE_PRECISION (GET_MODE (x)) + 1));
#endif
      break;

    case CONST_INT:
      /* If the constant is negative, take its 1's complement and remask.
	 Then see how many zero bits we have.  */
      nonzero = UINTVAL (x) & GET_MODE_MASK (mode);
      if (bitwidth <= HOST_BITS_PER_WIDE_INT
	  && (nonzero & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	nonzero = (~nonzero) & GET_MODE_MASK (mode);

      return (nonzero == 0 ? bitwidth : bitwidth - floor_log2 (nonzero) - 1);

    case SUBREG:
      /* If this is a SUBREG for a promoted object that is sign-extended
	 and we are looking at it in a wider mode, we know that at least the
	 high-order bits are known to be sign bit copies.  */

      if (SUBREG_PROMOTED_VAR_P (x) && SUBREG_PROMOTED_SIGNED_P (x))
	{
	  num0 = cached_num_sign_bit_copies (SUBREG_REG (x), mode,
					     known_x, known_mode, known_ret);
	  return MAX ((int) bitwidth
		      - (int) GET_MODE_PRECISION (GET_MODE (x)) + 1,
		      num0);
	}

      /* For a smaller object, just ignore the high bits.  */
      if (bitwidth <= GET_MODE_PRECISION (GET_MODE (SUBREG_REG (x))))
	{
	  num0 = cached_num_sign_bit_copies (SUBREG_REG (x), VOIDmode,
					     known_x, known_mode, known_ret);
	  return MAX (1, (num0
			  - (int) (GET_MODE_PRECISION (GET_MODE (SUBREG_REG (x)))
				   - bitwidth)));
	}

#ifdef LOAD_EXTEND_OP
      /* For paradoxical SUBREGs on machines where all register operations
	 affect the entire register, just look inside.  Note that we are
	 passing MODE to the recursive call, so the number of sign bit copies
	 will remain relative to that mode, not the inner mode.  */

      /* This works only if loads sign extend.  Otherwise, if we get a
	 reload for the inner part, it may be loaded from the stack, and
	 then we lose all sign bit copies that existed before the store
	 to the stack.  */

      if (WORD_REGISTER_OPERATIONS
	  && paradoxical_subreg_p (x)
	  && LOAD_EXTEND_OP (GET_MODE (SUBREG_REG (x))) == SIGN_EXTEND
	  && MEM_P (SUBREG_REG (x)))
	return cached_num_sign_bit_copies (SUBREG_REG (x), mode,
					   known_x, known_mode, known_ret);
#endif
      break;

    case SIGN_EXTRACT:
      if (CONST_INT_P (XEXP (x, 1)))
	return MAX (1, (int) bitwidth - INTVAL (XEXP (x, 1)));
      break;

    case SIGN_EXTEND:
      return (bitwidth - GET_MODE_PRECISION (GET_MODE (XEXP (x, 0)))
	      + cached_num_sign_bit_copies (XEXP (x, 0), VOIDmode,
					    known_x, known_mode, known_ret));

    case TRUNCATE:
      /* For a smaller object, just ignore the high bits.  */
      num0 = cached_num_sign_bit_copies (XEXP (x, 0), VOIDmode,
					 known_x, known_mode, known_ret);
      return MAX (1, (num0 - (int) (GET_MODE_PRECISION (GET_MODE (XEXP (x, 0)))
				    - bitwidth)));

    case NOT:
      return cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);

    case ROTATE:       case ROTATERT:
      /* If we are rotating left by a number of bits less than the number
	 of sign bit copies, we can just subtract that amount from the
	 number.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) < (int) bitwidth)
	{
	  num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					     known_x, known_mode, known_ret);
	  return MAX (1, num0 - (code == ROTATE ? INTVAL (XEXP (x, 1))
				 : (int) bitwidth - INTVAL (XEXP (x, 1))));
	}
      break;

    case NEG:
      /* In general, this subtracts one sign bit copy.  But if the value
	 is known to be positive, the number of sign bit copies is the
	 same as that of the input.  Finally, if the input has just one bit
	 that might be nonzero, all the bits are copies of the sign bit.  */
      num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);
      if (bitwidth > HOST_BITS_PER_WIDE_INT)
	return num0 > 1 ? num0 - 1 : 1;

      nonzero = nonzero_bits (XEXP (x, 0), mode);
      if (nonzero == 1)
	return bitwidth;

      if (num0 > 1
	  && (((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1)) & nonzero))
	num0--;

      return num0;

    case IOR:   case AND:   case XOR:
    case SMIN:  case SMAX:  case UMIN:  case UMAX:
      /* Logical operations will preserve the number of sign-bit copies.
	 MIN and MAX operations always return one of the operands.  */
      num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);
      num1 = cached_num_sign_bit_copies (XEXP (x, 1), mode,
					 known_x, known_mode, known_ret);

      /* If num1 is clearing some of the top bits then regardless of
	 the other term, we are guaranteed to have at least that many
	 high-order zero bits.  */
      if (code == AND
	  && num1 > 1
	  && bitwidth <= HOST_BITS_PER_WIDE_INT
	  && CONST_INT_P (XEXP (x, 1))
	  && (UINTVAL (XEXP (x, 1))
	      & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) == 0)
	return num1;

      /* Similarly for IOR when setting high-order bits.  */
      if (code == IOR
	  && num1 > 1
	  && bitwidth <= HOST_BITS_PER_WIDE_INT
	  && CONST_INT_P (XEXP (x, 1))
	  && (UINTVAL (XEXP (x, 1))
	      & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	return num1;

      return MIN (num0, num1);

    case PLUS:  case MINUS:
      /* For addition and subtraction, we can have a 1-bit carry.  However,
	 if we are subtracting 1 from a positive number, there will not
	 be such a carry.  Furthermore, if the positive number is known to
	 be 0 or 1, we know the result is either -1 or 0.  */

      if (code == PLUS && XEXP (x, 1) == constm1_rtx
	  && bitwidth <= HOST_BITS_PER_WIDE_INT)
	{
	  nonzero = nonzero_bits (XEXP (x, 0), mode);
	  if ((((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1)) & nonzero) == 0)
	    return (nonzero == 1 || nonzero == 0 ? bitwidth
		    : bitwidth - floor_log2 (nonzero) - 1);
	}

      num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);
      num1 = cached_num_sign_bit_copies (XEXP (x, 1), mode,
					 known_x, known_mode, known_ret);
      result = MAX (1, MIN (num0, num1) - 1);

      return result;

    case MULT:
      /* The number of bits of the product is the sum of the number of
	 bits of both terms.  However, unless one of the terms if known
	 to be positive, we must allow for an additional bit since negating
	 a negative number can remove one sign bit copy.  */

      num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);
      num1 = cached_num_sign_bit_copies (XEXP (x, 1), mode,
					 known_x, known_mode, known_ret);

      result = bitwidth - (bitwidth - num0) - (bitwidth - num1);
      if (result > 0
	  && (bitwidth > HOST_BITS_PER_WIDE_INT
	      || (((nonzero_bits (XEXP (x, 0), mode)
		    & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
		  && ((nonzero_bits (XEXP (x, 1), mode)
		       & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1)))
		      != 0))))
	result--;

      return MAX (1, result);

    case UDIV:
      /* The result must be <= the first operand.  If the first operand
	 has the high bit set, we know nothing about the number of sign
	 bit copies.  */
      if (bitwidth > HOST_BITS_PER_WIDE_INT)
	return 1;
      else if ((nonzero_bits (XEXP (x, 0), mode)
		& ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	return 1;
      else
	return cached_num_sign_bit_copies (XEXP (x, 0), mode,
					   known_x, known_mode, known_ret);

    case UMOD:
      /* The result must be <= the second operand.  If the second operand
	 has (or just might have) the high bit set, we know nothing about
	 the number of sign bit copies.  */
      if (bitwidth > HOST_BITS_PER_WIDE_INT)
	return 1;
      else if ((nonzero_bits (XEXP (x, 1), mode)
		& ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	return 1;
      else
	return cached_num_sign_bit_copies (XEXP (x, 1), mode,
					   known_x, known_mode, known_ret);

    case DIV:
      /* Similar to unsigned division, except that we have to worry about
	 the case where the divisor is negative, in which case we have
	 to add 1.  */
      result = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					   known_x, known_mode, known_ret);
      if (result > 1
	  && (bitwidth > HOST_BITS_PER_WIDE_INT
	      || (nonzero_bits (XEXP (x, 1), mode)
		  & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0))
	result--;

      return result;

    case MOD:
      result = cached_num_sign_bit_copies (XEXP (x, 1), mode,
					   known_x, known_mode, known_ret);
      if (result > 1
	  && (bitwidth > HOST_BITS_PER_WIDE_INT
	      || (nonzero_bits (XEXP (x, 1), mode)
		  & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0))
	result--;

      return result;

    case ASHIFTRT:
      /* Shifts by a constant add to the number of bits equal to the
	 sign bit.  */
      num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) > 0
	  && INTVAL (XEXP (x, 1)) < GET_MODE_PRECISION (GET_MODE (x)))
	num0 = MIN ((int) bitwidth, num0 + INTVAL (XEXP (x, 1)));

      return num0;

    case ASHIFT:
      /* Left shifts destroy copies.  */
      if (!CONST_INT_P (XEXP (x, 1))
	  || INTVAL (XEXP (x, 1)) < 0
	  || INTVAL (XEXP (x, 1)) >= (int) bitwidth
	  || INTVAL (XEXP (x, 1)) >= GET_MODE_PRECISION (GET_MODE (x)))
	return 1;

      num0 = cached_num_sign_bit_copies (XEXP (x, 0), mode,
					 known_x, known_mode, known_ret);
      return MAX (1, num0 - INTVAL (XEXP (x, 1)));

    case IF_THEN_ELSE:
      num0 = cached_num_sign_bit_copies (XEXP (x, 1), mode,
					 known_x, known_mode, known_ret);
      num1 = cached_num_sign_bit_copies (XEXP (x, 2), mode,
					 known_x, known_mode, known_ret);
      return MIN (num0, num1);

    case EQ:  case NE:  case GE:  case GT:  case LE:  case LT:
    case UNEQ:  case LTGT:  case UNGE:  case UNGT:  case UNLE:  case UNLT:
    case GEU: case GTU: case LEU: case LTU:
    case UNORDERED: case ORDERED:
      /* If the constant is negative, take its 1's complement and remask.
	 Then see how many zero bits we have.  */
      nonzero = STORE_FLAG_VALUE;
      if (bitwidth <= HOST_BITS_PER_WIDE_INT
	  && (nonzero & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	nonzero = (~nonzero) & GET_MODE_MASK (mode);

      return (nonzero == 0 ? bitwidth : bitwidth - floor_log2 (nonzero) - 1);

    default:
      break;
    }

  /* If we haven't been able to figure it out by one of the above rules,
     see if some of the high-order bits are known to be zero.  If so,
     count those bits and return one less than that amount.  If we can't
     safely compute the mask for this mode, always return BITWIDTH.  */

  bitwidth = GET_MODE_PRECISION (mode);
  if (bitwidth > HOST_BITS_PER_WIDE_INT)
    return 1;

  nonzero = nonzero_bits (x, mode);
  return nonzero & ((unsigned HOST_WIDE_INT) 1 << (bitwidth - 1))
	 ? 1 : bitwidth - floor_log2 (nonzero) - 1;
}

/* Calculate the rtx_cost of a single instruction.  A return value of
   zero indicates an instruction pattern without a known cost.  */

int
insn_rtx_cost (rtx pat, bool speed)
{
  int i, cost;
  rtx set;

  /* Extract the single set rtx from the instruction pattern.
     We can't use single_set since we only have the pattern.  */
  if (GET_CODE (pat) == SET)
    set = pat;
  else if (GET_CODE (pat) == PARALLEL)
    {
      set = NULL_RTX;
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx x = XVECEXP (pat, 0, i);
	  if (GET_CODE (x) == SET)
	    {
	      if (set)
		return 0;
	      set = x;
	    }
	}
      if (!set)
	return 0;
    }
  else
    return 0;

  cost = set_src_cost (SET_SRC (set), GET_MODE (SET_DEST (set)), speed);
  return cost > 0 ? cost : COSTS_N_INSNS (1);
}

/* Returns estimate on cost of computing SEQ.  */

unsigned
seq_cost (const rtx_insn *seq, bool speed)
{
  unsigned cost = 0;
  rtx set;

  for (; seq; seq = NEXT_INSN (seq))
    {
      set = single_set (seq);
      if (set)
        cost += set_rtx_cost (set, speed);
      else
        cost++;
    }

  return cost;
}

/* Given an insn INSN and condition COND, return the condition in a
   canonical form to simplify testing by callers.  Specifically:

   (1) The code will always be a comparison operation (EQ, NE, GT, etc.).
   (2) Both operands will be machine operands; (cc0) will have been replaced.
   (3) If an operand is a constant, it will be the second operand.
   (4) (LE x const) will be replaced with (LT x <const+1>) and similarly
       for GE, GEU, and LEU.

   If the condition cannot be understood, or is an inequality floating-point
   comparison which needs to be reversed, 0 will be returned.

   If REVERSE is nonzero, then reverse the condition prior to canonizing it.

   If EARLIEST is nonzero, it is a pointer to a place where the earliest
   insn used in locating the condition was found.  If a replacement test
   of the condition is desired, it should be placed in front of that
   insn and we will be sure that the inputs are still valid.

   If WANT_REG is nonzero, we wish the condition to be relative to that
   register, if possible.  Therefore, do not canonicalize the condition
   further.  If ALLOW_CC_MODE is nonzero, allow the condition returned
   to be a compare to a CC mode register.

   If VALID_AT_INSN_P, the condition must be valid at both *EARLIEST
   and at INSN.  */

rtx
canonicalize_condition (rtx_insn *insn, rtx cond, int reverse,
			rtx_insn **earliest,
			rtx want_reg, int allow_cc_mode, int valid_at_insn_p)
{
  enum rtx_code code;
  rtx_insn *prev = insn;
  const_rtx set;
  rtx tem;
  rtx op0, op1;
  int reverse_code = 0;
  machine_mode mode;
  basic_block bb = BLOCK_FOR_INSN (insn);

  code = GET_CODE (cond);
  mode = GET_MODE (cond);
  op0 = XEXP (cond, 0);
  op1 = XEXP (cond, 1);

  if (reverse)
    code = reversed_comparison_code (cond, insn);
  if (code == UNKNOWN)
    return 0;

  if (earliest)
    *earliest = insn;

  /* If we are comparing a register with zero, see if the register is set
     in the previous insn to a COMPARE or a comparison operation.  Perform
     the same tests as a function of STORE_FLAG_VALUE as find_comparison_args
     in cse.c  */

  while ((GET_RTX_CLASS (code) == RTX_COMPARE
	  || GET_RTX_CLASS (code) == RTX_COMM_COMPARE)
	 && op1 == CONST0_RTX (GET_MODE (op0))
	 && op0 != want_reg)
    {
      /* Set nonzero when we find something of interest.  */
      rtx x = 0;

      /* If comparison with cc0, import actual comparison from compare
	 insn.  */
      if (op0 == cc0_rtx)
	{
	  if ((prev = prev_nonnote_insn (prev)) == 0
	      || !NONJUMP_INSN_P (prev)
	      || (set = single_set (prev)) == 0
	      || SET_DEST (set) != cc0_rtx)
	    return 0;

	  op0 = SET_SRC (set);
	  op1 = CONST0_RTX (GET_MODE (op0));
	  if (earliest)
	    *earliest = prev;
	}

      /* If this is a COMPARE, pick up the two things being compared.  */
      if (GET_CODE (op0) == COMPARE)
	{
	  op1 = XEXP (op0, 1);
	  op0 = XEXP (op0, 0);
	  continue;
	}
      else if (!REG_P (op0))
	break;

      /* Go back to the previous insn.  Stop if it is not an INSN.  We also
	 stop if it isn't a single set or if it has a REG_INC note because
	 we don't want to bother dealing with it.  */

      prev = prev_nonnote_nondebug_insn (prev);

      if (prev == 0
	  || !NONJUMP_INSN_P (prev)
	  || FIND_REG_INC_NOTE (prev, NULL_RTX)
	  /* In cfglayout mode, there do not have to be labels at the
	     beginning of a block, or jumps at the end, so the previous
	     conditions would not stop us when we reach bb boundary.  */
	  || BLOCK_FOR_INSN (prev) != bb)
	break;

      set = set_of (op0, prev);

      if (set
	  && (GET_CODE (set) != SET
	      || !rtx_equal_p (SET_DEST (set), op0)))
	break;

      /* If this is setting OP0, get what it sets it to if it looks
	 relevant.  */
      if (set)
	{
	  machine_mode inner_mode = GET_MODE (SET_DEST (set));
#ifdef FLOAT_STORE_FLAG_VALUE
	  REAL_VALUE_TYPE fsfv;
#endif

	  /* ??? We may not combine comparisons done in a CCmode with
	     comparisons not done in a CCmode.  This is to aid targets
	     like Alpha that have an IEEE compliant EQ instruction, and
	     a non-IEEE compliant BEQ instruction.  The use of CCmode is
	     actually artificial, simply to prevent the combination, but
	     should not affect other platforms.

	     However, we must allow VOIDmode comparisons to match either
	     CCmode or non-CCmode comparison, because some ports have
	     modeless comparisons inside branch patterns.

	     ??? This mode check should perhaps look more like the mode check
	     in simplify_comparison in combine.  */
	  if (((GET_MODE_CLASS (mode) == MODE_CC)
	       != (GET_MODE_CLASS (inner_mode) == MODE_CC))
	      && mode != VOIDmode
	      && inner_mode != VOIDmode)
	    break;
	  if (GET_CODE (SET_SRC (set)) == COMPARE
	      || (((code == NE
		    || (code == LT
			&& val_signbit_known_set_p (inner_mode,
						    STORE_FLAG_VALUE))
#ifdef FLOAT_STORE_FLAG_VALUE
		    || (code == LT
			&& SCALAR_FLOAT_MODE_P (inner_mode)
			&& (fsfv = FLOAT_STORE_FLAG_VALUE (inner_mode),
			    REAL_VALUE_NEGATIVE (fsfv)))
#endif
		    ))
		  && COMPARISON_P (SET_SRC (set))))
	    x = SET_SRC (set);
	  else if (((code == EQ
		     || (code == GE
			 && val_signbit_known_set_p (inner_mode,
						     STORE_FLAG_VALUE))
#ifdef FLOAT_STORE_FLAG_VALUE
		     || (code == GE
			 && SCALAR_FLOAT_MODE_P (inner_mode)
			 && (fsfv = FLOAT_STORE_FLAG_VALUE (inner_mode),
			     REAL_VALUE_NEGATIVE (fsfv)))
#endif
		     ))
		   && COMPARISON_P (SET_SRC (set)))
	    {
	      reverse_code = 1;
	      x = SET_SRC (set);
	    }
	  else if ((code == EQ || code == NE)
		   && GET_CODE (SET_SRC (set)) == XOR)
	    /* Handle sequences like:

	       (set op0 (xor X Y))
	       ...(eq|ne op0 (const_int 0))...

	       in which case:

	       (eq op0 (const_int 0)) reduces to (eq X Y)
	       (ne op0 (const_int 0)) reduces to (ne X Y)

	       This is the form used by MIPS16, for example.  */
	    x = SET_SRC (set);
	  else
	    break;
	}

      else if (reg_set_p (op0, prev))
	/* If this sets OP0, but not directly, we have to give up.  */
	break;

      if (x)
	{
	  /* If the caller is expecting the condition to be valid at INSN,
	     make sure X doesn't change before INSN.  */
	  if (valid_at_insn_p)
	    if (modified_in_p (x, prev) || modified_between_p (x, prev, insn))
	      break;
	  if (COMPARISON_P (x))
	    code = GET_CODE (x);
	  if (reverse_code)
	    {
	      code = reversed_comparison_code (x, prev);
	      if (code == UNKNOWN)
		return 0;
	      reverse_code = 0;
	    }

	  op0 = XEXP (x, 0), op1 = XEXP (x, 1);
	  if (earliest)
	    *earliest = prev;
	}
    }

  /* If constant is first, put it last.  */
  if (CONSTANT_P (op0))
    code = swap_condition (code), tem = op0, op0 = op1, op1 = tem;

  /* If OP0 is the result of a comparison, we weren't able to find what
     was really being compared, so fail.  */
  if (!allow_cc_mode
      && GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC)
    return 0;

  /* Canonicalize any ordered comparison with integers involving equality
     if we can do computations in the relevant mode and we do not
     overflow.  */

  if (GET_MODE_CLASS (GET_MODE (op0)) != MODE_CC
      && CONST_INT_P (op1)
      && GET_MODE (op0) != VOIDmode
      && GET_MODE_PRECISION (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT const_val = INTVAL (op1);
      unsigned HOST_WIDE_INT uconst_val = const_val;
      unsigned HOST_WIDE_INT max_val
	= (unsigned HOST_WIDE_INT) GET_MODE_MASK (GET_MODE (op0));

      switch (code)
	{
	case LE:
	  if ((unsigned HOST_WIDE_INT) const_val != max_val >> 1)
	    code = LT, op1 = gen_int_mode (const_val + 1, GET_MODE (op0));
	  break;

	/* When cross-compiling, const_val might be sign-extended from
	   BITS_PER_WORD to HOST_BITS_PER_WIDE_INT */
	case GE:
	  if ((const_val & max_val)
	      != ((unsigned HOST_WIDE_INT) 1
		  << (GET_MODE_PRECISION (GET_MODE (op0)) - 1)))
	    code = GT, op1 = gen_int_mode (const_val - 1, GET_MODE (op0));
	  break;

	case LEU:
	  if (uconst_val < max_val)
	    code = LTU, op1 = gen_int_mode (uconst_val + 1, GET_MODE (op0));
	  break;

	case GEU:
	  if (uconst_val != 0)
	    code = GTU, op1 = gen_int_mode (uconst_val - 1, GET_MODE (op0));
	  break;

	default:
	  break;
	}
    }

  /* Never return CC0; return zero instead.  */
  if (CC0_P (op0))
    return 0;

  return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
}

/* Given a jump insn JUMP, return the condition that will cause it to branch
   to its JUMP_LABEL.  If the condition cannot be understood, or is an
   inequality floating-point comparison which needs to be reversed, 0 will
   be returned.

   If EARLIEST is nonzero, it is a pointer to a place where the earliest
   insn used in locating the condition was found.  If a replacement test
   of the condition is desired, it should be placed in front of that
   insn and we will be sure that the inputs are still valid.  If EARLIEST
   is null, the returned condition will be valid at INSN.

   If ALLOW_CC_MODE is nonzero, allow the condition returned to be a
   compare CC mode register.

   VALID_AT_INSN_P is the same as for canonicalize_condition.  */

rtx
get_condition (rtx_insn *jump, rtx_insn **earliest, int allow_cc_mode,
	       int valid_at_insn_p)
{
  rtx cond;
  int reverse;
  rtx set;

  /* If this is not a standard conditional jump, we can't parse it.  */
  if (!JUMP_P (jump)
      || ! any_condjump_p (jump))
    return 0;
  set = pc_set (jump);

  cond = XEXP (SET_SRC (set), 0);

  /* If this branches to JUMP_LABEL when the condition is false, reverse
     the condition.  */
  reverse
    = GET_CODE (XEXP (SET_SRC (set), 2)) == LABEL_REF
      && LABEL_REF_LABEL (XEXP (SET_SRC (set), 2)) == JUMP_LABEL (jump);

  return canonicalize_condition (jump, cond, reverse, earliest, NULL_RTX,
				 allow_cc_mode, valid_at_insn_p);
}

/* Initialize the table NUM_SIGN_BIT_COPIES_IN_REP based on
   TARGET_MODE_REP_EXTENDED.

   Note that we assume that the property of
   TARGET_MODE_REP_EXTENDED(B, C) is sticky to the integral modes
   narrower than mode B.  I.e., if A is a mode narrower than B then in
   order to be able to operate on it in mode B, mode A needs to
   satisfy the requirements set by the representation of mode B.  */

static void
init_num_sign_bit_copies_in_rep (void)
{
  machine_mode mode, in_mode;

  for (in_mode = GET_CLASS_NARROWEST_MODE (MODE_INT); in_mode != VOIDmode;
       in_mode = GET_MODE_WIDER_MODE (mode))
    for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != in_mode;
	 mode = GET_MODE_WIDER_MODE (mode))
      {
	machine_mode i;

	/* Currently, it is assumed that TARGET_MODE_REP_EXTENDED
	   extends to the next widest mode.  */
	gcc_assert (targetm.mode_rep_extended (mode, in_mode) == UNKNOWN
		    || GET_MODE_WIDER_MODE (mode) == in_mode);

	/* We are in in_mode.  Count how many bits outside of mode
	   have to be copies of the sign-bit.  */
	for (i = mode; i != in_mode; i = GET_MODE_WIDER_MODE (i))
	  {
	    machine_mode wider = GET_MODE_WIDER_MODE (i);

	    if (targetm.mode_rep_extended (i, wider) == SIGN_EXTEND
		/* We can only check sign-bit copies starting from the
		   top-bit.  In order to be able to check the bits we
		   have already seen we pretend that subsequent bits
		   have to be sign-bit copies too.  */
		|| num_sign_bit_copies_in_rep [in_mode][mode])
	      num_sign_bit_copies_in_rep [in_mode][mode]
		+= GET_MODE_PRECISION (wider) - GET_MODE_PRECISION (i);
	  }
      }
}

/* Suppose that truncation from the machine mode of X to MODE is not a
   no-op.  See if there is anything special about X so that we can
   assume it already contains a truncated value of MODE.  */

bool
truncated_to_mode (machine_mode mode, const_rtx x)
{
  /* This register has already been used in MODE without explicit
     truncation.  */
  if (REG_P (x) && rtl_hooks.reg_truncated_to_mode (mode, x))
    return true;

  /* See if we already satisfy the requirements of MODE.  If yes we
     can just switch to MODE.  */
  if (num_sign_bit_copies_in_rep[GET_MODE (x)][mode]
      && (num_sign_bit_copies (x, GET_MODE (x))
	  >= num_sign_bit_copies_in_rep[GET_MODE (x)][mode] + 1))
    return true;

  return false;
}

/* Return true if RTX code CODE has a single sequence of zero or more
   "e" operands and no rtvec operands.  Initialize its rtx_all_subrtx_bounds
   entry in that case.  */

static bool
setup_reg_subrtx_bounds (unsigned int code)
{
  const char *format = GET_RTX_FORMAT ((enum rtx_code) code);
  unsigned int i = 0;
  for (; format[i] != 'e'; ++i)
    {
      if (!format[i])
	/* No subrtxes.  Leave start and count as 0.  */
	return true;
      if (format[i] == 'E' || format[i] == 'V')
	return false;
    }

  /* Record the sequence of 'e's.  */
  rtx_all_subrtx_bounds[code].start = i;
  do
    ++i;
  while (format[i] == 'e');
  rtx_all_subrtx_bounds[code].count = i - rtx_all_subrtx_bounds[code].start;
  /* rtl-iter.h relies on this.  */
  gcc_checking_assert (rtx_all_subrtx_bounds[code].count <= 3);

  for (; format[i]; ++i)
    if (format[i] == 'E' || format[i] == 'V' || format[i] == 'e')
      return false;

  return true;
}

/* Initialize rtx_all_subrtx_bounds.  */
void
init_rtlanal (void)
{
  int i;
  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      if (!setup_reg_subrtx_bounds (i))
	rtx_all_subrtx_bounds[i].count = UCHAR_MAX;
      if (GET_RTX_CLASS (i) != RTX_CONST_OBJ)
	rtx_nonconst_subrtx_bounds[i] = rtx_all_subrtx_bounds[i];
    }

  init_num_sign_bit_copies_in_rep ();
}

/* Check whether this is a constant pool constant.  */
bool
constant_pool_constant_p (rtx x)
{
  x = avoid_constant_pool_reference (x);
  return CONST_DOUBLE_P (x);
}

/* If M is a bitmask that selects a field of low-order bits within an item but
   not the entire word, return the length of the field.  Return -1 otherwise.
   M is used in machine mode MODE.  */

int
low_bitmask_len (machine_mode mode, unsigned HOST_WIDE_INT m)
{
  if (mode != VOIDmode)
    {
      if (GET_MODE_PRECISION (mode) > HOST_BITS_PER_WIDE_INT)
	return -1;
      m &= GET_MODE_MASK (mode);
    }

  return exact_log2 (m + 1);
}

/* Return the mode of MEM's address.  */

machine_mode
get_address_mode (rtx mem)
{
  machine_mode mode;

  gcc_assert (MEM_P (mem));
  mode = GET_MODE (XEXP (mem, 0));
  if (mode != VOIDmode)
    return mode;
  return targetm.addr_space.address_mode (MEM_ADDR_SPACE (mem));
}

/* Split up a CONST_DOUBLE or integer constant rtx
   into two rtx's for single words,
   storing in *FIRST the word that comes first in memory in the target
   and in *SECOND the other.

   TODO: This function needs to be rewritten to work on any size
   integer.  */

void
split_double (rtx value, rtx *first, rtx *second)
{
  if (CONST_INT_P (value))
    {
      if (HOST_BITS_PER_WIDE_INT >= (2 * BITS_PER_WORD))
	{
	  /* In this case the CONST_INT holds both target words.
	     Extract the bits from it into two word-sized pieces.
	     Sign extend each half to HOST_WIDE_INT.  */
	  unsigned HOST_WIDE_INT low, high;
	  unsigned HOST_WIDE_INT mask, sign_bit, sign_extend;
	  unsigned bits_per_word = BITS_PER_WORD;

	  /* Set sign_bit to the most significant bit of a word.  */
	  sign_bit = 1;
	  sign_bit <<= bits_per_word - 1;

	  /* Set mask so that all bits of the word are set.  We could
	     have used 1 << BITS_PER_WORD instead of basing the
	     calculation on sign_bit.  However, on machines where
	     HOST_BITS_PER_WIDE_INT == BITS_PER_WORD, it could cause a
	     compiler warning, even though the code would never be
	     executed.  */
	  mask = sign_bit << 1;
	  mask--;

	  /* Set sign_extend as any remaining bits.  */
	  sign_extend = ~mask;

	  /* Pick the lower word and sign-extend it.  */
	  low = INTVAL (value);
	  low &= mask;
	  if (low & sign_bit)
	    low |= sign_extend;

	  /* Pick the higher word, shifted to the least significant
	     bits, and sign-extend it.  */
	  high = INTVAL (value);
	  high >>= bits_per_word - 1;
	  high >>= 1;
	  high &= mask;
	  if (high & sign_bit)
	    high |= sign_extend;

	  /* Store the words in the target machine order.  */
	  if (WORDS_BIG_ENDIAN)
	    {
	      *first = GEN_INT (high);
	      *second = GEN_INT (low);
	    }
	  else
	    {
	      *first = GEN_INT (low);
	      *second = GEN_INT (high);
	    }
	}
      else
	{
	  /* The rule for using CONST_INT for a wider mode
	     is that we regard the value as signed.
	     So sign-extend it.  */
	  rtx high = (INTVAL (value) < 0 ? constm1_rtx : const0_rtx);
	  if (WORDS_BIG_ENDIAN)
	    {
	      *first = high;
	      *second = value;
	    }
	  else
	    {
	      *first = value;
	      *second = high;
	    }
	}
    }
  else if (GET_CODE (value) == CONST_WIDE_INT)
    {
      /* All of this is scary code and needs to be converted to
	 properly work with any size integer.  */
      gcc_assert (CONST_WIDE_INT_NUNITS (value) == 2);
      if (WORDS_BIG_ENDIAN)
	{
	  *first = GEN_INT (CONST_WIDE_INT_ELT (value, 1));
	  *second = GEN_INT (CONST_WIDE_INT_ELT (value, 0));
	}
      else
	{
	  *first = GEN_INT (CONST_WIDE_INT_ELT (value, 0));
	  *second = GEN_INT (CONST_WIDE_INT_ELT (value, 1));
	}
    }
  else if (!CONST_DOUBLE_P (value))
    {
      if (WORDS_BIG_ENDIAN)
	{
	  *first = const0_rtx;
	  *second = value;
	}
      else
	{
	  *first = value;
	  *second = const0_rtx;
	}
    }
  else if (GET_MODE (value) == VOIDmode
	   /* This is the old way we did CONST_DOUBLE integers.  */
	   || GET_MODE_CLASS (GET_MODE (value)) == MODE_INT)
    {
      /* In an integer, the words are defined as most and least significant.
	 So order them by the target's convention.  */
      if (WORDS_BIG_ENDIAN)
	{
	  *first = GEN_INT (CONST_DOUBLE_HIGH (value));
	  *second = GEN_INT (CONST_DOUBLE_LOW (value));
	}
      else
	{
	  *first = GEN_INT (CONST_DOUBLE_LOW (value));
	  *second = GEN_INT (CONST_DOUBLE_HIGH (value));
	}
    }
  else
    {
      long l[2];

      /* Note, this converts the REAL_VALUE_TYPE to the target's
	 format, splits up the floating point double and outputs
	 exactly 32 bits of it into each of l[0] and l[1] --
	 not necessarily BITS_PER_WORD bits.  */
      REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (value), l);

      /* If 32 bits is an entire word for the target, but not for the host,
	 then sign-extend on the host so that the number will look the same
	 way on the host that it would on the target.  See for instance
	 simplify_unary_operation.  The #if is needed to avoid compiler
	 warnings.  */

#if HOST_BITS_PER_LONG > 32
      if (BITS_PER_WORD < HOST_BITS_PER_LONG && BITS_PER_WORD == 32)
	{
	  if (l[0] & ((long) 1 << 31))
	    l[0] |= ((unsigned long) (-1) << 32);
	  if (l[1] & ((long) 1 << 31))
	    l[1] |= ((unsigned long) (-1) << 32);
	}
#endif

      *first = GEN_INT (l[0]);
      *second = GEN_INT (l[1]);
    }
}

/* Return true if X is a sign_extract or zero_extract from the least
   significant bit.  */

static bool
lsb_bitfield_op_p (rtx x)
{
  if (GET_RTX_CLASS (GET_CODE (x)) == RTX_BITFIELD_OPS)
    {
      machine_mode mode = GET_MODE (XEXP (x, 0));
      HOST_WIDE_INT len = INTVAL (XEXP (x, 1));
      HOST_WIDE_INT pos = INTVAL (XEXP (x, 2));

      return (pos == (BITS_BIG_ENDIAN ? GET_MODE_PRECISION (mode) - len : 0));
    }
  return false;
}

/* Strip outer address "mutations" from LOC and return a pointer to the
   inner value.  If OUTER_CODE is nonnull, store the code of the innermost
   stripped expression there.

   "Mutations" either convert between modes or apply some kind of
   extension, truncation or alignment.  */

rtx *
strip_address_mutations (rtx *loc, enum rtx_code *outer_code)
{
  for (;;)
    {
      enum rtx_code code = GET_CODE (*loc);
      if (GET_RTX_CLASS (code) == RTX_UNARY)
	/* Things like SIGN_EXTEND, ZERO_EXTEND and TRUNCATE can be
	   used to convert between pointer sizes.  */
	loc = &XEXP (*loc, 0);
      else if (lsb_bitfield_op_p (*loc))
	/* A [SIGN|ZERO]_EXTRACT from the least significant bit effectively
	   acts as a combined truncation and extension.  */
	loc = &XEXP (*loc, 0);
      else if (code == AND && CONST_INT_P (XEXP (*loc, 1)))
	/* (and ... (const_int -X)) is used to align to X bytes.  */
	loc = &XEXP (*loc, 0);
      else if (code == SUBREG
               && !OBJECT_P (SUBREG_REG (*loc))
               && subreg_lowpart_p (*loc))
	/* (subreg (operator ...) ...) inside and is used for mode
	   conversion too.  */
	loc = &SUBREG_REG (*loc);
      else
	return loc;
      if (outer_code)
	*outer_code = code;
    }
}

/* Return true if CODE applies some kind of scale.  The scaled value is
   is the first operand and the scale is the second.  */

static bool
binary_scale_code_p (enum rtx_code code)
{
  return (code == MULT
          || code == ASHIFT
          /* Needed by ARM targets.  */
          || code == ASHIFTRT
          || code == LSHIFTRT
          || code == ROTATE
          || code == ROTATERT);
}

/* If *INNER can be interpreted as a base, return a pointer to the inner term
   (see address_info).  Return null otherwise.  */

static rtx *
get_base_term (rtx *inner)
{
  if (GET_CODE (*inner) == LO_SUM)
    inner = strip_address_mutations (&XEXP (*inner, 0));
  if (REG_P (*inner)
      || MEM_P (*inner)
      || GET_CODE (*inner) == SUBREG
      || GET_CODE (*inner) == SCRATCH)
    return inner;
  return 0;
}

/* If *INNER can be interpreted as an index, return a pointer to the inner term
   (see address_info).  Return null otherwise.  */

static rtx *
get_index_term (rtx *inner)
{
  /* At present, only constant scales are allowed.  */
  if (binary_scale_code_p (GET_CODE (*inner)) && CONSTANT_P (XEXP (*inner, 1)))
    inner = strip_address_mutations (&XEXP (*inner, 0));
  if (REG_P (*inner)
      || MEM_P (*inner)
      || GET_CODE (*inner) == SUBREG
      || GET_CODE (*inner) == SCRATCH)
    return inner;
  return 0;
}

/* Set the segment part of address INFO to LOC, given that INNER is the
   unmutated value.  */

static void
set_address_segment (struct address_info *info, rtx *loc, rtx *inner)
{
  gcc_assert (!info->segment);
  info->segment = loc;
  info->segment_term = inner;
}

/* Set the base part of address INFO to LOC, given that INNER is the
   unmutated value.  */

static void
set_address_base (struct address_info *info, rtx *loc, rtx *inner)
{
  gcc_assert (!info->base);
  info->base = loc;
  info->base_term = inner;
}

/* Set the index part of address INFO to LOC, given that INNER is the
   unmutated value.  */

static void
set_address_index (struct address_info *info, rtx *loc, rtx *inner)
{
  gcc_assert (!info->index);
  info->index = loc;
  info->index_term = inner;
}

/* Set the displacement part of address INFO to LOC, given that INNER
   is the constant term.  */

static void
set_address_disp (struct address_info *info, rtx *loc, rtx *inner)
{
  gcc_assert (!info->disp);
  info->disp = loc;
  info->disp_term = inner;
}

/* INFO->INNER describes a {PRE,POST}_{INC,DEC} address.  Set up the
   rest of INFO accordingly.  */

static void
decompose_incdec_address (struct address_info *info)
{
  info->autoinc_p = true;

  rtx *base = &XEXP (*info->inner, 0);
  set_address_base (info, base, base);
  gcc_checking_assert (info->base == info->base_term);

  /* These addresses are only valid when the size of the addressed
     value is known.  */
  gcc_checking_assert (info->mode != VOIDmode);
}

/* INFO->INNER describes a {PRE,POST}_MODIFY address.  Set up the rest
   of INFO accordingly.  */

static void
decompose_automod_address (struct address_info *info)
{
  info->autoinc_p = true;

  rtx *base = &XEXP (*info->inner, 0);
  set_address_base (info, base, base);
  gcc_checking_assert (info->base == info->base_term);

  rtx plus = XEXP (*info->inner, 1);
  gcc_assert (GET_CODE (plus) == PLUS);

  info->base_term2 = &XEXP (plus, 0);
  gcc_checking_assert (rtx_equal_p (*info->base_term, *info->base_term2));

  rtx *step = &XEXP (plus, 1);
  rtx *inner_step = strip_address_mutations (step);
  if (CONSTANT_P (*inner_step))
    set_address_disp (info, step, inner_step);
  else
    set_address_index (info, step, inner_step);
}

/* Treat *LOC as a tree of PLUS operands and store pointers to the summed
   values in [PTR, END).  Return a pointer to the end of the used array.  */

static rtx **
extract_plus_operands (rtx *loc, rtx **ptr, rtx **end)
{
  rtx x = *loc;
  if (GET_CODE (x) == PLUS)
    {
      ptr = extract_plus_operands (&XEXP (x, 0), ptr, end);
      ptr = extract_plus_operands (&XEXP (x, 1), ptr, end);
    }
  else
    {
      gcc_assert (ptr != end);
      *ptr++ = loc;
    }
  return ptr;
}

/* Evaluate the likelihood of X being a base or index value, returning
   positive if it is likely to be a base, negative if it is likely to be
   an index, and 0 if we can't tell.  Make the magnitude of the return
   value reflect the amount of confidence we have in the answer.

   MODE, AS, OUTER_CODE and INDEX_CODE are as for ok_for_base_p_1.  */

static int
baseness (rtx x, machine_mode mode, addr_space_t as,
	  enum rtx_code outer_code, enum rtx_code index_code)
{
  /* Believe *_POINTER unless the address shape requires otherwise.  */
  if (REG_P (x) && REG_POINTER (x))
    return 2;
  if (MEM_P (x) && MEM_POINTER (x))
    return 2;

  if (REG_P (x) && HARD_REGISTER_P (x))
    {
      /* X is a hard register.  If it only fits one of the base
	 or index classes, choose that interpretation.  */
      int regno = REGNO (x);
      bool base_p = ok_for_base_p_1 (regno, mode, as, outer_code, index_code);
      bool index_p = REGNO_OK_FOR_INDEX_P (regno);
      if (base_p != index_p)
	return base_p ? 1 : -1;
    }
  return 0;
}

/* INFO->INNER describes a normal, non-automodified address.
   Fill in the rest of INFO accordingly.  */

static void
decompose_normal_address (struct address_info *info)
{
  /* Treat the address as the sum of up to four values.  */
  rtx *ops[4];
  size_t n_ops = extract_plus_operands (info->inner, ops,
					ops + ARRAY_SIZE (ops)) - ops;

  /* If there is more than one component, any base component is in a PLUS.  */
  if (n_ops > 1)
    info->base_outer_code = PLUS;

  /* Try to classify each sum operand now.  Leave those that could be
     either a base or an index in OPS.  */
  rtx *inner_ops[4];
  size_t out = 0;
  for (size_t in = 0; in < n_ops; ++in)
    {
      rtx *loc = ops[in];
      rtx *inner = strip_address_mutations (loc);
      if (CONSTANT_P (*inner))
	set_address_disp (info, loc, inner);
      else if (GET_CODE (*inner) == UNSPEC)
	set_address_segment (info, loc, inner);
      else
	{
	  /* The only other possibilities are a base or an index.  */
	  rtx *base_term = get_base_term (inner);
	  rtx *index_term = get_index_term (inner);
	  gcc_assert (base_term || index_term);
	  if (!base_term)
	    set_address_index (info, loc, index_term);
	  else if (!index_term)
	    set_address_base (info, loc, base_term);
	  else
	    {
	      gcc_assert (base_term == index_term);
	      ops[out] = loc;
	      inner_ops[out] = base_term;
	      ++out;
	    }
	}
    }

  /* Classify the remaining OPS members as bases and indexes.  */
  if (out == 1)
    {
      /* If we haven't seen a base or an index yet, assume that this is
	 the base.  If we were confident that another term was the base
	 or index, treat the remaining operand as the other kind.  */
      if (!info->base)
	set_address_base (info, ops[0], inner_ops[0]);
      else
	set_address_index (info, ops[0], inner_ops[0]);
    }
  else if (out == 2)
    {
      /* In the event of a tie, assume the base comes first.  */
      if (baseness (*inner_ops[0], info->mode, info->as, PLUS,
		    GET_CODE (*ops[1]))
	  >= baseness (*inner_ops[1], info->mode, info->as, PLUS,
		       GET_CODE (*ops[0])))
	{
	  set_address_base (info, ops[0], inner_ops[0]);
	  set_address_index (info, ops[1], inner_ops[1]);
	}
      else
	{
	  set_address_base (info, ops[1], inner_ops[1]);
	  set_address_index (info, ops[0], inner_ops[0]);
	}
    }
  else
    gcc_assert (out == 0);
}

/* Describe address *LOC in *INFO.  MODE is the mode of the addressed value,
   or VOIDmode if not known.  AS is the address space associated with LOC.
   OUTER_CODE is MEM if *LOC is a MEM address and ADDRESS otherwise.  */

void
decompose_address (struct address_info *info, rtx *loc, machine_mode mode,
		   addr_space_t as, enum rtx_code outer_code)
{
  memset (info, 0, sizeof (*info));
  info->mode = mode;
  info->as = as;
  info->addr_outer_code = outer_code;
  info->outer = loc;
  info->inner = strip_address_mutations (loc, &outer_code);
  info->base_outer_code = outer_code;
  switch (GET_CODE (*info->inner))
    {
    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
      decompose_incdec_address (info);
      break;

    case PRE_MODIFY:
    case POST_MODIFY:
      decompose_automod_address (info);
      break;

    default:
      decompose_normal_address (info);
      break;
    }
}

/* Describe address operand LOC in INFO.  */

void
decompose_lea_address (struct address_info *info, rtx *loc)
{
  decompose_address (info, loc, VOIDmode, ADDR_SPACE_GENERIC, ADDRESS);
}

/* Describe the address of MEM X in INFO.  */

void
decompose_mem_address (struct address_info *info, rtx x)
{
  gcc_assert (MEM_P (x));
  decompose_address (info, &XEXP (x, 0), GET_MODE (x),
		     MEM_ADDR_SPACE (x), MEM);
}

/* Update INFO after a change to the address it describes.  */

void
update_address (struct address_info *info)
{
  decompose_address (info, info->outer, info->mode, info->as,
		     info->addr_outer_code);
}

/* Return the scale applied to *INFO->INDEX_TERM, or 0 if the index is
   more complicated than that.  */

HOST_WIDE_INT
get_index_scale (const struct address_info *info)
{
  rtx index = *info->index;
  if (GET_CODE (index) == MULT
      && CONST_INT_P (XEXP (index, 1))
      && info->index_term == &XEXP (index, 0))
    return INTVAL (XEXP (index, 1));

  if (GET_CODE (index) == ASHIFT
      && CONST_INT_P (XEXP (index, 1))
      && info->index_term == &XEXP (index, 0))
    return (HOST_WIDE_INT) 1 << INTVAL (XEXP (index, 1));

  if (info->index == info->index_term)
    return 1;

  return 0;
}

/* Return the "index code" of INFO, in the form required by
   ok_for_base_p_1.  */

enum rtx_code
get_index_code (const struct address_info *info)
{
  if (info->index)
    return GET_CODE (*info->index);

  if (info->disp)
    return GET_CODE (*info->disp);

  return SCRATCH;
}

/* Return true if RTL X contains a SYMBOL_REF.  */

bool
contains_symbol_ref_p (const_rtx x)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (SYMBOL_REF_P (*iter))
      return true;

  return false;
}

/* Return true if RTL X contains a SYMBOL_REF or LABEL_REF.  */

bool
contains_symbolic_reference_p (const_rtx x)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (SYMBOL_REF_P (*iter) || GET_CODE (*iter) == LABEL_REF)
      return true;

  return false;
}

/* Return true if X contains a thread-local symbol.  */

bool
tls_referenced_p (const_rtx x)
{
  if (!targetm.have_tls)
    return false;

  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (GET_CODE (*iter) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (*iter) != 0)
      return true;
  return false;
}
